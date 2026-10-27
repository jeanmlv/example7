# ==============================================================================
# ADaM Multi-Dataset Integration Pipeline V6
# Granularity: USUBJID + AVISIT + AVISITN
# ==============================================================================

library(tidyverse)
library(haven)

# ==============================================================================
# CONFIG
# ==============================================================================

CONFIG <- list(
  input_dir  = "/domino/datasets/local/clinical-trial-data/CNTO1275CRD3005-STARDUST/load-1884/Data/csv",
  output_dir = "/mnt",
  output_dataset = "analysis-ready-dataset.csv",
  output_rds = "analysis-ready-dataset.rds",
  coverage_report = "coverage_report.csv",
  selection_qc = "record_selection_qc.csv",
  unresolved_conflicts = "unresolved_conflicts.csv"
)

KEYS <- c("USUBJID", "AVISIT", "AVISITN")

# Priority for OBSERVED clinical ARD, NOT a CDISC-mandated hierarchy.
# TF is treated as Treatment Failure for this project; verify per study metadata.
# Other DTYPE codes are unknown and retain lowest preference (99).
DTYPE_PRIORITY <- c(
  "" = 1L,
  "DERIVED" = 2L,
  "AVERAGE" = 3L,
  "LOCF" = 4L,
  "WOCF" = 5L,
  "BOCF" = 6L,
  "TF" = 7L
)

# In the absence of an observed value, allow the best available derived/imputed
# candidate. Such selections are flagged in QC, never labelled observed.


# ==============================================================================
# HELPERS
# ==============================================================================

log_msg <- function(...) {
  cat(sprintf("[%s] %s\n", Sys.time(), paste0(..., collapse = "")))
}

clean_dataset_name <- function(path) {
  tools::file_path_sans_ext(basename(path)) %>%
    toupper() %>%
    str_replace_all("[^A-Z0-9_]", "_")
}

read_any_dataset <- function(path) {
  
  ext <- tolower(tools::file_ext(path))
  
  if (ext == "sas7bdat") {
    
    df <- haven::read_sas(path)
    
  } else if (ext == "csv") {
    
    # USAR fread (mais robusto para Domino + arquivos grandes)
    df <- data.table::fread(path, sep = "auto", data.table = FALSE)
    
  } else {
    stop("Unsupported file type: ", path)
  }
  
  # Padronização CDISC
  df %>%
    rename_with(~ toupper(.x))
}

ensure_keys <- function(df) {
  
  # Padronizar nomes
  names(df) <- toupper(names(df))
  
  # Criar USUBJID a partir de SUBJID quando necessário
  if (!"USUBJID" %in% names(df)) {
    if ("SUBJID" %in% names(df)) {
      df <- df %>% mutate(USUBJID = as.character(SUBJID))
    } else {
      stop("Dataset does not contain USUBJID or SUBJID.")
    }
  }
  
  # Converter USUBJID/SUBJID para character
  df <- df %>%
    mutate(
      USUBJID = as.character(USUBJID),
      USUBJID = trimws(USUBJID)
    )
  
  # Se USUBJID estiver vazio, usar SUBJID como fallback
  if ("SUBJID" %in% names(df)) {
    df <- df %>%
      mutate(
        SUBJID = as.character(SUBJID),
        SUBJID = trimws(SUBJID),
        USUBJID = if_else(
          is.na(USUBJID) | USUBJID == "" | toupper(USUBJID) %in% c("NA", "NAN", "NULL"),
          SUBJID,
          USUBJID
        )
      )
  }
  
  # Criar AVISIT/AVISITN se não existirem
  if (!"AVISIT" %in% names(df)) {
    df <- df %>% mutate(AVISIT = "PSEUDO")
  }
  
  if (!"AVISITN" %in% names(df)) {
    df <- df %>% mutate(AVISITN = "PSEUDO")
  }
  
  # Padronizar chaves e substituir vazios por PSEUDO
  df %>%
    mutate(
      USUBJID = as.character(USUBJID),
      AVISIT  = as.character(AVISIT),
      AVISITN = as.character(AVISITN),
      
      AVISIT = if_else(
        is.na(AVISIT) | trimws(AVISIT) == "" | toupper(trimws(AVISIT)) %in% c("NA", "NAN", "NULL"),
        "PSEUDO",
        trimws(AVISIT)
      ),
      
      AVISITN = if_else(
        is.na(AVISITN) | trimws(AVISITN) == "" | toupper(trimws(AVISITN)) %in% c("NA", "NAN", "NULL"),
        "PSEUDO",
        trimws(AVISITN)
      )
    )
}

prefix_non_key_columns <- function(df, dataset_name) {
  non_key_cols <- setdiff(names(df), KEYS)
  
  df %>%
    rename_with(
      .fn = ~ paste0(dataset_name, "_", .x),
      .cols = all_of(non_key_cols)
    )
}

collapse_duplicate_keys <- function(df) {
  df %>%
    group_by(across(all_of(KEYS))) %>%
    summarise(
      across(
        everything(),
        ~ {
          values <- unique(na.omit(as.character(.x)))
          if (length(values) == 0) NA_character_
          else paste(values, collapse = " | ")
        }
      ),
      .groups = "drop"
    )
}

# Selection is applied to LONG ADaM rows before the pivot.
# Preserve ties with distinct values, but collapse identical values to one.
select_param_records <- function(df, dataset_name) {
  empty_qc <- tibble()
  if (nrow(df) == 0) return(list(data = df, qc = empty_qc, conflicts = empty_qc))

  df <- df %>%
    mutate(
      .SOURCE_ROW = row_number(),
      .DTYPE_CLEAN = if ("DTYPE" %in% names(.)) {
        toupper(trimws(coalesce(as.character(DTYPE), "")))
      } else rep("", n()),
      .DTYPE_RANK = as.integer(unname(DTYPE_PRIORITY[.DTYPE_CLEAN])),
      .DTYPE_RANK = replace_na(.DTYPE_RANK, 99L),
      .ASEQ_QC = if ("ASEQ" %in% names(.)) as.character(ASEQ) else rep(NA_character_, n()),
      .ANL01_QC = if ("ANL01FL" %in% names(.)) as.character(ANL01FL) else rep(NA_character_, n()),
      .ANL02_QC = if ("ANL02FL" %in% names(.)) as.character(ANL02FL) else rep(NA_character_, n())
    )

  group_cols <- c(KEYS, "PARAMCD", "VALUE_SOURCE")
  ranked <- df %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(
      .BEST_RANK = min(.DTYPE_RANK),
      .SELECTED = .DTYPE_RANK == .BEST_RANK,
      .N_DISTINCT_BEST = n_distinct(PARAM_VALUE[.SELECTED]),
      .DECISION = case_when(
        .SELECTED & .N_DISTINCT_BEST > 1L ~ "UNRESOLVED_TIE_CONCATENATED",
        .SELECTED & .DTYPE_RANK == 99L ~ "SELECTED_UNKNOWN_DTYPE",
        .SELECTED & .DTYPE_RANK > 1L ~ "SELECTED_NON_OBSERVED_FALLBACK",
        .SELECTED ~ "SELECTED_OBSERVED",
        TRUE ~ "NOT_SELECTED_LOWER_PRIORITY"
      )
    ) %>% ungroup()

  qc <- ranked %>%
    transmute(
      DATASET = dataset_name,
      across(all_of(KEYS)), PARAMCD, VALUE_SOURCE,
      SOURCE_ROW = .SOURCE_ROW, ASEQ = .ASEQ_QC,
      DTYPE = .DTYPE_CLEAN, DTYPE_RANK = .DTYPE_RANK,
      ANL01FL = .ANL01_QC, ANL02FL = .ANL02_QC,
      PARAM_VALUE, SELECTED = .SELECTED,
      DECISION = .DECISION
    )

  list(
    data = ranked %>% filter(.SELECTED),
    qc = qc,
    conflicts = qc %>% filter(DECISION == "UNRESOLVED_TIE_CONCATENATED")
  )
}

process_dataset <- function(path) {
  
  dataset_name <- clean_dataset_name(path)
  
  log_msg("Processing dataset: ", dataset_name)
  
  raw_df <- read_any_dataset(path)
  original_cols <- names(raw_df)
  original_n_cols <- length(original_cols)
  
  df <- raw_df %>%
    ensure_keys()
  
  # Número de sujeitos únicos
  n_unique_subjects <- df %>%
    filter(!is.na(USUBJID) & USUBJID != "") %>%
    summarise(n = n_distinct(USUBJID)) %>%
    pull(n)
  
  # Percentual geral de missing no dataset original
  missing_info <- tibble(
    total_cells = nrow(df) * ncol(df),
    missing_cells = sum(sapply(df, function(x) {
      x_chr <- as.character(x)
      sum(is.na(x_chr) | trimws(x_chr) == "")
    })),
    missing_percent = round((missing_cells / total_cells) * 100, 2)
  )
  
  missing_total_cells <- missing_info$total_cells
  missing_cells <- missing_info$missing_cells
  missing_percent <- missing_info$missing_percent
  
  has_paramcd <- "PARAMCD" %in% names(df)
  has_aval  <- "AVAL"  %in% names(df)
  has_avalc <- "AVALC" %in% names(df)
  
  paramcd_values <- character(0)
  selection_qc <- tibble()
  unresolved_conflicts <- tibble()
  
  has_any_value <- has_aval | has_avalc
  
  if (has_paramcd && has_any_value) {
    
    df_safe <- df %>%
      mutate(
        AVAL  = if ("AVAL" %in% names(.)) as.character(AVAL) else NA_character_,
        AVALC = if ("AVALC" %in% names(.)) as.character(AVALC) else NA_character_
      )
    
    paramcd_values <- df_safe %>%
      filter(!is.na(PARAMCD) & PARAMCD != "") %>%
      distinct(PARAMCD) %>%
      pull(PARAMCD) %>%
      as.character() %>%
      sort()
    
    # Build one candidate per original source row, retaining its own DTYPE.
    # Avoid coalescing blank AVAL with AVALC and avoid mixing their semantics.
    param_long <- df_safe %>%
      mutate(
        PARAMCD = trimws(as.character(PARAMCD)),
        AVAL = na_if(trimws(AVAL), ""),
        AVALC = na_if(trimws(AVALC), ""),
        PARAM_VALUE = coalesce(AVAL, AVALC),
        VALUE_SOURCE = case_when(
          !is.na(AVAL) ~ "AVAL",
          !is.na(AVALC) ~ "AVALC",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(PARAMCD), PARAMCD != "",
             !is.na(PARAM_VALUE), !is.na(VALUE_SOURCE))

    selection <- select_param_records(param_long, dataset_name)
    selection_qc <- selection$qc
    unresolved_conflicts <- selection$conflicts

    pivot_df <- selection$data %>%
      mutate(pivot_name = paste0(dataset_name, "_PARAMCD_", PARAMCD, "_", VALUE_SOURCE)) %>%
      select(all_of(KEYS), pivot_name, PARAM_VALUE, .SOURCE_ROW) %>%
      arrange(.SOURCE_ROW) %>%
      group_by(across(all_of(KEYS)), pivot_name) %>%
      summarise(
        PARAM_VALUE = {
          values <- unique(PARAM_VALUE[!is.na(PARAM_VALUE)])
          if (length(values) == 0) NA_character_
          else paste(values, collapse = " | ")
        },
        .groups = "drop"
      ) %>%
      pivot_wider(names_from = pivot_name, values_from = PARAM_VALUE)

    other_df <- df %>%
      select(-PARAMCD, -any_of(c("AVAL", "AVALC"))) %>%
      prefix_non_key_columns(dataset_name) %>%
      collapse_duplicate_keys()
    
    final_df <- full_join(other_df, pivot_df, by = KEYS)
    
  } else {
    
    # --------------------------------------------------------------------------
    # Datasets without PARAMCD: keep all variables with dataset prefix
    # --------------------------------------------------------------------------
    
    final_df <- df %>%
      prefix_non_key_columns(dataset_name) %>%
      collapse_duplicate_keys()
  }
  
  extracted_cols <- setdiff(names(final_df), KEYS)
  extracted_n_cols <- length(extracted_cols)
  
  has_aval  <- "AVAL"  %in% names(df)
  has_avalc <- "AVALC" %in% names(df)
  has_any_value <- has_aval | has_avalc
  
  coverage <- tibble(
    dataset = dataset_name,
    original_total_columns = original_n_cols,
    extracted_total_columns = extracted_n_cols,
    coverage_percent = round((extracted_n_cols / original_n_cols) * 100, 2),
    
    n_unique_subjects = n_unique_subjects,
    total_cells = missing_total_cells,
    missing_cells = missing_cells,
    missing_percent = missing_percent,
    
    has_paramcd = has_paramcd,
    has_aval  = has_aval,
    has_avalc = has_avalc,
    has_any_value = has_any_value,
    
    n_paramcd_extracted = length(paramcd_values),
    paramcd_extracted = paste(paramcd_values, collapse = ", "),
    
    is_100_percent_coverage = coverage_percent >= 100
  )
  
  list(
    data = final_df,
    coverage = coverage,
    selection_qc = selection_qc,
    unresolved_conflicts = unresolved_conflicts
  )
}

# ==============================================================================
# EXECUTION
# ==============================================================================

log_msg("Starting ADaM integration pipeline")

files <- list.files(
  CONFIG$input_dir,
  pattern = "\\.(sas7bdat|csv)$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(files) == 0) {
  stop("No SAS7BDAT or CSV files found in: ", CONFIG$input_dir)
}

log_msg("Files found: ", length(files))

processed <- map(files, safely(process_dataset))

successful <- processed %>%
  keep(~ is.null(.x$error)) %>%
  map("result")

failed <- processed %>%
  keep(~ !is.null(.x$error))

if (length(failed) > 0) {
  log_msg("WARNING: Some datasets failed to process:")
  walk(failed, ~ log_msg(.x$error$message))
}

if (length(successful) == 0) {
  stop("No datasets were successfully processed.")
}

final_dataset <- successful %>%
  map("data") %>%
  reduce(full_join, by = KEYS) %>%
  arrange(USUBJID, AVISITN, AVISIT)

coverage_report <- successful %>%
  map("coverage") %>%
  bind_rows()

record_selection_qc <- successful %>% map("selection_qc") %>% bind_rows()
unresolved_conflicts <- successful %>% map("unresolved_conflicts") %>% bind_rows()

# ==============================================================================
# OUTPUT
# ==============================================================================

dir.create(CONFIG$output_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(
  final_dataset,
  file.path(CONFIG$output_dir, CONFIG$output_dataset),
  na = ""
)

saveRDS(
  final_dataset,
  file.path(CONFIG$output_dir, CONFIG$output_rds)
)

write_csv(
  coverage_report,
  file.path(CONFIG$output_dir, CONFIG$coverage_report),
  na = ""
)

write_csv(record_selection_qc,
          file.path(CONFIG$output_dir, CONFIG$selection_qc), na = "")
write_csv(unresolved_conflicts,
          file.path(CONFIG$output_dir, CONFIG$unresolved_conflicts), na = "")

log_msg("Selection QC records: ", nrow(record_selection_qc))
log_msg("Unresolved conflict candidate records: ", nrow(unresolved_conflicts))
log_msg("Final dataset saved to: ", file.path(CONFIG$output_dir, CONFIG$output_dataset))
log_msg("Final RDS saved to: ", file.path(CONFIG$output_dir, CONFIG$output_rds))
log_msg("Coverage report saved to: ", file.path(CONFIG$output_dir, CONFIG$coverage_report))

log_msg("Final dataset dimensions: ", nrow(final_dataset), " rows x ", ncol(final_dataset), " columns")

print(coverage_report)
