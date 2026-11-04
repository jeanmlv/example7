# example7


sdtm_dir <- paste0(
  "/domino/datasets/local/clinical-trial-data/",
  "CNTO1959UCO2002-VEGA-WK50/",
  "load-2388/Data/csv"
)

check_mapping <- function(domain, codes) {
  files <- list.files(
    sdtm_dir,
    pattern = "\\.csv$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  files <- files[
    grepl(
      paste0("^", domain, "\\.sas7bdat\\.csv$"),
      basename(files),
      ignore.case = TRUE
    )
  ]

  stopifnot(length(files) == 1)

  x <- read.csv(
    files[1],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  names(x) <- toupper(trimws(names(x)))

  code_col <- paste0(domain, "TESTCD")
  value_col <- paste0(domain, "STRESN")

  stopifnot(code_col %in% names(x))

  actual <- toupper(trimws(as.character(x[[code_col]])))

  cat("\nDOMAIN:", domain, "\n")
  cat("Rows:", nrow(x), "\n")
  cat("Columns:", paste(names(x), collapse = ", "), "\n")

  result <- data.frame(
    CODE = codes,
    N_MATCHES = vapply(
      codes,
      function(z) sum(actual == z, na.rm = TRUE),
      integer(1)
    )
  )

  print(result)

  for (code in codes) {
    idx <- which(actual == code)

    if (length(idx) > 0) {
      keep <- intersect(
        c("USUBJID", "VISIT", "VISITNUM",
          code_col, value_col),
        names(x)
      )

      cat("\nExample:", code, "\n")
      print(utils::head(x[idx, keep, drop = FALSE], 3))
    }
  }
}

check_mapping(
  "MI",
  c("ARCHCH", "CHRIINF", "NEUTLP", "NEUTEP",
    "CRYPTDES", "ULCER", "NANCYHI")
)

check_mapping("LB", "EOS")

sdtm_dir <- paste0(
  "/domino/datasets/local/clinical-trial-data/",
  "CNTO1959UCO2002-VEGA-WK50/",
  "load-2388/Data/csv"
)

check_mapping <- function(domain, codes) {
  files <- list.files(
    sdtm_dir,
    pattern = "\\.csv$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  files <- files[
    grepl(
      paste0("^", domain, "\\.sas7bdat\\.csv$"),
      basename(files),
      ignore.case = TRUE
    )
  ]

  stopifnot(length(files) == 1)

  x <- read.csv(
    files[1],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  names(x) <- toupper(trimws(names(x)))

  code_col <- paste0(domain, "TESTCD")
  value_col <- paste0(domain, "STRESN")

  stopifnot(code_col %in% names(x))

  actual <- toupper(trimws(as.character(x[[code_col]])))

  cat("\nDOMAIN:", domain, "\n")
  cat("Rows:", nrow(x), "\n")
  cat("Columns:", paste(names(x), collapse = ", "), "\n")

  result <- data.frame(
    CODE = codes,
    N_MATCHES = vapply(
      codes,
      function(z) sum(actual == z, na.rm = TRUE),
      integer(1)
    )
  )

  print(result)

  for (code in codes) {
    idx <- which(actual == code)

    if (length(idx) > 0) {
      keep <- intersect(
        c("USUBJID", "VISIT", "VISITNUM",
          code_col, value_col),
        names(x)
      )

      cat("\nExample:", code, "\n")
      print(utils::head(x[idx, keep, drop = FALSE], 3))
    }
  }
}

check_mapping(
  "MI",
  c("ARCHCH", "CHRIINF", "NEUTLP", "NEUTEP",
    "CRYPTDES", "ULCER", "NANCYHI")
)

check_mapping("LB", "EOS")
