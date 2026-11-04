#!/usr/bin/env Rscript
# VEGA load-2388 | Step 02: reviewed SDTM -> ARD (observed only)
# No semantic/automatic approval. No ICE/estimand derivation.
# Source: Data/csv (never mix with Data/cleaned).
# Run: source('02_build_uc_sdtm_ard_VEGA_load2388.R')

CONFIG <- list(
  study_id = 'CNTO1959UCO2002_VEGA_WK50',
  input_dir = '/domino/datasets/local/clinical-trial-data/CNTO1959UCO2002-VEGA-WK50/load-2388/Data/csv',
  mapping_dir = '/mnt/CNTO1959UCO2002_VEGA_load2388_mapping',
  output_dir = '/mnt/CNTO1959UCO2002_VEGA_load2388_ard',
  # Optional: reviewed CSV with PARAMCD,DOMAIN,CODE,APPROVED,VALUE_FIELD,
  # EVAL_FILTER,CAT_FILTER,SCAT_FILTER,DERIVATION_STATUS,REVIEWER_NOTES.
  approved_file = '/mnt/CNTO1959UCO2002_VEGA_load2388_mapping/07_mapping_review_template_VEGA_REVISED.csv',
  # Strict default: do not approve ICE/derived endpoints even if marked YES.
  allow_derived = FALSE
)

stopifnot(dir.exists(CONFIG$input_dir), dir.exists(CONFIG$mapping_dir))
if (!dir.exists('/mnt') || file.access('/mnt', 2) != 0) stop('/mnt is not writable')
dir.create(CONFIG$output_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(CONFIG$output_dir) || file.access(CONFIG$output_dir, 2) != 0) stop('Output directory not writable')

read_csv_base <- function(p) {
  read.csv(p, stringsAsFactors=FALSE, check.names=FALSE, colClasses='character', na.strings=c('', 'NA'), fileEncoding='UTF-8-BOM')
}
write_out <- function(x, filename) {
  write.csv(x, file.path(CONFIG$output_dir, filename), row.names=FALSE, na='')
  cat('Wrote:', filename, 'rows=', nrow(x), '\n')
}
clean <- function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ''; x }
normalize <- function(x) toupper(clean(x))
getcol <- function(x, nm) if (nm %in% names(x)) clean(x[[nm]]) else rep('', nrow(x))

# Prefer reviewed mapping supplied by user; never auto-approve best-candidate matches.
overview_path <- file.path(CONFIG$mapping_dir, '06_mapping_overview_best_candidate.csv')
if (!file.exists(overview_path)) stop('Missing step-01 overview: ', overview_path)
overview <- read_csv_base(overview_path)
if (!all(c('PARAMCD','DESCRIPTION') %in% names(overview))) stop('Invalid overview schema')
voi <- unique(overview[, c('PARAMCD','DESCRIPTION'), drop=FALSE]); voi$PARAMCD <- normalize(voi$PARAMCD)
if (nrow(voi) != 37L) warning('Expected 37 UC parameters, found ', nrow(voi))

review <- data.frame()
if (file.exists(CONFIG$approved_file)) review <- read_csv_base(CONFIG$approved_file)
if (!nrow(review)) {
  review <- data.frame(PARAMCD=character(),DOMAIN=character(),CODE=character(),APPROVED=character(),VALUE_FIELD=character(),EVAL_FILTER=character(),CAT_FILTER=character(),SCAT_FILTER=character(),DERIVATION_STATUS=character(),REVIEWER_NOTES=character())
}
# Convert step-01 template (DATASET, CODE) to normalized schema.
if (!'DOMAIN' %in% names(review) && 'DATASET' %in% names(review)) review$DOMAIN <- review$DATASET
for (nm in c('PARAMCD','DOMAIN','CODE','APPROVED','VALUE_FIELD','EVAL_FILTER','CAT_FILTER','SCAT_FILTER','DERIVATION_STATUS','REVIEWER_NOTES')) {
  if (!nm %in% names(review)) review[[nm]] <- rep('',nrow(review))
}
review$PARAMCD <- normalize(review$PARAMCD)
review$DOMAIN <- normalize(review$DOMAIN)
review$CODE <- normalize(review$CODE)
review$APPROVED <- normalize(review$APPROVED)
review$VALUE_FIELD <- normalize(review$VALUE_FIELD)
review$DERIVATION_STATUS <- normalize(review$DERIVATION_STATUS)
# Empty CSV cells are read as NA; normalize optional filters before nzchar().
for (nm in c('EVAL_FILTER','CAT_FILTER','SCAT_FILTER')) review[[nm]] <- clean(review[[nm]])

# Explicitly block derived / ICE / response / combined endpoints unless separate
# SAP-verified implementation is developed (this script does NOT implement it).
blocked <- c('MAYO','UCEISP','BLEEDP','EROSP','VASCP','CRESPP','HEHL','HIHL',
             'ENHLP','CREMP','PMRESPP','MOTEST','IBREMP','HEHLP','ENNRMP')
review$DECISION <- ifelse(review$APPROVED %in% c('YES','Y','TRUE'), 'APPROVED_REQUESTED', 'NOT_APPROVED')
review$DECISION[review$DECISION == 'APPROVED_REQUESTED' & review$PARAMCD %in% blocked] <- 'BLOCKED_DERIVATION_OR_ICE'
review$DECISION[review$DECISION == 'APPROVED_REQUESTED' & (!nzchar(review$DOMAIN) | !nzchar(review$CODE))] <- 'BLOCKED_MISSING_SOURCE'
review$DECISION[review$DECISION == 'APPROVED_REQUESTED' & !(review$PARAMCD %in% voi$PARAMCD)] <- 'BLOCKED_UNKNOWN_PARAMCD'
write_out(review, '01_mapping_decisions.csv')
accepted <- review[review$DECISION == 'APPROVED_REQUESTED', ,drop=FALSE]
if (nrow(accepted)) {
  keys <- paste(accepted$PARAMCD,accepted$DOMAIN,accepted$CODE,sep='|')
  if (anyDuplicated(keys)) stop('Duplicate approved mappings (PARAMCD+DOMAIN+CODE); resolve review file')
}

# File selection: only Data/csv; accept original domain.sas7bdat.csv.
files <- list.files(CONFIG$input_dir, pattern='\\.csv$', full.names=TRUE, recursive=TRUE, ignore.case=TRUE)
files <- files[!grepl('\\.clean\\.csv$', files, ignore.case=TRUE)]
domain_of <- function(p) toupper(sub('\\..*$', '', basename(p)))
domains <- vapply(files, domain_of, character(1))
file_index <- data.frame(DOMAIN=domains, PATH=files, stringsAsFactors=FALSE)
write_out(file_index, '02_input_files.csv')
if (anyDuplicated(domains)) stop('Multiple CSV files for one domain in selected source: ', paste(unique(domains[duplicated(domains)]),collapse=', '))
find_domain <- function(d) { p <- file_index$PATH[file_index$DOMAIN == d]; if(length(p)==1) p else NA_character_ }

# Master patient-visit grid: SV is primary, then only observed records when SV missing.
svpath <- find_domain('SV')
if (!is.na(svpath)) {
  sv <- read_csv_base(svpath)
  if (!all(c('USUBJID','VISIT') %in% names(sv))) stop('SV missing USUBJID/VISIT')
  grid <- data.frame(USUBJID=getcol(sv,'USUBJID'), AVISIT=getcol(sv,'VISIT'), AVISITN=getcol(sv,'VISITNUM'),stringsAsFactors=FALSE)
} else {
  warning('SV missing; constructing grid from approved source records only')
  grid <- data.frame(USUBJID=character(),AVISIT=character(),AVISITN=character())
}
grid <- unique(grid[nzchar(grid$USUBJID) & nzchar(grid$AVISIT),,drop=FALSE])
grid$KEY <- paste(grid$USUBJID, grid$AVISIT, grid$AVISITN, sep='\r')
if (anyDuplicated(grid$KEY)) stop('Duplicate SV keys')

trace <- list(); qc <- list(); coverage <- list()
for (i in seq_len(nrow(accepted))) {
  m <- accepted[i,,drop=FALSE]
  domain <- m$DOMAIN; code <- m$CODE; param <- m$PARAMCD
  path <- find_domain(domain)
  if (is.na(path)) { qc[[length(qc)+1L]] <- data.frame(PARAMCD=param,ISSUE='DOMAIN_FILE_MISSING',DETAIL=domain); next }
  dat <- read_csv_base(path)
  code_col <- paste0(domain,'TESTCD')
  if (!code_col %in% names(dat)) {
    qc[[length(qc)+1L]] <- data.frame(PARAMCD=param,ISSUE='TESTCD_COLUMN_MISSING',DETAIL=code_col); next
  }
  dat <- dat[normalize(dat[[code_col]]) == code,,drop=FALSE]
  if (nzchar(clean(m$EVAL_FILTER))) dat <- dat[normalize(getcol(dat,paste0(domain,'EVAL'))) == normalize(m$EVAL_FILTER),,drop=FALSE]
  if (nzchar(clean(m$CAT_FILTER))) dat <- dat[normalize(getcol(dat,paste0(domain,'CAT'))) == normalize(m$CAT_FILTER),,drop=FALSE]
  if (nzchar(clean(m$SCAT_FILTER))) dat <- dat[normalize(getcol(dat,paste0(domain,'SCAT'))) == normalize(m$SCAT_FILTER),,drop=FALSE]
  if (!nrow(dat)) { qc[[length(qc)+1L]] <- data.frame(PARAMCD=param,ISSUE='NO_MATCHING_RECORDS',DETAIL=paste(domain,code)); next }
  value_col <- m$VALUE_FIELD
  if (!nzchar(value_col)) value_col <- if(paste0(domain,'STRESN') %in% names(dat)) paste0(domain,'STRESN') else paste0(domain,'STRESC')
  if (!value_col %in% names(dat)) { qc[[length(qc)+1L]] <- data.frame(PARAMCD=param,ISSUE='VALUE_COLUMN_MISSING',DETAIL=value_col); next }
  seq_col <- paste0(domain,'SEQ')
  x <- data.frame(USUBJID=getcol(dat,'USUBJID'), AVISIT=getcol(dat,'VISIT'), AVISITN=getcol(dat,'VISITNUM'),
                  PARAMCD=param, DOMAIN=domain, TESTCD=code, VALUE=clean(dat[[value_col]]),
                  VALUE_FIELD=value_col, SRCSEQ=getcol(dat,seq_col), EVALUATOR=getcol(dat,paste0(domain,'EVAL')),
                  SOURCE_FILE=path, stringsAsFactors=FALSE)
  x <- x[nzchar(x$USUBJID) & nzchar(x$AVISIT) & nzchar(x$VALUE),,drop=FALSE]
  if (!nrow(x)) next
  trace[[length(trace)+1L]] <- x
  coverage[[length(coverage)+1L]] <- data.frame(PARAMCD=param,DOMAIN=domain,CODE=code,ROWS=nrow(x),PATIENTS=length(unique(x$USUBJID)),stringsAsFactors=FALSE)
}
long <- if(length(trace)) do.call(rbind,trace) else data.frame(USUBJID=character(),AVISIT=character(),AVISITN=character(),PARAMCD=character(),DOMAIN=character(),TESTCD=character(),VALUE=character(),VALUE_FIELD=character(),SRCSEQ=character(),EVALUATOR=character(),SOURCE_FILE=character())
write_out(long,'03_traceability_long.csv')
if(length(coverage)) write_out(do.call(rbind,coverage),'04_coverage.csv') else write_out(data.frame(PARAMCD=character(),ROWS=integer()),'04_coverage.csv')

# Every (patient, visit, PARAMCD) must have exactly one source record.
# Even identical repeated values are not silently selected: they require QC.
if(nrow(long)) {
  groupkey <- paste(long$USUBJID,long$AVISIT,long$AVISITN,long$PARAMCD,sep='\r')
  freq <- table(groupkey)
  duplicate_keys <- names(freq)[freq>1L]
  dup <- long[groupkey %in% duplicate_keys,,drop=FALSE]
  unique_long <- long[!(groupkey %in% duplicate_keys),,drop=FALSE]
} else {
  dup <- long; unique_long <- long
}
write_out(dup,'05_duplicate_conflicts.csv')
if(length(qc)) write_out(do.call(rbind,qc),'06_mapping_issues.csv') else write_out(data.frame(PARAMCD=character(),ISSUE=character(),DETAIL=character()),'06_mapping_issues.csv')

# Build ARD with all 37 requested columns. Missing/blocked stay blank.
# Naming convention: SDTM_<PARAMCD>_AVAL / _AVALC; do not label raw values as ADaM estimands.
for (p in voi$PARAMCD) {
  grid[[paste0('SDTM_',p,'_AVAL')]] <- rep('',nrow(grid))
  grid[[paste0('SDTM_',p,'_AVALC')]] <- rep('',nrow(grid))
}
if (nrow(unique_long)) {
  # Expand SV grid with any valid observed visits absent in SV (flagged in QC).
  uk <- paste(unique_long$USUBJID,unique_long$AVISIT,unique_long$AVISITN,sep='\r')
  missing <- !(uk %in% grid$KEY)
  if(any(missing)) {
    extra <- unique(unique_long[missing,c('USUBJID','AVISIT','AVISITN'),drop=FALSE]); extra$KEY <- paste(extra$USUBJID,extra$AVISIT,extra$AVISITN,sep='\r')
    add <- grid[rep(NA_integer_,nrow(extra)),,drop=FALSE]
    add$USUBJID <- extra$USUBJID; add$AVISIT <- extra$AVISIT; add$AVISITN <- extra$AVISITN; add$KEY <- extra$KEY
    for(j in setdiff(names(add),c('USUBJID','AVISIT','AVISITN','KEY'))) add[[j]] <- ''
    grid <- rbind(grid,add)
    write_out(extra,'07_visits_not_in_sv.csv')
  }
  idx <- match(uk,grid$KEY)
  for(j in seq_len(nrow(unique_long))) {
    p <- unique_long$PARAMCD[j]; value <- unique_long$VALUE[j]
    if (!p %in% voi$PARAMCD) next
    numeric_value <- suppressWarnings(as.numeric(value))
    dest <- if(is.finite(numeric_value)) paste0('SDTM_',p,'_AVAL') else paste0('SDTM_',p,'_AVALC')
    grid[[dest]][idx[j]] <- value
  }
}
grid$KEY <- NULL
if(nrow(grid)) grid <- grid[order(grid$USUBJID,suppressWarnings(as.numeric(grid$AVISITN)),grid$AVISIT),,drop=FALSE]
write_out(grid,'08_VEGA_load2388_SDTM_ARD_REVIEWED.csv')

# Gap report: no approval => no source data in ARD, not falsely called available.
status <- data.frame(PARAMCD=voi$PARAMCD, DESCRIPTION=voi$DESCRIPTION, APPROVED_SOURCE=FALSE, N_POPULATED=0L, stringsAsFactors=FALSE)
for (j in seq_len(nrow(status))) {
  p <- status$PARAMCD[j]
  status$APPROVED_SOURCE[j] <- p %in% accepted$PARAMCD
  status$N_POPULATED[j] <- sum(nzchar(grid[[paste0('SDTM_',p,'_AVAL')]]) | nzchar(grid[[paste0('SDTM_',p,'_AVALC')]]))
}
status$STATUS <- ifelse(status$N_POPULATED>0,'OBSERVED_SDTM_EXTRACTED',ifelse(status$APPROVED_SOURCE,'APPROVED_BUT_NO_UNAMBIGUOUS_VALUES','NOT_APPROVED'))
write_out(status,'09_37_variable_gap_summary.csv')
cat('\nFINISHED. Approved mappings:',nrow(accepted),'| ARD rows:',nrow(grid),'| nonempty mapped parameters:',sum(status$N_POPULATED>0),'\n')
cat('Note: source-derived clinical endpoints and ICE approaches are NOT implemented.\n')
