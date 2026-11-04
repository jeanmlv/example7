#!/usr/bin/env Rscript
# VEGA SDTM load-2388: endoscopy/UCEIS evidence and QC only.
# No clinical endpoint is approved or derived by this script.
options(stringsAsFactors = FALSE)
root <- '/domino/datasets/local/clinical-trial-data/CNTO1959UCO2002-VEGA-WK50/load-2388/Data/csv'
out <- '/mnt/CNTO1959UCO2002_VEGA_load2388_endoscopy_validation'
dir.create(out, recursive=TRUE, showWarnings=FALSE)
write_out <- function(x, filename) write.csv(x, file.path(out, filename), row.names=FALSE, na='')
load_domain <- function(name) {
  path <- file.path(root, paste0(tolower(name), '.sas7bdat.csv'))
  if (!file.exists(path)) stop('Missing source: ', path)
  x <- read.csv(path, check.names=FALSE, stringsAsFactors=FALSE, na.strings=c('', 'NA'))
  names(x) <- toupper(trimws(names(x)))
  x
}
mo <- load_domain('MO')
supp <- load_domain('SUPPMO')
required <- c('USUBJID','MOSEQ','MOTESTCD','MOEVAL','VISIT','VISITNUM','MOSTRESN','MOSTRESC')
if (!all(required %in% names(mo))) stop('MO missing: ', paste(setdiff(required,names(mo)),collapse=', '))
for (n in c('USUBJID','IDVAR','IDVARVAL','QNAM','QVAL')) if (!(n %in% names(supp))) stop('SUPPMO missing ',n)
mo$MOTESTCD <- toupper(trimws(as.character(mo$MOTESTCD)))
mo$MOEVAL <- toupper(trimws(as.character(mo$MOEVAL)))
mo$VISIT <- toupper(trimws(as.character(mo$VISIT)))
mo$VISITNUM <- suppressWarnings(as.numeric(mo$VISITNUM))
mo$MOSEQ <- suppressWarnings(as.numeric(mo$MOSEQ))
mo$MOSTRESN <- suppressWarnings(as.numeric(mo$MOSTRESN))
codes <- c('SCORE','MAYOSUBS','UCEISSC','BLEEDING','EROSION','VASCULAR','MCSSUBFN','MCSSUBSC','MCSSUBLO')
x <- mo[mo$MOTESTCD %in% codes, , drop=FALSE]
if (!nrow(x)) stop('No target codes found in MO')
# Reader-by-code counts, including missing results.
key <- paste(x$MOTESTCD, x$MOEVAL, sep='||')
groups <- split(seq_len(nrow(x)), key)
summary <- do.call(rbind, lapply(groups, function(idx) {
  z <- x[idx,,drop=FALSE]
  data.frame(MOTESTCD=z$MOTESTCD[1], MOEVAL=z$MOEVAL[1], N_RECORDS=nrow(z),
    N_NUMERIC=sum(!is.na(z$MOSTRESN)), N_SUBJECTS=length(unique(z$USUBJID)),
    N_PATIENT_VISITS=length(unique(paste(z$USUBJID,z$VISITNUM,z$VISIT,sep='|'))),
    MIN_RESULT=if(all(is.na(z$MOSTRESN))) NA_real_ else min(z$MOSTRESN,na.rm=TRUE),
    MAX_RESULT=if(all(is.na(z$MOSTRESN))) NA_real_ else max(z$MOSTRESN,na.rm=TRUE))
}))
write_out(summary,'01_reader_testcd_coverage.csv')
# All source rows for independent inspection; no arbitrary deduplication.
keep <- intersect(c('USUBJID','VISIT','VISITNUM','MOSEQ','MOTESTCD','MOTEST','MOEVAL','MOCAT','MOSCAT','MOSTRESN','MOSTRESC','MOORRES','MODTC','MOMETHOD','MONAM','MOGRPID','MOSPID'), names(x))
write_out(x[,keep,drop=FALSE],'02_endoscopy_source_records.csv')
# Candidate duplication at USUBJID + VISITNUM + TESTCD, with/without reader.
for (by_reader in c(FALSE,TRUE)) {
  k <- paste(x$USUBJID,x$VISITNUM,x$MOTESTCD, if(by_reader) x$MOEVAL else '',sep='|')
  tab <- table(k)
  dupe <- x[k %in% names(tab)[tab>1],keep,drop=FALSE]
  write_out(dupe, if(by_reader) '04_duplicates_within_reader.csv' else '03_multiple_readers_or_records.csv')
}
# Reader values at same patient/visit/code, without pretending one is the final read.
reader_values <- x[!is.na(x$MOSTRESN),intersect(c('USUBJID','VISIT','VISITNUM','MOTESTCD','MOEVAL','MOSEQ','MOSTRESN'),names(x)),drop=FALSE]
write_out(reader_values,'05_numeric_readers_long.csv')
# SUPPMO links by USUBJID + MOSEQ (IDVAR=MOSEQ). Preserve all qualifiers.
supp$IDVAR <- toupper(trimws(as.character(supp$IDVAR)))
supp$QNAM <- toupper(trimws(as.character(supp$QNAM)))
supp$IDVARVAL_NUM <- suppressWarnings(as.numeric(supp$IDVARVAL))
q <- supp[supp$IDVAR=='MOSEQ' & !is.na(supp$IDVARVAL_NUM),,drop=FALSE]
q <- q[q$USUBJID %in% unique(x$USUBJID),,drop=FALSE]
link <- merge(x[,c('USUBJID','MOSEQ','VISIT','VISITNUM','MOTESTCD','MOEVAL'),drop=FALSE],q,
  by.x=c('USUBJID','MOSEQ'),by.y=c('USUBJID','IDVARVAL_NUM'),all=FALSE)
qkeep <- intersect(c('USUBJID','MOSEQ','VISIT','VISITNUM','MOTESTCD','MOEVAL','QNAM','QLABEL','QVAL','QEVAL'),names(link))
write_out(link[,qkeep,drop=FALSE],'06_suppmo_qualifiers_linked.csv')
# Check UCEIS component sums for matching reader and visit. Only compare unique single observations.
u <- x[x$MOTESTCD %in% c('UCEISSC','BLEEDING','EROSION','VASCULAR') & !is.na(x$MOSTRESN),,drop=FALSE]
ukey <- paste(u$USUBJID,u$VISITNUM,u$VISIT,u$MOEVAL,sep='||')
ug <- split(seq_len(nrow(u)),ukey)
checks <- lapply(ug,function(idx){
  z <- u[idx,,drop=FALSE]
  cnt <- table(z$MOTESTCD)
  required <- c('UCEISSC','BLEEDING','EROSION','VASCULAR')
  unique_all <- all(required %in% names(cnt)) && all(cnt[required]==1)
  vals <- setNames(rep(NA_real_,length(required)),required)
  if(unique_all) for (nm in required) vals[nm] <- z$MOSTRESN[z$MOTESTCD==nm]
  total <- if(unique_all) sum(vals[c('BLEEDING','EROSION','VASCULAR')]) else NA_real_
  data.frame(USUBJID=z$USUBJID[1],VISIT=z$VISIT[1],VISITNUM=z$VISITNUM[1],MOEVAL=z$MOEVAL[1],
    N_ROWS=nrow(z), UNIQUE_FOUR=unique_all, UCEISSC=vals['UCEISSC'],BLEEDING=vals['BLEEDING'],
    EROSION=vals['EROSION'],VASCULAR=vals['VASCULAR'],COMPONENT_SUM=total,
    SUM_EQUALS_UCEIS=if(unique_all) total==vals['UCEISSC'] else NA)
})
write_out(do.call(rbind,checks),'07_uceis_component_consistency.csv')
# Mapping candidates remain unapproved pending reader/ICE/ADaM rules.
candidate <- data.frame(PARAMCD=c('ENLSCORE','ENCSCORE','ENSCORE','UCEISP','BLEEDP','EROSP','VASCP'),
  SDTM_DOMAIN='MO', SDTM_TESTCD=c('SCORE','MAYOSUBS','MAYOSUBS / SCORE','UCEISSC','BLEEDING','EROSION','VASCULAR'),
  SDTM_RESULT='MOSTRESN',APPROVED='NO',
  REVIEW_REQUIRED=c('Confirm local reader and source definition','Confirm central final reader/adjudication algorithm',
  'Confirm target endpoint and reader selection','Confirm primary ICE rule and reader selection',
  'Confirm primary ICE rule and reader selection','Confirm primary ICE rule and reader selection',
  'Confirm primary ICE rule and reader selection'))
write_out(candidate,'08_endoscopy_mapping_review_PENDING.csv')
cat('Validation complete. Output directory:',out,'\n')
cat('MO target rows:',nrow(x),' | linked SUPPMO qualifiers:',nrow(link),'\n')
cat('No mappings approved and no ARD values overwritten.\n')
