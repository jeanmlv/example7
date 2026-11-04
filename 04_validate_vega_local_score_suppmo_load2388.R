#!/usr/bin/env Rscript
# VEGA load-2388: local SCORE validation, duplicate investigation, SUPPMO linkage.
# Read-only source audit. Does NOT modify ARD or approve mappings.
options(stringsAsFactors = FALSE)
root <- '/domino/datasets/local/clinical-trial-data/CNTO1959UCO2002-VEGA-WK50/load-2388/Data/csv'
out <- '/mnt/CNTO1959UCO2002_VEGA_load2388_local_score_validation'
dir.create(out, recursive=TRUE, showWarnings=FALSE)
read_domain <- function(domain) {
  fs <- list.files(root, pattern='\\.csv$', recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  fs <- fs[tolower(basename(fs)) == paste0(tolower(domain),'.sas7bdat.csv')]
  if(length(fs)!=1L) stop('Expected exactly one ',domain,' source file; found ',length(fs))
  x <- read.csv(fs, check.names=FALSE, colClasses='character', na.strings=c('','NA'), stringsAsFactors=FALSE)
  names(x) <- toupper(trimws(names(x)))
  x
}
write_report <- function(x,name) write.csv(x,file.path(out,name),row.names=FALSE,na='')
clean <- function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ''; x }
mo <- read_domain('mo'); supp <- read_domain('suppmo')
stopifnot(all(c('USUBJID','MOSEQ','MOSPID','MOTESTCD','MOEVAL','MOSTRESC','MOSTRESN','VISIT','VISITNUM','MODTC') %in% names(mo)))
stopifnot(all(c('USUBJID','IDVAR','IDVARVAL','QNAM','QVAL') %in% names(supp)))
for(n in c('USUBJID','MOSEQ','MOSPID','MOTESTCD','MOEVAL','MOSTRESC','MOSTRESN','VISIT','VISITNUM','MODTC')) mo[[n]] <- clean(mo[[n]])
for(n in c('USUBJID','IDVAR','IDVARVAL','QNAM','QVAL')) supp[[n]] <- clean(supp[[n]])
score <- mo[toupper(mo$MOTESTCD)=='SCORE' & toupper(mo$MOEVAL)=='LOCAL READER',,drop=FALSE]
score$SCORE_PARSED <- NA_real_
score$PARSE_STATUS <- 'MISSING_RESULT'
for(i in seq_len(nrow(score))) {
  v <- score$MOSTRESC[i]; n <- score$MOSTRESN[i]
  a <- regmatches(v, regexec('^\\s*([0-3])\\s*(?:=|$)',v,perl=TRUE))[[1]]
  if(length(a)>=2L) {score$SCORE_PARSED[i] <- as.numeric(a[2]);score$PARSE_STATUS[i] <- 'VALID_PREFIX_0_3'}
  else if(nchar(v)>0) score$PARSE_STATUS[i] <- 'UNRECOGNIZED_TEXT'
  if(nchar(n)>0 && !is.na(score$SCORE_PARSED[i]) && suppressWarnings(as.numeric(n))!=score$SCORE_PARSED[i]) score$PARSE_STATUS[i] <- 'CONFLICT_WITH_MOSTRESN'
}
write_report(score,'01_local_score_conversion_audit.csv')
# Duplicate local scores: group by subject and SDTM visit identity, never auto-select.
k <- paste(score$USUBJID,score$VISITNUM,score$VISIT,sep='\r')
ct <- table(k); score$N_AT_VISIT <- as.integer(ct[k])
dup <- score[score$N_AT_VISIT>1,,drop=FALSE]
write_report(dup,'02_local_score_duplicate_details.csv')
# Candidate only when one source row and conversion valid; no clinical approval implied.
candidate <- score[score$N_AT_VISIT==1 & score$PARSE_STATUS=='VALID_PREFIX_0_3',c('USUBJID','VISIT','VISITNUM','MOSEQ','MOSPID','MODTC','MOSTRESC','SCORE_PARSED'),drop=FALSE]
write_report(candidate,'03_local_score_unique_candidates_NOT_APPROVED.csv')
# Link SUPPMO by declared IDVAR, not always MOSEQ. A MOSPID may reference multiple MO rows.
supp$IDVAR <- toupper(supp$IDVAR)
supp$LINK_KEY <- paste(supp$USUBJID,supp$IDVAR,supp$IDVARVAL,sep='\r')
mo$ROW_ID <- seq_len(nrow(mo))
key_seq <- paste(mo$USUBJID,'MOSEQ',mo$MOSEQ,sep='\r')
key_spid <- paste(mo$USUBJID,'MOSPID',mo$MOSPID,sep='\r')
seq_ix <- split(mo$ROW_ID,key_seq); spid_ix <- split(mo$ROW_ID,key_spid)
link_rows <- vector('list',nrow(supp)); summary_rows <- vector('list',nrow(supp))
for(i in seq_len(nrow(supp))) {
  z <- supp[i,,drop=FALSE]; id <- z$IDVARVAL
  ix <- if(z$IDVAR=='MOSEQ') seq_ix[[z$LINK_KEY]] else if(z$IDVAR=='MOSPID') spid_ix[[z$LINK_KEY]] else NULL
  if(is.null(ix)) ix <- integer(0)
  ix <- ix[!is.na(ix)]
  summary_rows[[i]] <- data.frame(SUPP_ROW=i,USUBJID=z$USUBJID,IDVAR=z$IDVAR,IDVARVAL=id,QNAM=z$QNAM,QVAL=z$QVAL,N_MATCHING_MO_ROWS=length(ix),LINK_STATUS=if(length(ix)==0) 'UNMATCHED' else if(length(ix)==1) 'ONE_ROW' else 'MULTIPLE_MO_ROWS',stringsAsFactors=FALSE)
  if(length(ix)) {
    r <- mo[ix,c('USUBJID','MOSEQ','MOSPID','VISIT','VISITNUM','MOTESTCD','MOEVAL','MOCAT','MOSCAT','MODTC'),drop=FALSE]
    r$SUPP_ROW <- i;r$IDVAR <- z$IDVAR;r$IDVARVAL <- id;r$QNAM <- z$QNAM;r$QLABEL <- if('QLABEL' %in% names(z)) z$QLABEL else '';r$QVAL <- z$QVAL;r$QEVAL <- if('QEVAL' %in% names(z)) z$QEVAL else '';r$N_MATCHING_MO_ROWS <- length(ix)
    link_rows[[i]] <- r
  }
}
link_summary <- do.call(rbind,summary_rows)
linked <- if(length(Filter(Negate(is.null),link_rows))) do.call(rbind,Filter(Negate(is.null),link_rows)) else data.frame()
write_report(link_summary,'04_suppmo_link_status.csv')
write_report(linked,'05_suppmo_linked_all_rows.csv')
write_report(linked[linked$MOTESTCD %in% c('SCORE','MAYOSUBS','UCEISSC','BLEEDING','EROSION','VASCULAR'),,drop=FALSE],'06_suppmo_linked_endoscopy.csv')
# Additional checks: exact key uniqueness, supplemental qualifier counts, local score distribution.
key_dup <- data.frame(KEY=c('USUBJID+MOSEQ','USUBJID+MOSPID (nonempty)'),DUPLICATE_KEYS=c(sum(table(key_seq)>1),sum(table(key_spid[mo$MOSPID!=''])>1)))
write_report(key_dup,'07_mo_key_uniqueness.csv')
score_tab <- as.data.frame(table(score$PARSE_STATUS,useNA='ifany'),stringsAsFactors=FALSE);names(score_tab) <- c('PARSE_STATUS','N_RECORDS')
write_report(score_tab,'08_local_score_parse_summary.csv')
cat('\nVEGA Script 04 completed.\nOutput:',out,'\n')
cat('Local SCORE rows:',nrow(score),' | Unique valid candidates:',nrow(candidate),' | Duplicate rows:',nrow(dup),'\n')
cat('SUPPMO rows:',nrow(supp),' | Linked expanded rows:',nrow(linked),'\n')
print(table(link_summary$IDVAR,link_summary$LINK_STATUS))
