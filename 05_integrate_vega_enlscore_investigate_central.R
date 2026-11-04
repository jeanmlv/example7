#!/usr/bin/env Rscript
# VEGA SDTM load-2388: candidate ENLSCORE integration and central-reading investigation.
# Does NOT assert equivalence to ADaM endpoints or apply ICE/adjudication algorithms.
# Base R only. No clinical values are sent outside Domino.
options(stringsAsFactors = FALSE)
sdtm_dir <- '/domino/datasets/local/clinical-trial-data/CNTO1959UCO2002-VEGA-WK50/load-2388/Data/csv'
prior_ard <- '/mnt/CNTO1959UCO2002_VEGA_load2388_ard/08_VEGA_load2388_SDTM_ARD_REVIEWED.csv'
out_dir <- '/mnt/CNTO1959UCO2002_VEGA_load2388_script05'
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
write_out <- function(x, name) write.csv(x, file.path(out_dir,name), row.names=FALSE, na='')
clean <- function(x) {x <- trimws(as.character(x)); x[is.na(x)] <- ''; x}
up <- function(x) toupper(clean(x))
find_file <- function(domain) {
  ff <- list.files(sdtm_dir, pattern='\\.csv$', recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  ff <- ff[tolower(basename(ff)) == tolower(paste0(domain,'.sas7bdat.csv'))]
  if(length(ff)!=1L) stop('Expected exactly one ',domain,' CSV; found ',length(ff))
  ff
}
read_domain <- function(d) {x<-read.csv(find_file(d),check.names=FALSE); names(x)<-toupper(trimws(names(x))); x}
mo <- read_domain('mo'); supp <- read_domain('suppmo')
required_mo <- c('USUBJID','MOSEQ','MOSPID','MOTESTCD','MOEVAL','MOSTRESC','MOSTRESN','VISIT','VISITNUM','MODTC','MOSCAT','MOCAT')
if(!all(required_mo %in% names(mo))) stop('MO missing: ',paste(setdiff(required_mo,names(mo)),collapse=', '))
if(!all(c('USUBJID','IDVAR','IDVARVAL','QNAM','QVAL') %in% names(supp))) stop('SUPPMO required fields missing')
if(!file.exists(prior_ard)) stop('Prior ARD missing: ',prior_ard,'; set prior_ard to existing Script 02 output')
ard <- read.csv(prior_ard,check.names=FALSE)
if(!all(c('USUBJID','AVISIT','AVISITN','SDTM_ENLSCORE_AVAL','SDTM_ENLSCORE_AVALC') %in% names(ard))) stop('ARD missing expected keys/ENLSCORE columns')
# Numeric visit keys: 1000 and 1000.01 must remain different.
key <- function(id,visnum) paste(clean(id),format(as.numeric(visnum),scientific=FALSE,trim=TRUE,digits=15),sep='|')
ard_key <- key(ard$USUBJID,ard$AVISITN)
if(anyDuplicated(ard_key)) stop('Prior ARD contains duplicate USUBJID + AVISITN keys')
loc <- mo[up(mo$MOTESTCD)=='SCORE' & up(mo$MOEVAL)=='LOCAL READER',,drop=FALSE]
parse_score <- function(v) {
  z<-clean(v); matched<-grepl('^[0-3]([[:space:]]*=|[[:space:]]*$)',z)
  val<-rep(NA_real_,length(z)); val[matched]<-as.numeric(substr(z[matched],1,1)); val
}
loc$SCORE_PARSED <- parse_score(loc$MOSTRESC)
loc$PARSE_STATUS <- ifelse(is.na(loc$SCORE_PARSED),'REVIEW_UNPARSEABLE','VALID_0_3')
loc$KEY <- key(loc$USUBJID,loc$VISITNUM)
loc$N_AT_KEY <- as.integer(ave(rep(1L,nrow(loc)),loc$KEY,FUN=length))
loc$IN_ARD <- loc$KEY %in% ard_key
loc$MOSEQ_NUM <- suppressWarnings(as.numeric(loc$MOSEQ))
write_out(loc,'01_local_score_full_audit.csv')
write_out(loc[loc$N_AT_KEY>1L,,drop=FALSE],'02_local_score_duplicate_review.csv')
# Candidate only: unique source record, parsed score, key exists in ARD.
selected <- loc[loc$N_AT_KEY==1L & !is.na(loc$SCORE_PARSED) & loc$IN_ARD,,drop=FALSE]
if(anyDuplicated(selected$KEY)) stop('Unexpected duplicate in selected candidates')
write_out(selected[,intersect(c('USUBJID','VISIT','VISITNUM','MOSEQ','MOSPID','MODTC','MOEVAL','MOSTRESC','SCORE_PARSED','KEY'),names(selected)),drop=FALSE], '03_enlscore_candidates_unique.csv')
# Do not silently overwrite nonempty existing values.
neward <- ard
idx <- match(selected$KEY,ard_key)
existing <- !is.na(neward$SDTM_ENLSCORE_AVAL[idx]) & clean(neward$SDTM_ENLSCORE_AVAL[idx])!=''
if(any(existing)) stop('Refusing to overwrite existing ENLSCORE values in ',sum(existing),' rows')
neward$SDTM_ENLSCORE_AVAL[idx] <- selected$SCORE_PARSED
neward$SDTM_ENLSCORE_AVALC[idx] <- as.character(selected$SCORE_PARSED)
write_out(neward,'04_VEGA_SDTM_ARD_ENLSCORE_CANDIDATE_NOT_CLINICALLY_APPROVED.csv')
# SUPPMO linking: MOSEQ individual, MOSPID group; no row multiplication in candidate ARD.
supp$SUPP_ROW <- seq_len(nrow(supp)); supp$IDVAR <- up(supp$IDVAR)
mo$SEQ_KEY <- paste(clean(mo$USUBJID),clean(mo$MOSEQ),sep='|')
mo$SPID_KEY <- paste(clean(mo$USUBJID),clean(mo$MOSPID),sep='|')
supp$JOIN_KEY <- paste(clean(supp$USUBJID),clean(supp$IDVARVAL),sep='|')
seq_count <- table(mo$SEQ_KEY); spid_count <- table(mo$SPID_KEY)
supp$N_MATCHING_MO_ROWS <- 0L
is_seq <- supp$IDVAR=='MOSEQ'; is_spid <- supp$IDVAR=='MOSPID'
supp$N_MATCHING_MO_ROWS[is_seq] <- as.integer(seq_count[supp$JOIN_KEY[is_seq]])
supp$N_MATCHING_MO_ROWS[is_spid] <- as.integer(spid_count[supp$JOIN_KEY[is_spid]])
supp$N_MATCHING_MO_ROWS[is.na(supp$N_MATCHING_MO_ROWS)] <- 0L
supp$LINK_CLASS <- ifelse(supp$N_MATCHING_MO_ROWS==0,'UNMATCHED',ifelse(supp$N_MATCHING_MO_ROWS==1,'INDIVIDUAL','GROUP'))
write_out(supp,'05_suppmo_link_audit.csv')
# Central Mayo reading inventory: preserve read categories and qualifiers; do not pick final read.
cen <- mo[up(mo$MOTESTCD)=='MAYOSUBS',,drop=FALSE]
cen$KEY <- key(cen$USUBJID,cen$VISITNUM)
cen$READER <- up(cen$MOEVAL); cen$READ_TYPE <- up(cen$MOSCAT)
cen$SCORE_NUM <- suppressWarnings(as.numeric(clean(cen$MOSTRESN)))
cen$SEQ_KEY <- paste(clean(cen$USUBJID),clean(cen$MOSEQ),sep='|')
cen$SPID_KEY <- paste(clean(cen$USUBJID),clean(cen$MOSPID),sep='|')
# Attach READDTC qualifier as an aggregated string by source key, preserving multiplicity.
readq <- supp[up(supp$QNAM)=='READDTC' & supp$N_MATCHING_MO_ROWS>0,,drop=FALSE]
readq$SOURCE_KEY <- readq$JOIN_KEY
seq_read <- readq[readq$IDVAR=='MOSEQ',,drop=FALSE]; spid_read <- readq[readq$IDVAR=='MOSPID',,drop=FALSE]
aggregate_values <- function(k,v) {
  if(!length(k)) return(setNames(character(),character()))
  z <- split(clean(v),k)
  vapply(z,function(x) paste(unique(x[nzchar(x)]),collapse=' | '),character(1))
}
qseq <- aggregate_values(seq_read$SOURCE_KEY,seq_read$QVAL)
qspid <- aggregate_values(spid_read$SOURCE_KEY,spid_read$QVAL)
cen$READDTC_BY_MOSEQ <- unname(qseq[cen$SEQ_KEY]); cen$READDTC_BY_MOSPID <- unname(qspid[cen$SPID_KEY])
cen$READDTC_BY_MOSEQ[is.na(cen$READDTC_BY_MOSEQ)] <- ''
cen$READDTC_BY_MOSPID[is.na(cen$READDTC_BY_MOSPID)] <- ''
write_out(cen,'06_central_mayo_all_readings_with_readdtc.csv')
# Per patient-visit summary and reader-category inventory.
if(nrow(cen)) {
  groups <- split(seq_len(nrow(cen)),cen$KEY)
  central_summary <- do.call(rbind,lapply(groups,function(ix) {
    z<-cen[ix,,drop=FALSE]
    data.frame(USUBJID=z$USUBJID[1],VISIT=z$VISIT[1],VISITNUM=z$VISITNUM[1],N_READS=nrow(z),
      N_DISTINCT_READERS=length(unique(z$READER)),N_DISTINCT_SCORES=length(unique(z$SCORE_NUM[!is.na(z$SCORE_NUM)])),
      READ_TYPES=paste(sort(unique(z$READ_TYPE)),collapse=' | '),
      READERS=paste(sort(unique(z$READER)),collapse=' | '),
      SCORES=paste(sort(unique(z$SCORE_NUM[!is.na(z$SCORE_NUM)])),collapse=' | '),stringsAsFactors=FALSE)
  }))
} else central_summary <- data.frame()
write_out(central_summary,'07_central_patient_visit_reading_summary.csv')
reader_inventory <- as.data.frame(table(cen$READER,cen$READ_TYPE),stringsAsFactors=FALSE)
names(reader_inventory)<-c('MOEVAL','MOSCAT','N_RECORDS')
reader_inventory <- reader_inventory[reader_inventory$N_RECORDS>0,,drop=FALSE]
write_out(reader_inventory,'08_central_reader_type_inventory.csv')
# QC: distinguish rows from unique keys; no clinical approvals.
qc <- data.frame(METRIC=c('LOCAL_SCORE_RECORDS','LOCAL_SCORE_PARSED','LOCAL_SCORE_DUPLICATE_ROWS','LOCAL_SCORE_DUPLICATE_KEYS',
 'LOCAL_SCORE_UNIQUE_MATCHED_TO_ARD','LOCAL_SCORE_UNMATCHED_UNIQUE','ENLSCORE_NEW_CELLS_FILLED','SUPPMO_MATCHED_QUALIFIERS',
 'SUPPMO_UNMATCHED_QUALIFIERS','SUPPMO_GROUP_LINKED_QUALIFIERS','CENTRAL_MAYOSUBS_RECORDS','CENTRAL_PATIENT_VISITS'),
 VALUE=c(nrow(loc),sum(!is.na(loc$SCORE_PARSED)),sum(loc$N_AT_KEY>1),length(unique(loc$KEY[loc$N_AT_KEY>1])),
 nrow(selected),sum(loc$N_AT_KEY==1 & !loc$IN_ARD),sum(!is.na(neward$SDTM_ENLSCORE_AVAL) & clean(neward$SDTM_ENLSCORE_AVAL)!=''),
 sum(supp$N_MATCHING_MO_ROWS>0),sum(supp$N_MATCHING_MO_ROWS==0),sum(supp$LINK_CLASS=='GROUP'),nrow(cen),nrow(central_summary)))
write_out(qc,'09_script05_qc_summary.csv')
cat('\nScript 05 completed. Output: ',out_dir,'\n',sep='')
print(qc,row.names=FALSE)
cat('\nWARNING: ENLSCORE is a provisional SDTM local-reader candidate, NOT clinically approved.\n')
cat('Central Mayo readings are inventoried only; no final central score/ICE endpoint derived.\n')
