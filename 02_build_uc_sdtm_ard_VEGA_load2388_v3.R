#!/usr/bin/env Rscript
# VEGA WK50 / SDTM load-2388 — CONFIG-DRIVEN SDTM -> UC ARD v3 (base R)
# Produces all 37 requested PARAMCD columns; populates ONLY explicitly reviewed
# direct SDTM mappings + a PROVISIONAL local-reader ENLSCORE candidate.
# No ADaM equivalence, final central-reader algorithm, ICE or estimand derivation.
# Run: source('02_build_uc_sdtm_ard_VEGA_load2388_CONSOLIDATED.R')
options(stringsAsFactors=FALSE)
CFG <- list(
  input_dir='/domino/datasets/local/clinical-trial-data/CNTO1959UCO2002-VEGA-WK50/load-2388/Data/csv',
  mapping_dir='/mnt/CNTO1959UCO2002_VEGA_load2388_mapping',
  mapping_file='/mnt/CNTO1959UCO2002_VEGA_load2388_mapping/07_mapping_review_template_VEGA_REVISED.csv',
  output_dir='/mnt/CNTO1959UCO2002_VEGA_load2388_CONSOLIDATED',
  config_file='/mnt/CNTO1959UCO2002_VEGA_load2388_mapping/VEGA_37UC_mapping_config_v3.csv',
  enable_provisional_enlscore=TRUE,
  expand_grid_with_observed_visits=TRUE
)
if (!dir.exists(CFG$input_dir)) stop('SDTM input directory not found: ',CFG$input_dir)
if (!file.exists(CFG$config_file)) stop('37-variable configuration not found: ',CFG$config_file)
if (!dir.exists(CFG$mapping_dir)) stop('Mapping directory missing')
dir.create(CFG$output_dir,recursive=TRUE,showWarnings=FALSE)
if (file.access(CFG$output_dir,2)!=0) stop('Output directory is not writable')
clean <- function(x) { x<-trimws(as.character(x)); x[is.na(x)]<-''; x }
upper <- function(x) toupper(clean(x))
colv <- function(d,n) if(n %in% names(d)) clean(d[[n]]) else rep('',nrow(d))
readc <- function(p) { x<-read.csv(p,stringsAsFactors=FALSE,check.names=FALSE,colClasses='character',na.strings=c('', 'NA'),fileEncoding='UTF-8-BOM'); names(x)<-upper(names(x)); x }
out <- function(x,n) { write.csv(x,file.path(CFG$output_dir,n),row.names=FALSE,na=''); cat(sprintf('  %-48s %s rows\n',n,nrow(x))) }
key <- function(id,visit,visitn) paste(clean(id),upper(visit),clean(visitn),sep='\r')
keyn <- function(id,visitn) paste(clean(id),clean(visitn),sep='\r')
empty_trace <- function() data.frame(USUBJID=character(),AVISIT=character(),AVISITN=character(),PARAMCD=character(),DOMAIN=character(),TESTCD=character(),VALUE=character(),VALUE_FIELD=character(),SRCSEQ=character(),MOSPID=character(),EVALUATOR=character(),SOURCE_DTC=character(),SOURCE_FILE=character(),MAPPING_CLASS=character(),stringsAsFactors=FALSE)
empty_issue <- function() data.frame(PARAMCD=character(),ISSUE=character(),DETAIL=character(),stringsAsFactors=FALSE)
issue <- empty_issue(); add_issue <- function(p,code,detail) issue <<- rbind(issue,data.frame(PARAMCD=p,ISSUE=code,DETAIL=detail,stringsAsFactors=FALSE))
# Only exact SDTM source filenames, no cleaned/alternate loads or mixed versions.
files <- list.files(CFG$input_dir,pattern='\\.sas7bdat\\.csv$',full.names=TRUE,recursive=TRUE,ignore.case=TRUE)
domain_names <- upper(sub('\\.sas7bdat\\.csv$','',basename(files),ignore.case=TRUE))
if(anyDuplicated(domain_names)) stop('Duplicate source domain files: ',paste(unique(domain_names[duplicated(domain_names)]),collapse=', '))
index <- data.frame(DOMAIN=domain_names,PATH=files,stringsAsFactors=FALSE)
findfile <- function(d) { z<-index$PATH[index$DOMAIN==upper(d)]; if(length(z)==1) z else NA_character_ }
cache <- new.env(parent=emptyenv())
getdomain <- function(d) { d<-upper(d); if(exists(d,envir=cache,inherits=FALSE)) return(get(d,envir=cache)); p<-findfile(d); if(is.na(p)) stop('Domain file missing: ',d); x<-readc(p); assign(d,x,envir=cache); x }
out(index,'01_input_files.csv')
# Study variable dictionary from Step 01; require exactly the 37 known variables.
ovpath <- file.path(CFG$mapping_dir,'06_mapping_overview_best_candidate.csv')
if(!file.exists(ovpath)) stop('Missing 06_mapping_overview_best_candidate.csv')
ov <- readc(ovpath)
if(!all(c('PARAMCD','DESCRIPTION') %in% names(ov))) stop('Invalid variable overview schema')
voi <- unique(ov[,c('PARAMCD','DESCRIPTION'),drop=FALSE]); voi$PARAMCD<-upper(voi$PARAMCD)
if(anyDuplicated(voi$PARAMCD) || nrow(voi)!=37) stop('Expected 37 unique PARAMCD in variable overview; found ',nrow(voi))
# Explicit configuration for ALL 37 targets. Only approved observations can enter ARD.
review <- readc(CFG$config_file)
needed <- c('PARAMCD','STATUS','SOURCE_DOMAIN','SOURCE_TESTCD','VALUE_FIELD','TRANSFORM','EVAL_FILTER','CAT_FILTER','SCAT_FILTER')
if(!all(needed %in% names(review))) stop('Config missing: ',paste(setdiff(needed,names(review)),collapse=', '))
if(anyDuplicated(upper(review$PARAMCD)) || nrow(review)!=37L || !setequal(upper(review$PARAMCD),voi$PARAMCD)) stop('Config must contain exactly the 37 distinct target PARAMCDs')
for(n in needed) review[[n]] <- clean(review[[n]])
for(n in c('PARAMCD','STATUS','SOURCE_DOMAIN','SOURCE_TESTCD','VALUE_FIELD','TRANSFORM')) review[[n]]<-upper(review[[n]])
allowed_status <- c('APPROVED_OBSERVED','PROVISIONAL_OBSERVED','PENDING_CLINICAL_REVIEW')
if(any(!(review$STATUS %in% allowed_status))) stop('Invalid config STATUS')
review$DECISION <- ifelse(review$STATUS=='APPROVED_OBSERVED','APPROVED_REQUESTED',ifelse(review$STATUS=='PROVISIONAL_OBSERVED','PROVISIONAL_ONLY','NOT_APPROVED'))
# Safety: approved direct sources must be exact previously verified mappings;
# future approvals require explicit update of this allowlist after independent review.
validated <- c('ARCHCH'='MI|ARCHCH|MISTRESN','CHRIINF'='MI|CHRIINF|MISTRESN','NEUTLP'='MI|NEUTLP|MISTRESN','NEUTEP'='MI|NEUTEP|MISTRESN','CRYPTDES'='MI|CRYPTDES|MISTRESN','ULCER'='MI|ULCER|MISTRESN','NANCYHI'='MI|NANCYHI|MISTRESN','EOS'='LB|EOS|LBSTRESN')
for(i in which(review$DECISION=='APPROVED_REQUESTED')) {
 p<-review$PARAMCD[i]; sig<-paste(review$SOURCE_DOMAIN[i],review$SOURCE_TESTCD[i],review$VALUE_FIELD[i],sep='|')
 if(!(p %in% names(validated)) || sig!=unname(validated[p]) || review$TRANSFORM[i]!='DIRECT_NUMERIC') review$DECISION[i]<-'BLOCKED_UNVALIDATED_SOURCE_OR_RULE'
}
if(any(review$DECISION=='BLOCKED_UNVALIDATED_SOURCE_OR_RULE')) add_issue('CONFIG','UNVALIDATED_APPROVAL_BLOCKED',paste(review$PARAMCD[review$DECISION=='BLOCKED_UNVALIDATED_SOURCE_OR_RULE'],collapse='|'))
if(!identical(review$STATUS[review$PARAMCD=='ENLSCORE'],'PROVISIONAL_OBSERVED')) CFG$enable_provisional_enlscore <- FALSE
out(review,'02_mapping_decisions.csv')
accepted <- review[review$DECISION=='APPROVED_REQUESTED',,drop=FALSE]
accepted$DOMAIN<-accepted$SOURCE_DOMAIN
accepted$CODE<-accepted$SOURCE_TESTCD
if(anyDuplicated(accepted$PARAMCD)) stop('Multiple approved mappings for one PARAMCD')
# The remaining candidates are inventoried, never silently treated as observed endpoints.
out(review[review$DECISION!='APPROVED_REQUESTED',,drop=FALSE],'16_pending_clinical_mapping_review.csv')
# SV is the authoritative visit grid. Exact visit labels AND visit numbers are retained.
sv <- getdomain('SV')
if(!all(c('USUBJID','VISIT','VISITNUM') %in% names(sv))) stop('SV missing required key fields')
grid <- unique(data.frame(USUBJID=colv(sv,'USUBJID'),AVISIT=colv(sv,'VISIT'),AVISITN=colv(sv,'VISITNUM'),stringsAsFactors=FALSE))
grid <- grid[nzchar(grid$USUBJID) & nzchar(grid$AVISIT),,drop=FALSE]
grid$KEY <- key(grid$USUBJID,grid$AVISIT,grid$AVISITN)
if(anyDuplicated(grid$KEY)) stop('Duplicate exact SV patient/visit keys')
# Extract all approved observed values, with original source traceability.
parts<-list(); get_part <- function(x) if(nrow(x)) parts[[length(parts)+1L]] <<- x
for(i in seq_len(nrow(accepted))) {
  m<-accepted[i,,drop=FALSE]; d<-m$DOMAIN; p<-m$PARAMCD; code<-m$CODE; x<-getdomain(d)
  cc<-paste0(d,'TESTCD'); vc<-m$VALUE_FIELD
  if(!all(c(cc,vc,'USUBJID','VISIT','VISITNUM') %in% names(x))) {add_issue(p,'REQUIRED_SOURCE_COLUMN_MISSING',paste(setdiff(c(cc,vc,'USUBJID','VISIT','VISITNUM'),names(x)),collapse='|')); next}
  x<-x[upper(x[[cc]])==code,,drop=FALSE]
  for(f in c('EVAL','CAT','SCAT')) {
    filter <- clean(m[[paste0(f,'_FILTER')]])
    if(nzchar(filter)) x<-x[upper(colv(x,paste0(d,f)))==upper(filter),,drop=FALSE]
  }
  if(!nrow(x)) {add_issue(p,'NO_MATCHING_RECORDS',paste(d,code)); next}
  value <- clean(x[[vc]])
  valid <- nzchar(value) & !is.na(suppressWarnings(as.numeric(value))) & is.finite(suppressWarnings(as.numeric(value)))
  if(any(nzchar(value) & !valid)) add_issue(p,'NON_NUMERIC_SOURCE_VALUES',as.character(sum(nzchar(value) & !valid)))
  x<-x[valid,,drop=FALSE]; value<-value[valid]
  if(!nrow(x)) {add_issue(p,'NO_NUMERIC_RESULTS',paste(d,code)); next}
  z<-data.frame(USUBJID=colv(x,'USUBJID'),AVISIT=colv(x,'VISIT'),AVISITN=colv(x,'VISITNUM'),PARAMCD=p,DOMAIN=d,TESTCD=code,VALUE=value,VALUE_FIELD=vc,SRCSEQ=colv(x,paste0(d,'SEQ')),MOSPID=colv(x,paste0(d,'SPID')),EVALUATOR=colv(x,paste0(d,'EVAL')),SOURCE_DTC=colv(x,paste0(d,'DTC')),SOURCE_FILE=findfile(d),MAPPING_CLASS='VALIDATED_DIRECT_SDTM',stringsAsFactors=FALSE)
  z<-z[nzchar(z$USUBJID) & nzchar(z$AVISIT),,drop=FALSE]
  get_part(z)
}
# Local-reader candidate: controlled parsing of 0-3 from MOSTRESC.
local_audit <- data.frame(); local_dups <- data.frame()
mo <- getdomain('MO')
if(CFG$enable_provisional_enlscore) {
  req<-c('USUBJID','VISIT','VISITNUM','MOTESTCD','MOEVAL','MOSTRESC','MOSEQ')
  if(!all(req %in% names(mo))) stop('MO missing local score fields: ',paste(setdiff(req,names(mo)),collapse=','))
  loc<-mo[upper(colv(mo,'MOTESTCD'))=='SCORE' & upper(colv(mo,'MOEVAL'))=='LOCAL READER',,drop=FALSE]
  txt<-clean(colv(loc,'MOSTRESC'))
  valid<-grepl('^[0-3]([[:space:]]*=|[[:space:]]*$)',txt)
  parsed<-rep('',length(txt)); parsed[valid]<-substr(txt[valid],1L,1L)
  loc$SCORE_PARSED<-parsed; loc$PARSE_STATUS<-ifelse(valid,'VALID_0_3','REVIEW_UNPARSEABLE')
  loc$GROUP_KEY<-key(loc$USUBJID,loc$VISIT,loc$VISITNUM)
  loc$N_AT_KEY<-as.integer(ave(rep(1L,nrow(loc)),loc$GROUP_KEY,FUN=length))
  local_audit<-loc; local_dups<-loc[loc$N_AT_KEY>1L,,drop=FALSE]
  selected<-loc[loc$N_AT_KEY==1L & valid,,drop=FALSE]
  if(nrow(selected)) {
    z<-data.frame(USUBJID=colv(selected,'USUBJID'),AVISIT=colv(selected,'VISIT'),AVISITN=colv(selected,'VISITNUM'),PARAMCD='ENLSCORE',DOMAIN='MO',TESTCD='SCORE',VALUE=selected$SCORE_PARSED,VALUE_FIELD='MOSTRESC_PARSED_0_3',SRCSEQ=colv(selected,'MOSEQ'),MOSPID=colv(selected,'MOSPID'),EVALUATOR=colv(selected,'MOEVAL'),SOURCE_DTC=colv(selected,'MODTC'),SOURCE_FILE=findfile('MO'),MAPPING_CLASS='PROVISIONAL_LOCAL_READER_NOT_CLINICALLY_APPROVED',stringsAsFactors=FALSE)
    get_part(z)
  }
  if(nrow(local_dups)) add_issue('ENLSCORE','DUPLICATE_LOCAL_SCORE_KEYS',as.character(length(unique(local_dups$GROUP_KEY))))
  if(any(!valid)) add_issue('ENLSCORE','UNPARSEABLE_LOCAL_SCORE',as.character(sum(!valid)))
}
out(local_audit,'03_local_score_conversion_audit.csv')
out(local_dups,'04_local_score_duplicate_review.csv')
long<-if(length(parts)) do.call(rbind,parts) else empty_trace()
# Detect duplicates before any join; preserve all conflicting source rows in QC.
long$JOIN_KEY <- key(long$USUBJID,long$AVISIT,long$AVISITN)
long$GROUP_KEY <- paste(long$JOIN_KEY,long$PARAMCD,sep='\r')
frequency<-table(long$GROUP_KEY)
conflict_keys<-names(frequency)[frequency>1L]
dups<-long[long$GROUP_KEY %in% conflict_keys,,drop=FALSE]
unique_long<-long[!(long$GROUP_KEY %in% conflict_keys),,drop=FALSE]
# Strict check of SV grid: do not use patient+visitnum alone to silently join different labels.
missing <- !(unique_long$JOIN_KEY %in% grid$KEY)
if(any(missing)) {
  extra<-unique(unique_long[missing,c('USUBJID','AVISIT','AVISITN'),drop=FALSE])
  out(extra,'05_observed_visits_not_in_sv.csv')
  add_issue('ALL','OBSERVED_VISITS_NOT_IN_SV',as.character(nrow(extra)))
  if(CFG$expand_grid_with_observed_visits) {
    extra$KEY<-key(extra$USUBJID,extra$AVISIT,extra$AVISITN)
    grid<-rbind(grid,extra[,names(grid),drop=FALSE])
  } else unique_long<-unique_long[!missing,,drop=FALSE]
} else out(data.frame(USUBJID=character(),AVISIT=character(),AVISITN=character()),'05_observed_visits_not_in_sv.csv')
# Create 37 x 2 columns; keep raw source data separate from derived endpoints.
for(p in voi$PARAMCD) {grid[[paste0('SDTM_',p,'_AVAL')]]<-'';grid[[paste0('SDTM_',p,'_AVALC')]]<-''}
idx<-match(unique_long$JOIN_KEY,grid$KEY)
if(anyNA(idx)) stop('Unexpected unmatched key after grid expansion')
for(j in seq_len(nrow(unique_long))) {
  p<-unique_long$PARAMCD[j]; if(!(p %in% voi$PARAMCD)) next
  v<-unique_long$VALUE[j]; grid[[paste0('SDTM_',p,'_AVAL')]][idx[j]]<-v
  # AVALC intentionally blank for numeric parameters (no double counting).
}
# Complete traceability, including source rows excluded due to duplicate keys.
long$SOURCE_STATUS<-ifelse(long$GROUP_KEY %in% conflict_keys,'EXCLUDED_DUPLICATE','ELIGIBLE_UNIQUE')
long$JOIN_STATUS<-ifelse(long$JOIN_KEY %in% grid$KEY,'MATCHED_ARD','NOT_IN_ARD')
out(long,'06_traceability_all.csv'); out(dups,'07_duplicate_conflicts.csv')
# Supplemental qualifier linkage: individual MOSEQ and group MOSPID, no row expansion in ARD.
supp<-getdomain('SUPPMO')
if(!all(c('USUBJID','IDVAR','IDVARVAL','QNAM','QVAL') %in% names(supp))) stop('SUPPMO missing linkage fields')
supp$IDVAR<-upper(supp$IDVAR)
mo$SEQ_KEY<-keyn(colv(mo,'USUBJID'),colv(mo,'MOSEQ'))
mo$SPID_KEY<-keyn(colv(mo,'USUBJID'),colv(mo,'MOSPID'))
supp$JOIN_KEY<-keyn(colv(supp,'USUBJID'),colv(supp,'IDVARVAL'))
seq_counts<-table(mo$SEQ_KEY); spid_counts<-table(mo$SPID_KEY)
supp$N_MATCHING_MO_ROWS<-0L
is_seq<-supp$IDVAR=='MOSEQ'; is_spid<-supp$IDVAR=='MOSPID'
supp$N_MATCHING_MO_ROWS[is_seq]<-as.integer(seq_counts[supp$JOIN_KEY[is_seq]])
supp$N_MATCHING_MO_ROWS[is_spid]<-as.integer(spid_counts[supp$JOIN_KEY[is_spid]])
supp$N_MATCHING_MO_ROWS[is.na(supp$N_MATCHING_MO_ROWS)]<-0L
supp$LINK_CLASS<-ifelse(supp$N_MATCHING_MO_ROWS==0,'UNMATCHED',ifelse(supp$N_MATCHING_MO_ROWS==1,'INDIVIDUAL','GROUP'))
out(supp,'08_suppmo_link_audit.csv')
# Central Mayo: inventory only; do not choose final/adjudicated reading without protocol rules.
central<-mo[upper(colv(mo,'MOTESTCD'))=='MAYOSUBS',,drop=FALSE]
central$READ_TYPE<-upper(colv(central,'MOSCAT'))
central$READER<-upper(colv(central,'MOEVAL'))
central$SCORE_NUM<-suppressWarnings(as.numeric(colv(central,'MOSTRESN')))
central$SEQ_KEY<-keyn(colv(central,'USUBJID'),colv(central,'MOSEQ'))
central$SPID_KEY<-keyn(colv(central,'USUBJID'),colv(central,'MOSPID'))
readq<-supp[upper(supp$QNAM)=='READDTC' & supp$N_MATCHING_MO_ROWS>0,,drop=FALSE]
agg <- function(z) { if(!nrow(z)) return(setNames(character(),character())); s<-split(clean(z$QVAL),z$JOIN_KEY); vapply(s,function(x)paste(unique(x[nzchar(x)]),collapse=' | '),character(1)) }
qs<-agg(readq[readq$IDVAR=='MOSEQ',,drop=FALSE]); qp<-agg(readq[readq$IDVAR=='MOSPID',,drop=FALSE])
central$READDTC_BY_MOSEQ<-unname(qs[central$SEQ_KEY]); central$READDTC_BY_MOSPID<-unname(qp[central$SPID_KEY])
central$READDTC_BY_MOSEQ[is.na(central$READDTC_BY_MOSEQ)]<-''
central$READDTC_BY_MOSPID[is.na(central$READDTC_BY_MOSPID)]<-''
out(central,'09_central_mayo_readings_QC_ONLY.csv')
central$PVKEY<-key(central$USUBJID,colv(central,'VISIT'),colv(central,'VISITNUM'))
if(nrow(central)) {
  ss<-split(seq_len(nrow(central)),central$PVKEY)
  central_summary<-do.call(rbind,lapply(ss,function(ix) {z<-central[ix,,drop=FALSE]; v<-unique(z$SCORE_NUM[!is.na(z$SCORE_NUM)]);data.frame(USUBJID=z$USUBJID[1],VISIT=z$VISIT[1],VISITNUM=z$VISITNUM[1],N_READS=nrow(z),N_READERS=length(unique(z$READER)),N_SCORES=length(v),SCORES=paste(sort(v),collapse='|'),READ_TYPES=paste(sort(unique(z$READ_TYPE)),collapse='|'),stringsAsFactors=FALSE)}))
} else central_summary<-data.frame(USUBJID=character(),VISIT=character(),VISITNUM=character(),N_READS=integer(),N_READERS=integer(),N_SCORES=integer(),SCORES=character(),READ_TYPES=character())
out(central_summary,'10_central_mayo_patient_visit_summary.csv')
# UCEIS: source evidence by patient/visit/reader/reading context. Do not apply ICE.
uceis_codes<-c('UCEISSC','BLEEDING','EROSION','VASCULAR')
u<-mo[upper(colv(mo,'MOTESTCD')) %in% uceis_codes,,drop=FALSE]
u$PV_READER<-paste(key(u$USUBJID,colv(u,'VISIT'),colv(u,'VISITNUM')),upper(colv(u,'MOEVAL')),upper(colv(u,'MOSCAT')),sep='\r')
u$CODE<-upper(colv(u,'MOTESTCD')); u$NUMERIC_VALUE<-suppressWarnings(as.numeric(colv(u,'MOSTRESN')))
if(nrow(u)) {
  groups<-split(seq_len(nrow(u)),u$PV_READER)
  uceis_qc<-do.call(rbind,lapply(groups,function(ix) {
    z<-u[ix,,drop=FALSE]; cnt<-vapply(uceis_codes,function(c)sum(z$CODE==c),integer(1))
    vals<-vapply(uceis_codes,function(c){v<-z$NUMERIC_VALUE[z$CODE==c]; if(length(v)==1 && is.finite(v)) v else NA_real_},numeric(1))
    complete<-all(cnt==1) && all(is.finite(vals))
    data.frame(USUBJID=z$USUBJID[1],VISIT=z$VISIT[1],VISITNUM=z$VISITNUM[1],MOEVAL=colv(z,'MOEVAL')[1],MOSCAT=colv(z,'MOSCAT')[1],N_RECORDS=nrow(z),N_UCEISSC=cnt[1],N_BLEEDING=cnt[2],N_EROSION=cnt[3],N_VASCULAR=cnt[4],UCEISSC=vals[1],COMPONENT_SUM=if(complete)sum(vals[2:4]) else NA_real_,STATUS=if(!complete)'INCOMPLETE_OR_MULTIPLE' else if(isTRUE(all.equal(vals[1],sum(vals[2:4]))))'CONSISTENT' else 'INCONSISTENT',stringsAsFactors=FALSE)
  }))
} else uceis_qc<-data.frame()
out(uceis_qc,'11_uceis_components_QC_ONLY.csv')
# Final dataset and summary. Not called a clinically approved ADaM ARD.
grid$KEY<-NULL
if(nrow(grid)) grid<-grid[order(grid$USUBJID,suppressWarnings(as.numeric(grid$AVISITN)),grid$AVISIT),,drop=FALSE]
if(anyDuplicated(key(grid$USUBJID,grid$AVISIT,grid$AVISITN))) stop('Final ARD key is not unique')
out(grid,'12_VEGA_load2388_37UC_SDTM_ARD_CONSOLIDATED_CANDIDATE.csv')
status<-data.frame(PARAMCD=voi$PARAMCD,DESCRIPTION=voi$DESCRIPTION,N_POPULATED=integer(nrow(voi)),STATUS=character(nrow(voi)),stringsAsFactors=FALSE)
for(i in seq_len(nrow(status))) {
  p<-status$PARAMCD[i]; status$N_POPULATED[i]<-sum(nzchar(grid[[paste0('SDTM_',p,'_AVAL')]]))
  status$STATUS[i]<-if(p=='ENLSCORE' && status$N_POPULATED[i]>0)'PROVISIONAL_LOCAL_READER_NOT_CLINICALLY_APPROVED' else if(status$N_POPULATED[i]>0)'VALIDATED_DIRECT_SDTM_OBSERVATION' else 'NOT_POPULATED_REQUIRES_MAPPING_OR_CLINICAL_RULE'
}
out(status,'13_37_variable_coverage.csv')
out(issue,'14_mapping_issues.csv')
metrics<-data.frame(METRIC=c('ARD_ROWS','UNIQUE_PATIENTS','PARAMETERS_TOTAL','PARAMETERS_WITH_VALUES','DIRECT_MAPPINGS_APPROVED','DIRECT_NUMERIC_CELLS','ENLSCORE_PROVISIONAL_CELLS','DUPLICATE_SOURCE_ROWS_EXCLUDED','SUPPMO_UNMATCHED_QUALIFIERS','CENTRAL_MAYO_SOURCE_READINGS','UCEIS_QC_GROUPS'),VALUE=c(nrow(grid),length(unique(grid$USUBJID)),nrow(status),sum(status$N_POPULATED>0),nrow(accepted),sum(status$N_POPULATED[status$PARAMCD %in% names(validated)]),status$N_POPULATED[status$PARAMCD=='ENLSCORE'],nrow(dups),sum(supp$N_MATCHING_MO_ROWS==0),nrow(central),nrow(uceis_qc)),stringsAsFactors=FALSE)
out(metrics,'15_run_summary.csv')
cat('\nDONE: ',CFG$output_dir,'\n',sep='')
print(metrics,row.names=FALSE)
cat('\nIMPORTANT: ARD is an SDTM OBSERVED/CANDIDATE dataset, NOT a final clinically approved ADaM/ICE endpoint ARD.\n')
cat('Unapproved endpoints remain blank by design; central Mayo and UCEIS outputs are QC-only.\n')
cat('Review 16_pending_clinical_mapping_review.csv before requesting clinical finalization.\n')
