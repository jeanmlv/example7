# ANTHEM UC - ADaM record-level reference extractor (Domino)
# Requires: data.table; haven only for SAS files.
suppressPackageStartupMessages(library(data.table))

ROOT <- '/domino/datasets/local/clinical-trial-data'
OUTPUT_DIR <- '/mnt/anthem_dtype_reference'
# Edit folder names if your Domino mount differs. Order: preferred first.
LOADS <- c(
 '77242113UC02001-ANTHEM-UC-UNBLINDED-WK78',
 '77242113UC02001-ANTHEM-UC-UNBLINDED-WK28',
 '77242113UC02001-ANTHEM-UC-UNBLINDED-WK12',
 '77242113UC02001-ANTHEM-UC-WK28-HIST',
 '77242113UC02001-ANTHEM-UC-WK12-HIST'
)
# Only parameters used in the Slim ARD; empty list means all parameters.
PARAMS <- list(
 ADMAYO = c('ASTOOL','ASTOOLAL','DRBLEED','DSTOOL','ENASCOR','ENCSCOR','ENFSCOR','ENFSCORI','ENLSCOR','FMAYO','FMAYOAL','MFMAYO','MFMAYOAL','MMAYO','MMAYOAL','MMMAYO','MMMAYOAL','MPMAYO','MPMAYOAL','NFMAYO','NFMAYOAL','NMMAYO','NMMAYOAL','NPMAYO','NPMAYOAL','PGSCOR','PMAYO','PMAYOAL','RBSCOR','RBSCORAL','RBSCORI','SFSCOR','SFSCORAL','SFSCORI','CREM','CREMA1','CREMA2','CREMAL','CRESP','CRESPAL','CS1CREM','CSCREM','ENIMP','ENREM','RBSCOR0','SFSCOR0','SFSCOR01','SREM','SREMA1','SREMA2','SREMAL'),
 ADIBDQ = c('IBDQB','IBDQE','IBDQSF','IBDQSY','IBDQTOT','IBDQREM'),
 ADLBI = c('CALPRO','CALPROP','CRP','CRPP','LCALPRO','LCALPROP','LCRP','LCRPP','CRPNRM','CRPNRMP')
)
DTYPE_PRIORITY_MAP <- c(''=1L,'DERIVED'=2L,'AVERAGE'=3L,'LOCF'=4L,'WOCF'=5L,'BOCF'=6L,'TF'=7L)

stopifnot(dir.exists(ROOT))
dir.create(OUTPUT_DIR, recursive=TRUE, showWarnings=FALSE)
refs <- list(); logrows <- list(); idx <- 0L
for (i in seq_along(LOADS)) {
  load_name <- LOADS[i]; folder <- file.path(ROOT,load_name)
  if (!dir.exists(folder)) {
    warning('Missing load folder: ', folder)
    logrows[[length(logrows)+1L]] <- data.table(load=load_name,source='',path=folder,status='MISSING_FOLDER',rows=0L)
    next
  }
  paths <- list.files(folder, pattern='\\.(csv|sas7bdat)$', recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  for (src in names(PARAMS)) {
    # Files generated from SAS often use tmpadmayo.sas7bdat.csv; match exact dataset stem.
    stems <- toupper(basename(paths))
    stems <- sub('\\.CSV$','',stems)
    stems <- sub('\\.SAS7BDAT$','',stems)
    candidate <- paths[stems %in% c(src,paste0('TMP',src))]
    if (!length(candidate)) {
      logrows[[length(logrows)+1L]] <- data.table(load=load_name,source=src,path='',status='NOT_FOUND',rows=0L)
      next
    }
    # Prefer CSV over SAS for duplicate representations of the same dataset.
    candidate <- candidate[order(!grepl('\\.csv$',candidate,ignore.case=TRUE),nchar(candidate))]
    path <- candidate[1]
    tryCatch({
      d <- if (grepl('\\.csv$',path,ignore.case=TRUE)) fread(path,na.strings=c('','NA'),showProgress=FALSE) else as.data.table(haven::read_sas(path))
      setnames(d,toupper(names(d)))
      needed <- c('USUBJID','AVISIT','AVISITN','PARAMCD')
      if (!all(needed %in% names(d)) || !any(c('AVAL','AVALC') %in% names(d))) stop('Required keys or values missing')
      d <- d[!is.na(PARAMCD) & as.character(PARAMCD) %in% PARAMS[[src]]]
      for (nm in c('AVAL','AVALC','DTYPE','ASEQ','ANL01FL','ANL02FL')) if (!nm %in% names(d)) d[, (nm):=NA_character_]
      keep <- c(needed,'AVAL','AVALC','DTYPE','ASEQ','ANL01FL','ANL02FL')
      d <- d[,..keep]
      for (nm in keep) set(d,j=nm,value=trimws(as.character(d[[nm]])))
      for (nm in keep) set(d,which(is.na(d[[nm]]) | d[[nm]] %in% c('NA','NaN','NULL')),nm,'')
      d[, `:=`(SOURCE=src,LOAD=load_name,LOAD_RANK=i)]
      d[, DTYPE_CLEAN:=toupper(DTYPE)]
      d[, DTYPE_RANK:=as.integer(DTYPE_PRIORITY_MAP[DTYPE_CLEAN])]
      d[is.na(DTYPE_RANK),DTYPE_RANK:=99L]
      idx <- idx+1L; refs[[idx]] <- d
      logrows[[length(logrows)+1L]] <- data.table(load=load_name,source=src,path=path,status='OK',rows=nrow(d))
      cat(sprintf('OK %s %s: %d rows\n',load_name,src,nrow(d)))
    },error=function(e) {
      warning(load_name,'/',src,': ',conditionMessage(e))
      logrows[[length(logrows)+1L]] <<- data.table(load=load_name,source=src,path=path,status=paste0('ERROR: ',conditionMessage(e)),rows=0L)
    })
  }
}
manifest <- rbindlist(logrows,fill=TRUE)
fwrite(manifest,file.path(OUTPUT_DIR,'extraction_manifest.csv'))
if (!length(refs)) stop('No parameterized ADaM records extracted; inspect extraction_manifest.csv')
ref <- rbindlist(refs,fill=TRUE)
# Keep all records and loads. Never deduplicate here: preserve provenance.
setcolorder(ref,c('USUBJID','AVISIT','AVISITN','SOURCE','PARAMCD','AVAL','AVALC','DTYPE','DTYPE_CLEAN','DTYPE_RANK','ASEQ','ANL01FL','ANL02FL','LOAD','LOAD_RANK'))
fwrite(ref,file.path(OUTPUT_DIR,'anthem_adam_record_reference.csv'),na='')
cat(sprintf('Reference: %d rows, %d loads, %d subjects\n',nrow(ref),uniqueN(ref$LOAD),uniqueN(ref$USUBJID)))
cat('Files saved in: ',OUTPUT_DIR,'\n')
