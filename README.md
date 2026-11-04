# example7


> sdtm_dir <- paste0(
+   "/domino/datasets/local/clinical-trial-data/",
+   "CNTO1959UCO2002-VEGA-WK50/",
+   "load-2388/Data/csv"
+ )
> 
> check_mapping <- function(domain, codes) {
+   files <- list.files(
+     sdtm_dir,
+     pattern = "\\.csv$",
+     full.names = TRUE,
+     recursive = TRUE,
+     ignore.case = TRUE
+   )
+   
+   files <- files[
+     grepl(
+       paste0("^", domain, "\\.sas7bdat\\.csv$"),
+       basename(files),
+       ignore.case = TRUE
+     )
+   ]
+   
+   stopifnot(length(files) == 1)
+   
+   x <- read.csv(
+     files[1],
+     stringsAsFactors = FALSE,
+     check.names = FALSE
+   )
+   
+   names(x) <- toupper(trimws(names(x)))
+   
+   code_col <- paste0(domain, "TESTCD")
+   value_col <- paste0(domain, "STRESN")
+   
+   stopifnot(code_col %in% names(x))
+   
+   actual <- toupper(trimws(as.character(x[[code_col]])))
+   
+   cat("\nDOMAIN:", domain, "\n")
+   cat("Rows:", nrow(x), "\n")
+   cat("Columns:", paste(names(x), collapse = ", "), "\n")
+   
+   result <- data.frame(
+     CODE = codes,
+     N_MATCHES = vapply(
+       codes,
+       function(z) sum(actual == z, na.rm = TRUE),
+       integer(1)
+     )
+   )
+   
+   print(result)
+   
+   for (code in codes) {
+     idx <- which(actual == code)
+     
+     if (length(idx) > 0) {
+       keep <- intersect(
+         c("USUBJID", "VISIT", "VISITNUM",
+           code_col, value_col),
+         names(x)
+       )
+       
+       cat("\nExample:", code, "\n")
+       print(utils::head(x[idx, keep, drop = FALSE], 3))
+     }
+   }
+ }
> 
> check_mapping(
+   "MI",
+   c("ARCHCH", "CHRIINF", "NEUTLP", "NEUTEP",
+     "CRYPTDES", "ULCER", "NANCYHI")
+ )

DOMAIN: MI 
Rows: 6111 
Columns: STUDYID, DOMAIN, USUBJID, MISEQ, MIREFID, MISPID, MITESTCD, MITEST, MITSTDTL, MICAT, MIORRES, MISTRESC, MISTRESN, MISTAT, MIREASND, MINAM, MISPEC, MISPCCND, MILOC, MIMETHOD, MIBLFL, VISITNUM, VISIT, VISITDY, EPOCH, MIDTC, MIDY 
             CODE N_MATCHES
ARCHCH     ARCHCH       679
CHRIINF   CHRIINF       679
NEUTLP     NEUTLP       679
NEUTEP     NEUTEP       679
CRYPTDES CRYPTDES       679
ULCER       ULCER       679
NANCYHI   NANCYHI       679

Example: ARCHCH 
                 USUBJID     VISIT VISITNUM MITESTCD MISTRESN
1 CNTO1959UCO2002-100001 SCREENING     1000   ARCHCH        2
2 CNTO1959UCO2002-100001   WEEK 12     2012   ARCHCH        1
3 CNTO1959UCO2002-100001   WEEK 38     2038   ARCHCH        2

Example: CHRIINF 
                 USUBJID     VISIT VISITNUM MITESTCD MISTRESN
7 CNTO1959UCO2002-100001 SCREENING     1000  CHRIINF        2
8 CNTO1959UCO2002-100001   WEEK 12     2012  CHRIINF        0
9 CNTO1959UCO2002-100001   WEEK 38     2038  CHRIINF        0

Example: NEUTLP 
                  USUBJID     VISIT VISITNUM MITESTCD MISTRESN
22 CNTO1959UCO2002-100001 SCREENING     1000   NEUTLP        1
23 CNTO1959UCO2002-100001   WEEK 12     2012   NEUTLP        0
24 CNTO1959UCO2002-100001   WEEK 38     2038   NEUTLP        0

Example: NEUTEP 
                  USUBJID     VISIT VISITNUM MITESTCD MISTRESN
19 CNTO1959UCO2002-100001 SCREENING     1000   NEUTEP        1
20 CNTO1959UCO2002-100001   WEEK 12     2012   NEUTEP        0
21 CNTO1959UCO2002-100001   WEEK 38     2038   NEUTEP        0

Example: CRYPTDES 
                  USUBJID     VISIT VISITNUM MITESTCD MISTRESN
10 CNTO1959UCO2002-100001 SCREENING     1000 CRYPTDES        0
11 CNTO1959UCO2002-100001   WEEK 12     2012 CRYPTDES        0
12 CNTO1959UCO2002-100001   WEEK 38     2038 CRYPTDES        0

Example: ULCER 
                  USUBJID     VISIT VISITNUM MITESTCD MISTRESN
25 CNTO1959UCO2002-100001 SCREENING     1000    ULCER        1
26 CNTO1959UCO2002-100001   WEEK 12     2012    ULCER        0
27 CNTO1959UCO2002-100001   WEEK 38     2038    ULCER        0

Example: NANCYHI 
                  USUBJID     VISIT VISITNUM MITESTCD MISTRESN
16 CNTO1959UCO2002-100001 SCREENING     1000  NANCYHI        2
17 CNTO1959UCO2002-100001   WEEK 12     2012  NANCYHI        1
18 CNTO1959UCO2002-100001   WEEK 38     2038  NANCYHI        0
> 
> check_mapping("LB", "EOS")

DOMAIN: LB 
Rows: 116496 
Columns: STUDYID, DOMAIN, USUBJID, LBSEQ, LBSPID, LBTESTCD, LBTEST, LBCAT, LBORRES, LBORRESU, LBORNRLO, LBORNRHI, LBSTRESC, LBSTRESN, LBSTRESU, LBSTNRLO, LBSTNRHI, LBSTNRC, LBNRIND, LBSTAT, LBREASND, LBNAM, LBSPEC, LBSPCCND, LBMETHOD, LBBLFL, VISITNUM, VISIT, VISITDY, EPOCH, LBDTC, LBDY 
    CODE N_MATCHES
EOS  EOS      3811

Example: EOS 
                   USUBJID     VISIT VISITNUM LBTESTCD LBSTRESN
342 CNTO1959UCO2002-100001 SCREENING     1000      EOS     0.55
343 CNTO1959UCO2002-100001    WEEK 0     2000      EOS     0.49
344 CNTO1959UCO2002-100001    WEEK 2     2002      EOS     0.19
> 
> sdtm_dir <- paste0(
+   "/domino/datasets/local/clinical-trial-data/",
+   "CNTO1959UCO2002-VEGA-WK50/",
+   "load-2388/Data/csv"
+ )
> 
> check_mapping <- function(domain, codes) {
+   files <- list.files(
+     sdtm_dir,
+     pattern = "\\.csv$",
+     full.names = TRUE,
+     recursive = TRUE,
+     ignore.case = TRUE
+   )
+   
+   files <- files[
+     grepl(
+       paste0("^", domain, "\\.sas7bdat\\.csv$"),
+       basename(files),
+       ignore.case = TRUE
+     )
+   ]
+   
+   stopifnot(length(files) == 1)
+   
+   x <- read.csv(
+     files[1],
+     stringsAsFactors = FALSE,
+     check.names = FALSE
+   )
+   
+   names(x) <- toupper(trimws(names(x)))
+   
+   code_col <- paste0(domain, "TESTCD")
+   value_col <- paste0(domain, "STRESN")
+   
+   stopifnot(code_col %in% names(x))
+   
+   actual <- toupper(trimws(as.character(x[[code_col]])))
+   
+   cat("\nDOMAIN:", domain, "\n")
+   cat("Rows:", nrow(x), "\n")
+   cat("Columns:", paste(names(x), collapse = ", "), "\n")
+   
+   result <- data.frame(
+     CODE = codes,
+     N_MATCHES = vapply(
+       codes,
+       function(z) sum(actual == z, na.rm = TRUE),
+       integer(1)
+     )
+   )
+   
+   print(result)
+   
+   for (code in codes) {
+     idx <- which(actual == code)
+     
+     if (length(idx) > 0) {
+       keep <- intersect(
+         c("USUBJID", "VISIT", "VISITNUM",
+           code_col, value_col),
+         names(x)
+       )
+       
+       cat("\nExample:", code, "\n")
+       print(utils::head(x[idx, keep, drop = FALSE], 3))
+     }
+   }
+ }
> 
> check_mapping(
+   "MI",
+   c("ARCHCH", "CHRIINF", "NEUTLP", "NEUTEP",
+     "CRYPTDES", "ULCER", "NANCYHI")
+ )

DOMAIN: MI 
Rows: 6111 
Columns: STUDYID, DOMAIN, USUBJID, MISEQ, MIREFID, MISPID, MITESTCD, MITEST, MITSTDTL, MICAT, MIORRES, MISTRESC, MISTRESN, MISTAT, MIREASND, MINAM, MISPEC, MISPCCND, MILOC, MIMETHOD, MIBLFL, VISITNUM, VISIT, VISITDY, EPOCH, MIDTC, MIDY 
             CODE N_MATCHES
ARCHCH     ARCHCH       679
CHRIINF   CHRIINF       679
NEUTLP     NEUTLP       679
NEUTEP     NEUTEP       679
CRYPTDES CRYPTDES       679
ULCER       ULCER       679
NANCYHI   NANCYHI       679

Example: ARCHCH 
                 USUBJID     VISIT VISITNUM MITESTCD MISTRESN
1 CNTO1959UCO2002-100001 SCREENING     1000   ARCHCH        2
2 CNTO1959UCO2002-100001   WEEK 12     2012   ARCHCH        1
3 CNTO1959UCO2002-100001   WEEK 38     2038   ARCHCH        2

Example: CHRIINF 
                 USUBJID     VISIT VISITNUM MITESTCD MISTRESN
7 CNTO1959UCO2002-100001 SCREENING     1000  CHRIINF        2
8 CNTO1959UCO2002-100001   WEEK 12     2012  CHRIINF        0
9 CNTO1959UCO2002-100001   WEEK 38     2038  CHRIINF        0

Example: NEUTLP 
                  USUBJID     VISIT VISITNUM MITESTCD MISTRESN
22 CNTO1959UCO2002-100001 SCREENING     1000   NEUTLP        1
23 CNTO1959UCO2002-100001   WEEK 12     2012   NEUTLP        0
24 CNTO1959UCO2002-100001   WEEK 38     2038   NEUTLP        0

Example: NEUTEP 
                  USUBJID     VISIT VISITNUM MITESTCD MISTRESN
19 CNTO1959UCO2002-100001 SCREENING     1000   NEUTEP        1
20 CNTO1959UCO2002-100001   WEEK 12     2012   NEUTEP        0
21 CNTO1959UCO2002-100001   WEEK 38     2038   NEUTEP        0

Example: CRYPTDES 
                  USUBJID     VISIT VISITNUM MITESTCD MISTRESN
10 CNTO1959UCO2002-100001 SCREENING     1000 CRYPTDES        0
11 CNTO1959UCO2002-100001   WEEK 12     2012 CRYPTDES        0
12 CNTO1959UCO2002-100001   WEEK 38     2038 CRYPTDES        0

Example: ULCER 
                  USUBJID     VISIT VISITNUM MITESTCD MISTRESN
25 CNTO1959UCO2002-100001 SCREENING     1000    ULCER        1
26 CNTO1959UCO2002-100001   WEEK 12     2012    ULCER        0
27 CNTO1959UCO2002-100001   WEEK 38     2038    ULCER        0

Example: NANCYHI 
                  USUBJID     VISIT VISITNUM MITESTCD MISTRESN
16 CNTO1959UCO2002-100001 SCREENING     1000  NANCYHI        2
17 CNTO1959UCO2002-100001   WEEK 12     2012  NANCYHI        1
18 CNTO1959UCO2002-100001   WEEK 38     2038  NANCYHI        0
> 
> check_mapping("LB", "EOS")

DOMAIN: LB 
Rows: 116496 
Columns: STUDYID, DOMAIN, USUBJID, LBSEQ, LBSPID, LBTESTCD, LBTEST, LBCAT, LBORRES, LBORRESU, LBORNRLO, LBORNRHI, LBSTRESC, LBSTRESN, LBSTRESU, LBSTNRLO, LBSTNRHI, LBSTNRC, LBNRIND, LBSTAT, LBREASND, LBNAM, LBSPEC, LBSPCCND, LBMETHOD, LBBLFL, VISITNUM, VISIT, VISITDY, EPOCH, LBDTC, LBDY 
    CODE N_MATCHES
EOS  EOS      3811

Example: EOS 
                   USUBJID     VISIT VISITNUM LBTESTCD LBSTRESN
342 CNTO1959UCO2002-100001 SCREENING     1000      EOS     0.55
343 CNTO1959UCO2002-100001    WEEK 0     2000      EOS     0.49
344 CNTO1959UCO2002-100001    WEEK 2     2002      EOS     0.19
