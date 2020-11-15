# Authors:     Ju
# Maintainers: Ju
#
# Purpose: merge static and dynamic estimates into one file
# ================================================================

library(argparse)
library(yaml)

parser    <- ArgumentParser()
parser$add_argument("--est_static", type='character')
parser$add_argument("--est_dynamic", type='character')
parser$add_argument("--CONSTANTS", type='character')
parser$add_argument("--outputfile", type='character')
arguments <- parser$parse_args()

CONSTANTS <- yaml.load_file(arguments$CONSTANTS)

static  <- read.csv(arguments$est_static, header = TRUE, sep = '|', stringsAsFactors = FALSE)
dynamic <- read.csv(arguments$est_dynamic, header = TRUE, sep = '|', stringsAsFactors = FALSE)

specify_theta_colnames <- function(data) {
  col_index <- grep('theta', colnames(data))
  colnames(data)[col_index] <- paste(colnames(data)[col_index], deparse(substitute(data)), sep = '_')
  return(data)
}

static <- specify_theta_colnames(static)
dynamic <- specify_theta_colnames(dynamic)

svac <- merge(static, dynamic[, c(which(colnames(dynamic)=="year"),
                                  which(colnames(dynamic)=='conflictid_new'), 
                                  which(colnames(dynamic)=='actor'), 
                                  grep('theta', colnames(dynamic)))], 
              by=c('year', 'conflictid_new', 'actor'), all = TRUE)
svac <- svac[order(svac$conflictid_new, svac$actor, svac$year), ]
stopifnot(nrow(svac)==CONSTANTS$N_total_obs)

stopifnot(nrow(svac[duplicated(svac[, c('conflict_country', 'year', 'conflictid_new', 'actor')])==TRUE,])==0)
print(paste('There are', 
            nrow(svac[duplicated(svac[, c('conflict_country', 'year', 'conflictid_new', 'actor')])==TRUE,]), 
            'rows of duplicate records.'))

print(head(svac))
write.table(svac, arguments$outputfile, row.names = FALSE, sep = '|')
#end of script.