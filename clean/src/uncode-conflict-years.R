# Authors:     Ju
# Maintainers: Ju
#
# Purpose: recode SVAC conflict years as "not a conflict year" if they are not listed 
#           as conflict years in the UCDP/PRIO data
# ============================================

library(argparse)
parser <- ArgumentParser()
parser$add_argument("--inputfile", type="character")
parser$add_argument("--recode_conflict_yrs", type="character")
parser$add_argument("--outputfile", type="character")
arguments <- parser$parse_args()

data <- read.csv(arguments$inputfile, header=TRUE, sep='|', stringsAsFactors = FALSE)

#recode SVAC conflict years, if not in UCDP/PRIO set
not_conflict_years <- read.csv(arguments$recode_conflict_yrs, header=TRUE, sep = '|', stringsAsFactors = FALSE)
not_conflict_years$conflict_country <- NULL
not_conflict_years$recode_conflictyear <- 1
recode.check <- nrow(not_conflict_years)
svac.cy.check <- nrow(data$conflictyear==1)

data <- merge(data, not_conflict_years, by=c('conflictid_new', 'year'), all.x = TRUE)
data$conflictyear <- ifelse(data$recode_conflictyear==1 & !is.na(data$recode_conflictyear), 0, data$conflictyear)
stopifnot(svac.cy.check - nrow(data$conflictyear==1) == recode.check)
data$recode_conflictyear <- NULL
print(table(data$conflictyear, useNA="always"))

write.table(data, arguments$outputfile, row.names=FALSE, sep='|', quote=FALSE)
print(paste("done printing data to", arguments$outputfile))
# end of Rscript.