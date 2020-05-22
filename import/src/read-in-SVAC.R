# Authors:     Ju
# Maintainers: Ju
# Copyright:   2018, Ju
#
# Purpose: read in SVAC version 2.0 data
# ============================================

library(argparse)

parser <- ArgumentParser()
parser$add_argument("--input", type="character")
parser$add_argument("--fix_conflict_yrs", type="character")
parser$add_argument("--output", type="character")
arguments <- parser$parse_args()

data <- read.csv(arguments$input, header=TRUE, sep='|', stringsAsFactors = FALSE)

## ---------------------------
## some data cleaning
## ---------------------------

## set missing data in prevalence vars to NA
prev_vars <- c('ai_prev', 'hrw_prev', 'state_prev')

for (i in prev_vars) {
  data[[i]] <- ifelse(data[[i]]==-99, NA, data[[i]])
  print(table(data[[i]], useNA='always'))
}

## rename location variable to country
data$country <- data$location
data$location <- NULL

#need to clean up country.names a bit
table(data$gwnoloc, useNA = 'always')
length(unique(data$gwnoloc))
length(unique(data$gwnoloc2))
length(unique(data$gwnoloc3))
length(unique(data$gwnoloc4))

#for LVM data, reduce observations to state and rebels
data <- data[data$actor_type==1 | data$actor_type==3,]
print(table(data$actor_type, useNA = 'always'))

#uncode years as conflict, if not in UCDP/PRIO set of conflict years
not_conflict_years <- read.csv(arguments$fix_conflict_yrs, header=TRUE, sep = '|', stringsAsFactors = FALSE)
not_conflict_years$conflict_country <- NULL
not_conflict_years$recode_conflictyear <- 1
recode.check <- nrow(not_conflict_years)
svac.cy.check <- nrow(data$conflictyear==1)
data <- merge(data, not_conflict_years, by=c('conflictid_new', 'year'), all.x = TRUE)
data$conflictyear <- ifelse(data$recode_conflictyear==1 & !is.na(data$recode_conflictyear), 0, data$conflictyear)
stopifnot(svac.cy.check - nrow(data$conflictyear==1) == recode.check)
data$recode_conflictyear <- NULL

print("DONE WITH DATA CLEANING")

print(paste('Final nrow(data) is:', nrow(data)))
str(data)

write.table(data, arguments$output, row.names=FALSE, sep='|', quote=FALSE)
print(paste("done printing data to", arguments$output))

#end.