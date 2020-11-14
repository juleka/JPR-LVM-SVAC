# Authors:     Ju
# Maintainers: Ju
#
# Purpose: read in SVAC version 2.0 data
# ============================================

library(argparse)

parser <- ArgumentParser()
parser$add_argument("--inputfile", type="character")
parser$add_argument("--outputfile", type="character")
arguments <- parser$parse_args()

data <- read.csv(arguments$inputfile, header=TRUE, sep='|', stringsAsFactors = FALSE)
print(str(data))
## ---------------------------
## a little bit of data cleaning
## ---------------------------

## set missing data in prevalence vars to NA
prev_vars <- c('ai_prev', 'hrw_prev', 'state_prev')

for (i in prev_vars) {
  print(table(data[[i]], useNA='always'))
  data[[i]] <- ifelse(data[[i]]==-99, NA, data[[i]])
  print(table(data[[i]], useNA='always'))
  stopifnot(nrow(data[data[[i]]==-99 & !is.na(data[[i]]),])==0)
}

## rename location variable to country
data$country <- data$location
data$location <- NULL

#for LVM data, reduce observations to state and rebels
data <- data[data$actor_type==1 | data$actor_type==3,]
stopifnot(unique(sort(data$actor_type))==c(1,3))

print(str(data))

write.table(data, arguments$outputfile, row.names=FALSE, sep='|', quote=FALSE)
print(paste("done printing data to", arguments$outputfile))

#end.