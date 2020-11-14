## Authors:      Ju
## Maintainers:  Ju
##
## ---------------------------------
## Purpose: aggregate non-state conflict-actor-year observations
## ---------------------------------

library(argparse)
library(yaml)

parser    <- ArgumentParser()
parser$add_argument("--inputfile", type='character')
parser$add_argument("--CONSTANTS", type='character')
parser$add_argument("--agg_func", type='character')
parser$add_argument("--outputfile", type='character')
parser$add_argument("--all_years", type='character')
arguments <- parser$parse_args()

CONSTANTS <- yaml.load_file(arguments$CONSTANTS)
source(arguments$agg_func)
data <- read.csv(arguments$inputfile, header=TRUE, sep='|', stringsAsFactors = FALSE)

## in the LVM estimates, we are only looking at actual conflict years
## but for the applied regression model, we need to preserve some info for all years to avoid missing years
all_years <- data
conf_years <- data[data$conflictyear==1,]

##for each intrastate or internationalized conflict, aggregate state and non-state actors such that:
## we do NOT aggregate GOV conflict-year observations,
## we get one conflict-actor-type (GOV/REB) observation per year for INTRAstate conflicts, 
## we keep the maximum prevalence value for each conflict-actor-year observation
## (all aggregation functions are in a separate R file)

print("aggregating for ALL YEARS")
all_years <- aggregate_conflicts(all_years)
print("aggregating for CONFLICT YEARS")
conf_years <- aggregate_conflicts(conf_years)

stopifnot(nrow(conf_years)==CONSTANTS$N_total_obs)

write.table(conf_years, file=arguments$outputfile, sep='|', row.names = FALSE)
write.table(all_years, file=arguments$all_years, sep='|', row.names = FALSE)
#end of script.