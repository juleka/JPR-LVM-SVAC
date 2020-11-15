# Authors:     Ju
# Maintainers: Ju
#
# Purpose: create new variabless from the existing data (max_prev, lags)
# =======================================================

library(argparse)
library(yaml)

parser    <- ArgumentParser()
parser$add_argument("--inputfile", type='character')
parser$add_argument("--CONSTANTS", type='character')
parser$add_argument("--dep_var_func", type='character')
parser$add_argument("--outputfile", type='character')
arguments <- parser$parse_args()

CONSTANTS <- yaml.load_file(arguments$CONSTANTS)
source(arguments$dep_var_func)
data <- read.csv(arguments$inputfile, header = TRUE, sep = '|', stringsAsFactors = FALSE)

# set observed prevalence variables to zero if NA
for (pvec in CONSTANTS$prev_vecs) {
  data[[pvec]] <- ifelse(is.na(data[[pvec]]), 0, data[[pvec]])
  print(pvec)
  print(table(data[[pvec]], useNA = 'always'))
}

# compute maximum observed prevalence value across the three human rights indicators
for (i in 1:nrow(data)) {
  data[i, 'max_prev'] <- max(data[i, c('ai_prev', 'hrw_prev', 'state_prev')], na.rm = TRUE)
}
data$max_prev <- ifelse(data$max_prev==-Inf, NA, data$max_prev)
print('max_prev')
print(table(data$max_prev, useNA = 'always'))

# lag vars for prevalence SV at t-1
SV_vectors <- c('ai_prev', 'hrw_prev',  'state_prev', 'max_prev', 
                'theta_static', 'theta_sd_static', 
                'theta_dynamic', 'theta_sd_dynamic')
print('creating lagged dependent variables')
for (sv_vec in SV_vectors) {
  data <- create_prev_dep_var_lag(data, sv_vec)
}

print(str(data))
write.table(data, file = arguments$outputfile, row.names = FALSE, sep = '|')
print(paste('Wrote data to', arguments$outputfile))
#end of script.