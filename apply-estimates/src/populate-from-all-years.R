# Authors:     Ju
# Maintainers: Ju
#
# Purpose: populate missing observations in regression data from all_years LVM estimates
# =========================================================

library(argparse)
library(yaml)

parser    <- ArgumentParser()
parser$add_argument("--svac", type='character')
parser$add_argument("--all_years", type='character')
parser$add_argument("--all_years_static", type='character')
parser$add_argument("--all_years_dynamic", type='character')
parser$add_argument("--lag_func", type='character')
parser$add_argument("--CONSTANTS", type='character')
parser$add_argument("--outputfile", type='character')
arguments <- parser$parse_args()

CONSTANTS <- yaml.load_file(arguments$CONSTANTS)
source(arguments$lag_func)
svac <- read.csv(arguments$svac, header = TRUE, sep = '|', stringsAsFactors = FALSE)
all_years <- read.csv(arguments$all_years, header = TRUE, sep = '|', stringsAsFactors = FALSE)

for (i in 1:nrow(all_years)) {
  all_years[i, 'max_prev'] <- max(all_years[i, c('ai_prev', 'hrw_prev', 'state_prev')], na.rm = TRUE)
}
all_years$max_prev <- ifelse(all_years$max_prev==-Inf, NA, all_years$max_prev)
print(table(all_years$max_prev, useNA = 'always'))

#lag prev_vecs in all_years
SV_vectors <- c('ai_prev', 'hrw_prev', 'state_prev', 'max_prev')

print('creating lagged dependent variables')
for (sv_vec in SV_vectors) {
  all_years <- create_prev_dep_var_lag(all_years, sv_vec)
}

lag_prev_vecs <- c('lag_ai_prev', 'lag_hrw_prev', 'lag_state_prev', 'lag_max_prev')

for (lag_vec in lag_prev_vecs) {
  svac <- populate_values_for_missing_years(fill_data=svac, lag_vec=lag_vec, pop_data=all_years)
}

## populate from all years static/dynamic
populate_from_estimates <- function(model.name) {
  ## model.name = static/dynamic
  inputname <- paste('all_years', model.name, sep = '_')
  print(paste("Now working on", arguments[[inputname]]))
  data <- read.csv(arguments[[inputname]], header = TRUE, sep = '|', stringsAsFactors = FALSE)
  theta_model_name <- paste('theta', model.name, sep = '_')
  data[[theta_model_name]] <- data[['theta']]
  theta_sd_model_name <- paste('theta_sd', model.name, sep = '_')
  data[[theta_sd_model_name]] <- data[['theta_sd']]
  data <- create_prev_dep_var_lag(data, theta_model_name)
  data <- create_prev_dep_var_lag(data, theta_sd_model_name)
  lag_theta_model_name <- paste('lag', theta_model_name, sep='_')
  lag_theta_sd_model_name <- paste('lag', theta_sd_model_name, sep='_')
  svac <- populate_values_for_missing_years(fill_data=svac, lag_vec=lag_theta_model_name, pop_data=data)
  svac <- populate_values_for_missing_years(fill_data=svac, lag_vec=lag_theta_sd_model_name, pop_data=data)
  return(svac)
}

svac <- populate_from_estimates('static')
svac <- populate_from_estimates('dynamic')

stopifnot(nrow(svac)==CONSTANTS$N_total_obs)
write.table(svac, file = arguments$outputfile, row.names = FALSE, sep = '|')
print(paste('Wrote populated data to', arguments$outputfile))
#end of script.