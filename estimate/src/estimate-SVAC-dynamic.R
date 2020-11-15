## Authors:      Ju
## Maintainers:  Ju, CJF
##
## ---------------------------------
## Purpose: calculate dynamic model
## ---------------------------------

library(argparse)
library(yaml)
library(rstan)
library(parallel)
library(loo)

rm(list=ls())

parser    <- ArgumentParser()
parser$add_argument("--inputfile", type='character')
parser$add_argument("--CONSTANTS", type='character')
parser$add_argument("--STANcode", type='character')
parser$add_argument("--model_fit_functions", type='character')
parser$add_argument("--extract_parameters", type='character')
parser$add_argument("--dynamic_params", type='character')
parser$add_argument("--dynamic_waic", type='character')
parser$add_argument("--outputfile", type='character')
arguments <- parser$parse_args()

source(arguments$model_fit_functions)
CONSTANTS <- yaml.load_file(arguments$CONSTANTS)

df <- read.csv(arguments$inputfile, sep="|", stringsAsFactors = FALSE)

## the order of the obs in the dataset matters for when the panel_index_lag is set
## --> the order is set in clean/src/aggregation-functions.R

panel_index_lag <- 0
for(ii in 2:nrow(df)){
    if(df$year[ii] - df$year[ii-1]==1){
        panel_index_lag[ii] <- panel_index_lag[ii-1] + 1
    }
    else{
        panel_index_lag[ii] <- 0
    }
}

## state_prev ai_prev hrw_prev
state <- df$state_prev + 1
ai    <- df$ai_prev + 1
hrw   <- df$hrw_prev + 1

index_all   <- 1:nrow(df)
index_state <- index_all[!is.na(state)]
index_ai    <- index_all[!is.na(ai)]
index_hrw   <- index_all[!is.na(hrw)]

state <- state[!is.na(state)]
ai    <- ai[!is.na(ai)]
hrw   <- hrw[!is.na(hrw)]

n_state <- length(state)
n_ai    <- length(ai)
n_hrw   <- length(hrw)
n_all   <- nrow(df)

stan.data <- list(
    n_state = n_state,
    n_ai = n_ai,
    n_hrw = n_hrw,
    n_all = n_all,

    index_state = index_state,
    index_ai = index_ai,
    index_hrw = index_hrw,

    state = state,
    ai = ai,
    hrw = hrw,

    panel_index_lag = panel_index_lag
)

dynamic.stan.fit <- stan(
    file=arguments$STANcode,
    data=stan.data,
    iter=CONSTANTS$dynamic_STAN_iter,
    chains=CONSTANTS$dynamic_STAN_chains,
    cores=parallel::detectCores(logical = FALSE)
)

log_lik_dyn <- extract_log_lik(dynamic.stan.fit, merge_chains=F)
waic_dynamic <- waic(log_lik_dyn)
save(waic_dynamic, file=arguments$dynamic_waic)
print('saved dynamic waic estimates')

## ONLINE APPENDIX: FIGURE 4 b
make_Rhat_plot(dynamic.stan.fit, 'dynamic')
print('rhat plot generated')

extracted_fit <- extract(dynamic.stan.fit)

dynamic_params <- source(arguments$extract_parameters)
save(dynamic_params, file = arguments$dynamic_params)
print('saved dynamic alpha and beta parameters')

## MAIN ARTICLE: FIGURE 6 DYNAMIC d: AI, e: HRW, f: STATE
plot_cutpoints_by_source(extracted_fit, 'dynamic')
print('all cutpoints plotted')

df$theta       <- apply(extracted_fit$theta, 2, mean)
df$theta_sd    <- apply(extracted_fit$theta, 2, sd)
df$theta_upper <- apply(extracted_fit$theta, 2, quantile, 0.975)
df$theta_low   <- apply(extracted_fit$theta, 2, quantile, 0.025)

write.table(df, arguments$outputfile, sep='|', row.names = FALSE)
#end of script.