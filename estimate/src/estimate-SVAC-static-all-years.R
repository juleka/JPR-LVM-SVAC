## Authors:      Ju
## Maintainers:  Ju
##
## ---------------------------------
## Purpose: calculate static model for all years
## ---------------------------------

library(argparse)
library(yaml)
library(rstan)
library(parallel)

rm(list=ls())

parser    <- ArgumentParser()
parser$add_argument("--inputfile", type='character')
parser$add_argument("--CONSTANTS", type='character')
parser$add_argument("--STANcode", type='character')
parser$add_argument("--model_fit_functions", type='character')
parser$add_argument("--outputfile", type='character')
arguments <- parser$parse_args()

source(arguments$model_fit_functions)
CONSTANTS <- yaml.load_file(arguments$CONSTANTS)

df <- read.csv(arguments$inputfile, sep="|", stringsAsFactors = FALSE)
str(df)

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
    n_ai    = n_ai,
    n_hrw   = n_hrw,
    n_all   = n_all,

    index_state = index_state,
    index_ai    = index_ai,
    index_hrw   = index_hrw,

    state = state,
    ai    = ai,
    hrw   = hrw
)

static.stan.fit <- stan(
    file=arguments$STANcode,
    data=stan.data,
    iter=CONSTANTS$static_STAN_iter,
    chains=CONSTANTS$static_STAN_chains,
    cores=parallel::detectCores(logical = FALSE)
)

make_Rhat_plot(static.stan.fit, 'static-all-years')
print('rhat plot generated')

static_extract <- extract(static.stan.fit)

plot_cutpoints_by_source(static_extract, 'static-all-years')
print('all cutpoints plotted')

df$theta       <- apply(static_extract$theta, 2, mean)
df$theta_sd    <- apply(static_extract$theta, 2, sd)
df$theta_upper <- apply(static_extract$theta, 2, quantile, 0.975)
df$theta_low   <- apply(static_extract$theta, 2, quantile, 0.025)

write.table(df, arguments$outputfile, sep="|", row.names = FALSE)
## end of script.