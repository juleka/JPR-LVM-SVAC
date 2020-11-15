## Authors:      Ju
## Maintainers:  Ju, CJF
##
## ---------------------------------
## Purpose: calculate static model, all conflict years
## ---------------------------------

## https://cran.r-project.org/web/packages/loo/vignettes/loo2-with-rstan.html

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
parser$add_argument("--static_params", type='character')
parser$add_argument("--static_waic", type='character')
parser$add_argument("--outputfile", type='character')
arguments <- parser$parse_args()

source(arguments$model_fit_functions)
CONSTANTS <- yaml.load_file(arguments$CONSTANTS)

df <- read.csv(arguments$inputfile, sep="|", stringsAsFactors = FALSE)
stopifnot(nrow(df)==CONSTANTS$N_total_obs)

## adding +1 because STAN and JAGS want categorical data to start at 1 
state <- df$state_prev + 1
ai    <- df$ai_prev + 1
hrw   <- df$hrw_prev + 1

## JAGS and STAN deal w missing data differently:
##  we want to keep track of the missing data by setting them relative to the full index; 
##  i.e., we keep track of the NA positions in original data index but also get rid of them
index_all   <- 1:nrow(df)
index_state <- index_all[!is.na(state)]
index_ai    <- index_all[!is.na(ai)]
index_hrw   <- index_all[!is.na(hrw)]

## make sure these vars of observed values are same length as the indices; 
##   these are the vectors entering the models
state <- state[!is.na(state)]
ai    <- ai[!is.na(ai)]
hrw   <- hrw[!is.na(hrw)]

## make scalars, the total number of observed values; in STAN, we declare the length 
##  of each of the vectors, so program knows how long they are before entering the model
n_state <- length(state)
n_ai    <- length(ai)
n_hrw   <- length(hrw)
n_all   <- nrow(df)

## create a list of all things created so far, will be sending this to STAN
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

#create fitted object to pass into Stan
static.stan.fit <- stan(
    file=arguments$STANcode,
    data=stan.data,
    ## iter: number of times simulations will happen, the first half (1K) are used for 
    ##   burning in the model, the second half (1K) are used for inference, first 1K get 
    ##   thrown out, the second 1K get saved. To know you have enough iterations, calculate
    ##   the R-hat statistic (Gelman-Rubin statistic) --> want it to be as close to 1.1 as 
    ##   possible to ensure that model has converged
    iter=CONSTANTS$static_STAN_iter,
    ## chains: number of independent simulations happening in parallel at the same time, one sim on
    ##   each processor if possible
    chains=CONSTANTS$static_STAN_chains,
    cores=parallel::detectCores(logical = FALSE)
)

#Stan passes simulated values back to R, as a list. Takes all the chains in separate arrays
##  parses them back together, calculates R-hat statistic, i.e., info summary

log_lik_stat <- extract_log_lik(static.stan.fit, merge_chains=F)
waic_static <- waic(log_lik_stat)
save(waic_static, file=arguments$static_waic)
print('saved static waic estimates')

## make rhat plot
## ONLINE APPENDIX: FIGURE 4 a
make_Rhat_plot(static.stan.fit, 'static')
print('rhat plot generated')

extracted_fit <- extract(static.stan.fit)

static_params <- source(arguments$extract_parameters)
save(static_params, file = arguments$static_params)
print('saved static alpha and beta parameters')

##  plot cut points for each source
## MAIN ARTICLE: FIGURE 6 STATIC a: AI, b: HRW, c: STATE
plot_cutpoints_by_source(extracted_fit, 'static')
print('all cutpoints plotted')

## the extracted fit is a list of names, we can call parameters from that list
#theta is the latent variable, we want its mean, std, upper and lower bounds
df$theta       <- apply(extracted_fit$theta, 2, mean)
df$theta_sd    <- apply(extracted_fit$theta, 2, sd)
## compute credible intervals (CI)
df$theta_upper <- apply(extracted_fit$theta, 2, quantile, 0.975)
df$theta_low   <- apply(extracted_fit$theta, 2, quantile, 0.025)

#saving the LVM estimates
write.table(df, arguments$outputfile, sep="|", row.names = FALSE)
## end of script.