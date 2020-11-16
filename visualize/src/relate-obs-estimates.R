# Authors:     Ju
# Maintainers: Ju
#
# Purpose: calculate relationship between prev vars and latent estimates, Spearman rank, 1,000 draws from posterior
#          Online Appendix Table II
# ==========================================================================

library(argparse)
library(xtable)

parser    <- ArgumentParser()
parser$add_argument("--inputfile", type='character')
parser$add_argument("--corr_function", type='character')
parser$add_argument("--xt_out_corr_obs_est", type='character')
arguments <- parser$parse_args()

source(arguments$corr_function)
data <- read.csv(arguments$inputfile, header=TRUE, sep='|', stringsAsFactors = FALSE)

## calculate correlations also separately, by actor
GOV <- data[data$actor_type==1,]
REB <- data[data$actor_type==3,]

all_ai_static     <- correlate_obs_estimates(data, 'ai_prev', 'static')
all_ai_dynamic    <- correlate_obs_estimates(data, 'ai_prev', 'dynamic')
all_hrw_static    <- correlate_obs_estimates(data, 'hrw_prev', 'static')
all_hrw_dynamic   <- correlate_obs_estimates(data, 'hrw_prev', 'dynamic')
all_state_static  <- correlate_obs_estimates(data, 'state_prev', 'static')
all_state_dynamic <- correlate_obs_estimates(data, 'state_prev', 'dynamic')

gov_ai_static     <- correlate_obs_estimates(GOV, 'ai_prev', 'static')
gov_ai_dynamic    <- correlate_obs_estimates(GOV, 'ai_prev', 'dynamic')
gov_hrw_static    <- correlate_obs_estimates(GOV, 'hrw_prev', 'static')
gov_hrw_dynamic   <- correlate_obs_estimates(GOV, 'hrw_prev', 'dynamic')
gov_state_static  <- correlate_obs_estimates(GOV, 'state_prev', 'static')
gov_state_dynamic <- correlate_obs_estimates(GOV, 'state_prev', 'dynamic')

reb_ai_static     <- correlate_obs_estimates(REB, 'ai_prev', 'static')
reb_ai_dynamic    <- correlate_obs_estimates(REB, 'ai_prev', 'dynamic')
reb_hrw_static    <- correlate_obs_estimates(REB, 'hrw_prev', 'static')
reb_hrw_dynamic   <- correlate_obs_estimates(REB, 'hrw_prev', 'dynamic')
reb_state_static  <- correlate_obs_estimates(REB, 'state_prev', 'static')
reb_state_dynamic <- correlate_obs_estimates(REB, 'state_prev', 'dynamic')

## --------------------------------------------
##  make xtable of Spearman rank correlations
## --------------------------------------------

corr.tab <- data.frame(corr.name=c("AI x static", "HRW x static", "USSD x static", 
                                   "AI x dynamic", "HRW x dynamic", "USSD x dynamic"), 
                       all_coefs=c(all_ai_static$corr_coef, all_hrw_static$corr_coef, all_state_static$corr_coef,
                                   all_ai_dynamic$corr_coef, all_hrw_dynamic$corr_coef, all_state_dynamic$corr_coef), 
                       all_pvals=c(all_ai_static$corr_p_val, all_hrw_static$corr_p_val, all_state_static$corr_p_val,
                                   all_ai_dynamic$corr_p_val, all_hrw_dynamic$corr_p_val, all_state_dynamic$corr_p_val),
                       gov_coefs=c(gov_ai_static$corr_coef, gov_hrw_static$corr_coef, gov_state_static$corr_coef,
                                    gov_ai_dynamic$corr_coef, gov_hrw_dynamic$corr_coef, gov_state_dynamic$corr_coef), 
                       gov_pvals=c(gov_ai_static$corr_p_val, gov_hrw_static$corr_p_val, gov_state_static$corr_p_val,
                                   gov_ai_dynamic$corr_p_val, gov_hrw_dynamic$corr_p_val, gov_state_dynamic$corr_p_val),
                       reb_coefs=c(reb_ai_static$corr_coef, reb_hrw_static$corr_coef, reb_state_static$corr_coef,
                                   reb_ai_dynamic$corr_coef, reb_hrw_dynamic$corr_coef, reb_state_dynamic$corr_coef), 
                       reb_pvals=c(reb_ai_static$corr_p_val, reb_hrw_static$corr_p_val, reb_state_static$corr_p_val,
                                   reb_ai_dynamic$corr_p_val, reb_hrw_dynamic$corr_p_val, reb_state_dynamic$corr_p_val))

# corr.tab <- corr.tab[order(corr.tab$all_coefs, decreasing = TRUE),]
xt.spearman.obs.est <- xtable(corr.tab, digits=3, caption="Spearman rank correlation coefficients (with p-values) 
                              for all combinations of SVAC prevalence indicators with the static and dynamic latent 
                              variable estimates, respectively, 
                              and by dataset. We compute correlations for the entire SVAC dataset, and then separately  for  
                              government and rebel group observations.
                              The Spearman rank correlations account for the uncertainty in the estimates of latent prevalence.
                              To this aim, we take 1,000 draws from the posterior distributions of the static and dynamic estimates to obtain
                              the empirical values for computing 1,000 correlations for each examined relationship. Here, we report
                              the average  correlation coefficients and p-values across each set of 1,000 samples.",
                              label='xt-corr-spearman-obs-estimates')
names(xt.spearman.obs.est) <- c("Correlation between", "All SVAC", "p-value", 
                                "Governments", "p-value", "Rebels", "p-value")
xt.spearman.obs.est

## ONLINE APPENDIX TABLE II
print(xt.spearman.obs.est, file=arguments$xt_out_corr_obs_est,
      table.placement='h', caption.placement='top',
      include.rownames=FALSE, hline.after=c(-1,0,3,6))

#end of script.