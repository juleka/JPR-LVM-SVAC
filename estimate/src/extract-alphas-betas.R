## Authors:      Ju
## Maintainers:  Ju
##
## ---------------------------------
## Purpose: extract alpha and beta params from fit
## ---------------------------------

##extract list of betas (for x-table later on), for each indicator
state_beta       <- mean(extracted_fit$beta[,1])
state_beta_upper <- quantile(extracted_fit$beta[,1], 0.975)
state_beta_lower <- quantile(extracted_fit$beta[,1], 0.025)

ai_beta       <- mean(extracted_fit$beta[,2])
ai_beta_upper <- quantile(extracted_fit$beta[,2], 0.975)
ai_beta_lower <- quantile(extracted_fit$beta[,2], 0.025)

hrw_beta       <- mean(extracted_fit$beta[,3])
hrw_beta_upper <- quantile(extracted_fit$beta[,3], 0.975)
hrw_beta_lower <- quantile(extracted_fit$beta[,3], 0.025)

##extract list of alphas (for x-table later on), for each indicator

#raw versions
state_alpha1       <- mean(extracted_fit[['c_state']][,1])
state_alpha1_upper <- quantile(extracted_fit[['c_state']][,1], 0.975)
state_alpha1_lower <- quantile(extracted_fit[['c_state']][,1], 0.025)

state_alpha2       <- mean(extracted_fit[['c_state']][,2])
state_alpha2_upper <- quantile(extracted_fit[['c_state']][,2], 0.975)
state_alpha2_lower <- quantile(extracted_fit[['c_state']][,2], 0.025)

state_alpha3       <- mean(extracted_fit[['c_state']][,3])
state_alpha3_upper <- quantile(extracted_fit[['c_state']][,3], 0.975)
state_alpha3_lower <- quantile(extracted_fit[['c_state']][,3], 0.025)

ai_alpha1       <- mean(extracted_fit[['c_ai']][,1])
ai_alpha1_upper <- quantile(extracted_fit[['c_ai']][,1], 0.975)
ai_alpha1_lower <- quantile(extracted_fit[['c_ai']][,1], 0.025)

ai_alpha2       <- mean(extracted_fit[['c_ai']][,2])
ai_alpha2_upper <- quantile(extracted_fit[['c_ai']][,2], 0.975)
ai_alpha2_lower <- quantile(extracted_fit[['c_ai']][,2], 0.025)

ai_alpha3       <- mean(extracted_fit[['c_ai']][,3])
ai_alpha3_upper <- quantile(extracted_fit[['c_ai']][,3], 0.975)
ai_alpha3_lower <- quantile(extracted_fit[['c_ai']][,3], 0.025)

hrw_alpha1       <- mean(extracted_fit[['c_hrw']][,1])
hrw_alpha1_upper <- quantile(extracted_fit[['c_hrw']][,1], 0.975)
hrw_alpha1_lower <- quantile(extracted_fit[['c_hrw']][,1], 0.025)

hrw_alpha2       <- mean(extracted_fit[['c_hrw']][,2])
hrw_alpha2_upper <- quantile(extracted_fit[['c_hrw']][,2], 0.975)
hrw_alpha2_lower <- quantile(extracted_fit[['c_hrw']][,2], 0.025)

hrw_alpha3       <- mean(extracted_fit[['c_hrw']][,3])
hrw_alpha3_upper <- quantile(extracted_fit[['c_hrw']][,3], 0.975)
hrw_alpha3_lower <- quantile(extracted_fit[['c_hrw']][,3], 0.025)

## scale alphas
sc_state_alpha1       <- mean(extracted_fit[['c_state']][,1]/extracted_fit$beta[,1])
sc_state_alpha1_upper <- quantile(extracted_fit[['c_state']][,1]/extracted_fit$beta[,1], 0.975)
sc_state_alpha1_lower <- quantile(extracted_fit[['c_state']][,1]/extracted_fit$beta[,1], 0.025)

sc_state_alpha2       <- mean(extracted_fit[['c_state']][,2]/extracted_fit$beta[,1])
sc_state_alpha2_upper <- quantile(extracted_fit[['c_state']][,2]/extracted_fit$beta[,1], 0.975)
sc_state_alpha2_lower <- quantile(extracted_fit[['c_state']][,2]/extracted_fit$beta[,1], 0.025)

sc_state_alpha3       <- mean(extracted_fit[['c_state']][,3]/extracted_fit$beta[,1])
sc_state_alpha3_upper <- quantile(extracted_fit[['c_state']][,3]/extracted_fit$beta[,1], 0.975)
sc_state_alpha3_lower <- quantile(extracted_fit[['c_state']][,3]/extracted_fit$beta[,1], 0.025)

sc_ai_alpha1       <- mean(extracted_fit[['c_ai']][,1]/extracted_fit$beta[,2])
sc_ai_alpha1_upper <- quantile(extracted_fit[['c_ai']][,1]/extracted_fit$beta[,2], 0.975)
sc_ai_alpha1_lower <- quantile(extracted_fit[['c_ai']][,1]/extracted_fit$beta[,2], 0.025)

sc_ai_alpha2       <- mean(extracted_fit[['c_ai']][,2]/extracted_fit$beta[,2])
sc_ai_alpha2_upper <- quantile(extracted_fit[['c_ai']][,2]/extracted_fit$beta[,2], 0.975)
sc_ai_alpha2_lower <- quantile(extracted_fit[['c_ai']][,2]/extracted_fit$beta[,2], 0.025)

sc_ai_alpha3       <- mean(extracted_fit[['c_ai']][,3]/extracted_fit$beta[,2])
sc_ai_alpha3_upper <- quantile(extracted_fit[['c_ai']][,3]/extracted_fit$beta[,2], 0.975)
sc_ai_alpha3_lower <- quantile(extracted_fit[['c_ai']][,3]/extracted_fit$beta[,2], 0.025)

sc_hrw_alpha1       <- mean(extracted_fit[['c_hrw']][,1]/extracted_fit$beta[,3])
sc_hrw_alpha1_upper <- quantile(extracted_fit[['c_hrw']][,1]/extracted_fit$beta[,3], 0.975)
sc_hrw_alpha1_lower <- quantile(extracted_fit[['c_hrw']][,1]/extracted_fit$beta[,3], 0.025)

sc_hrw_alpha2       <- mean(extracted_fit[['c_hrw']][,2]/extracted_fit$beta[,3])
sc_hrw_alpha2_upper <- quantile(extracted_fit[['c_hrw']][,2]/extracted_fit$beta[,3], 0.975)
sc_hrw_alpha2_lower <- quantile(extracted_fit[['c_hrw']][,2]/extracted_fit$beta[,3], 0.025)

sc_hrw_alpha3       <- mean(extracted_fit[['c_hrw']][,3]/extracted_fit$beta[,3])
sc_hrw_alpha3_upper <- quantile(extracted_fit[['c_hrw']][,3]/extracted_fit$beta[,3], 0.975)
sc_hrw_alpha3_lower <- quantile(extracted_fit[['c_hrw']][,3]/extracted_fit$beta[,3], 0.025)

list_params <- list(state_beta = state_beta, state_beta_upper = state_beta_upper, state_beta_lower = state_beta_lower,
                      ai_beta = ai_beta, ai_beta_upper = ai_beta_upper, ai_beta_lower = ai_beta_lower,
                      hrw_beta = hrw_beta, hrw_beta_upper = hrw_beta_upper, hrw_beta_lower = hrw_beta_lower,
                      state_alpha1 = state_alpha1, state_alpha1_upper = state_alpha1_upper, state_alpha1_lower = state_alpha1_lower,
                      state_alpha2 = state_alpha2, state_alpha2_upper = state_alpha2_upper, state_alpha2_lower = state_alpha2_lower,
                      state_alpha3 = state_alpha3, state_alpha3_upper = state_alpha3_upper, state_alpha3_lower = state_alpha3_lower,
                      ai_alpha1 = ai_alpha1, ai_alpha1_upper = ai_alpha1_upper, ai_alpha1_lower = ai_alpha1_lower,
                      ai_alpha2 = ai_alpha2, ai_alpha2_upper = ai_alpha2_upper, ai_alpha2_lower = ai_alpha2_lower,
                      ai_alpha3 = ai_alpha3, ai_alpha3_upper = ai_alpha3_upper, ai_alpha3_lower = ai_alpha3_lower,
                      hrw_alpha1 = hrw_alpha1, hrw_alpha1_upper = hrw_alpha1_upper, hrw_alpha1_lower = hrw_alpha1_lower,
                      hrw_alpha2 = hrw_alpha2, hrw_alpha2_upper = hrw_alpha2_upper, hrw_alpha2_lower = hrw_alpha2_lower,
                      hrw_alpha3 = hrw_alpha3, hrw_alpha3_upper = hrw_alpha3_upper, hrw_alpha3_lower = hrw_alpha3_lower,
                      sc_state_alpha1 = sc_state_alpha1, sc_state_alpha1_upper = sc_state_alpha1_upper, sc_state_alpha1_lower = sc_state_alpha1_lower,
                      sc_state_alpha2 = sc_state_alpha2, sc_state_alpha2_upper = sc_state_alpha2_upper, sc_state_alpha2_lower = sc_state_alpha2_lower,
                      sc_state_alpha3 = sc_state_alpha3, sc_state_alpha3_upper = sc_state_alpha3_upper, sc_state_alpha3_lower = sc_state_alpha3_lower,
                      sc_ai_alpha1 = sc_ai_alpha1, sc_ai_alpha1_upper = sc_ai_alpha1_upper, sc_ai_alpha1_lower = sc_ai_alpha1_lower,
                      sc_ai_alpha2 = sc_ai_alpha2, sc_ai_alpha2_upper = sc_ai_alpha2_upper, sc_ai_alpha2_lower = sc_ai_alpha2_lower,
                      sc_ai_alpha3 = sc_ai_alpha3, sc_ai_alpha3_upper = sc_ai_alpha3_upper, sc_ai_alpha3_lower = sc_ai_alpha3_lower,
                      sc_hrw_alpha1 = sc_hrw_alpha1, sc_hrw_alpha1_upper = sc_hrw_alpha1_upper, sc_hrw_alpha1_lower = sc_hrw_alpha1_lower,
                      sc_hrw_alpha2 = sc_hrw_alpha2, sc_hrw_alpha2_upper = sc_hrw_alpha2_upper, sc_hrw_alpha2_lower = sc_hrw_alpha2_lower,
                      sc_hrw_alpha3 = sc_hrw_alpha3, sc_hrw_alpha3_upper = sc_hrw_alpha3_upper, sc_hrw_alpha3_lower = sc_hrw_alpha3_lower)

return(list_params)
#done with extracting params.