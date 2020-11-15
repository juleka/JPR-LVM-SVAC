# Authors:     Ju
# Maintainers: Ju
#
# Purpose: run simple ordinal logit models, used lagged var to predict dep var
# ==========================================================================

library(argparse)
library(yaml)
library(ordinal) #clm
library(texreg)

parser    <- ArgumentParser()
parser$add_argument("--inputfile", type='character')
parser$add_argument("--CONSTANTS", type='character')
parser$add_argument("--xt_ai", type='character')
parser$add_argument("--xt_hrw", type='character')
parser$add_argument("--xt_state", type='character')
parser$add_argument("--xt_max", type='character')
arguments <- parser$parse_args()

CONSTANTS <- yaml.load_file(arguments$CONSTANTS)

data <- read.csv(arguments$inputfile, header = TRUE, sep = '|', stringsAsFactors = FALSE)

prev_vecs <- c(CONSTANTS$prev_vecs, 'max_prev')

gof_names_list <- c("log-Likelihood", "Num.\ obs.", "Corr.\ obs x predicted", "Corr.\ p-value")

for (opv in prev_vecs) {
  data[[opv]] <- factor(data[[opv]], ordered=TRUE)
  lag_opv_name <- paste('lag', opv, sep='_')
  print(paste('Now working on:', lag_opv_name))
  data[[lag_opv_name]] <- as.numeric(data[[lag_opv_name]])
  print(table(data[[lag_opv_name]], useNA = 'always'))
}

regress_obs_values <- function(prev_vec) {
  lag_var_name <- paste("lag", prev_vec, sep = "_")
  olr_model <- clm(formula = as.formula(paste(prev_vec, lag_var_name, sep="~")),
                   data=data, link ='logit')
  print(summary(olr_model))
  corr_data <- data.frame(predicted_values = as.numeric(as.character(unlist(predict(olr_model, type = 'class')))),  
                          observed_values = as.numeric(as.character(olr_model$y)))
  corr_obj  <- cor.test(~ predicted_values + observed_values,
                        data = corr_data, method='spearman', exact=FALSE)
  corr_coef <- corr_obj$estimate
  corr_p_value <- corr_obj$p.value 
  tex_obj <- createTexreg(coef.names=dimnames(summary(olr_model)$coefficients)[[1]], 
                              coef= summary(olr_model)$coefficients[,1], 
                              se=summary(olr_model)$coefficients[,2], 
                              pvalues = summary(olr_model)$coefficients[,4],
                              gof.names = gof_names_list, 
                              gof = c(as.numeric(as.character(olr_model$info$logLik)), 
                                      olr_model$nobs, 
                                      corr_coef, corr_p_value),
                              gof.decimal = c(TRUE, FALSE, TRUE, TRUE), model.name = "olr prevalence")

  return(tex_obj)
}

regress_latent_values <- function(data, prev_vec, estimate_type) {
  
  set.seed(42)
  
  # estimate_type == 'static'/'dynamic'
  lag_prev_name <- paste('lag', prev_vec, sep='_')
  vecdata <- data[!is.na(data[[lag_prev_name]]),]

  # a list to save datasets
  newdata <- list()
  
  # take K draws from the posterior distribution and make K new datasets in the list object just created
  # the newdata object is a list with 1000 dataframes 
  # each dataframe contains a new column of draws from the distribution of each country year's latent variable.
  
  draw_name         <- paste('draw', estimate_type, sep = '_')
  theta_name        <- paste('theta', estimate_type, sep = '_')
  theta_sd_name     <- paste('theta_sd', estimate_type, sep = '_')
  lag_draw_name     <- paste('lag', draw_name, sep = '_')
  lag_theta_name    <- paste('lag', theta_name, sep = '_')
  lag_theta_sd_name <- paste('lag', theta_sd_name, sep = '_')
  
  for(i in 1:1000) {
    newdata[[i]]                  <- vecdata
    newdata[[i]][[lag_draw_name]] <- rnorm(cbind(rep(1,nrow(vecdata))), 
                                           mean=vecdata[[lag_theta_name]], sd=vecdata[[lag_theta_sd_name]])
  }

  # define a formula as a character string:
  FML <- paste(prev_vec, lag_draw_name, sep="~")
  print(FML)
  
  milm <- function(fml, midata) {
    xx <- terms(as.formula(fml))
    #the plus 3 is for the category cuts coeffs
    lms <- matrix(data=NA, nrow=(length(attr(xx, "term.labels")) + 3), ncol=length(midata))
    ses <- matrix(data=NA, nrow=(length(attr(xx, "term.labels")) + 3), ncol=length(midata))
    pvl <- matrix(data=NA, nrow=(length(attr(xx, "term.labels")) + 3), ncol=length(midata))
    vcovs <- list()
    log_lik <- list()
    n_obs <- list()
    corr_coef <- list()
    corr_p_val <- list()
    
    for(i in 1:length(midata)){
      tmp <- clm(formula=as.formula(fml), data=midata[[i]], link = "logit")
      lms[,i] <- tmp$coefficients
      ses[,i] <- sqrt(diag(vcov(tmp)))
      pvl[,i] <- summary(tmp)$coefficients[, 4] #p-values
      vcovs[[i]] <- vcov(tmp)
      log_lik[[i]] <- tmp$logLik
      n_obs[[i]] <- tmp$nobs
      
      corr_data <- data.frame(predicted_values = as.numeric(as.character(unlist(predict(tmp, type = 'class')))),  
                              observed_values = as.numeric(as.character(tmp$y)))
      corr_data <- corr_data[!corr_data$observed_values==0,]

      corr_obj  <- cor.test(~ predicted_values + observed_values,
                            data = corr_data, method='spearman', exact=FALSE)
      
      corr_coef[[i]] <- corr_obj$estimate
      corr_p_val[[i]] <- corr_obj$p.value 
    }
    par.est <- apply(lms, 1, mean)
    se.within <- apply(ses, 1, mean)
    se.between <- apply(lms, 1, var)
    se.est <- sqrt(se.within^2 + se.between*(1 + (1/length(midata))))
    pvl.est <- apply(pvl, 1, mean)
    logLik <- mean(unlist(log_lik))
    n_obs <- unique(unlist(n_obs))
    corr_coef <- mean(unlist(corr_coef), na.rm = TRUE)
    corr_p_val <- mean(unlist(corr_p_val), na.rm = TRUE)
    list("terms"=names(tmp$coefficients), "beta" = par.est, "SE"=se.est, 
         "vcovs"=vcovs, "coefs" = lms, "significance" = pvl.est,
         "logLik" = logLik, "n_obs" = n_obs, "corr_coef" = corr_coef, "corr_p_val" = corr_p_val)    
  }
  clm_model <- milm(fml=FML, midata=newdata)
  tex_obj <- createTexreg(coef.names=clm_model$terms, coef= clm_model$beta, 
                        se=clm_model$SE, pvalues = clm_model$significance,
                        gof.names = gof_names_list, 
                        gof = c(clm_model$logLik, clm_model$n_obs, clm_model$corr_coef, clm_model$corr_p_val),
                        gof.decimal = c(TRUE, FALSE, TRUE, TRUE), model.name = "clm latent prevalence")
  return(tex_obj)
}

custom_model_names <- c("Observed Lag", "Static Latent Lag", "Dynamic Latent Lag")
custom_coef_list <- list("0|1" = "y>=1",
                         "1|2" = "y>=2",
                         "2|3" = "y>=3",
                         "y>=1" = "y>=1",
                         "y>=2" = "y>=2",
                         "y>=3" = "y>=3",
                         "lag_ai_prev" = "Observed AI, lag",
                         "lag_hrw_prev" = "Observed HRW, lag",
                         "lag_state_prev" = "Observed USSD, lag",
                         "lag_max_prev" = "Observed max value, lag",
                         "lag_draw_static" = "Static  latent, lag",
                         "lag_draw_dynamic" = "Dynamic latent, lag")

stars_list <- c(0.001, 0.01, 0.05, 0.1)

## ONLINE APPENDIX: Table III Amnesty International 
ai_obs <- regress_obs_values("ai_prev")
ai_static <- regress_latent_values(data, 'ai_prev', 'static')
ai_dynamic <- regress_latent_values(data, 'ai_prev', 'dynamic')
texreg(l=list(ai_obs, ai_static, ai_dynamic),
       file=arguments$xt_ai,
       stars = stars_list,
       custom.model.names = custom_model_names,
       custom.coef.map = custom_coef_list,
       custom.gof.names = gof_names_list,
       digits = 3,
       label = "xt-ai",
       caption = "\\emph{Amnesty International}: Ordinal logistic regression using the AI human rights 
       indicator as dependent variable. In the first model (``Observed Lag''), the predictor is a 1-year lag
       of the observed AI response variable. In the second model (``Static Latent Lag''), the predictor is a 1-year
       lag of the static estimates of SVAC prevalence, using a multiple imputation procedure that averages across 1,000 draws 
        from the posterior distribution to account for the statistical uncertainty in the LVM estimates. 
       In the third model (``Dynamic Latent Lag''), the predictor is a 1-yar lag of the dynamic latent 
       estimates of SVAC prevalence, also using a multiple imputation procedure that averages across 1,000 draws. To assess
       model performance, we compare the log-Likelihood across models, as well as, the Spearman rank
       correlation coefficients between the dependent variable and the predicted values of each regression
       model.",
       float.pos = "h"
)

## ONLINE APPENDIX: Table IV Human Rights Watch 
hrw_obs <- regress_obs_values("hrw_prev")
hrw_static <- regress_latent_values(data, 'hrw_prev', 'static')
hrw_dynamic <- regress_latent_values(data, 'hrw_prev', 'dynamic')
texreg(l=list(hrw_obs, hrw_static, hrw_dynamic),
       file=arguments$xt_hrw,
       stars = stars_list,
       custom.model.names = custom_model_names,
       custom.coef.map = custom_coef_list,
       custom.gof.names = gof_names_list,
       digits = 3,
       label = "xt-hrw",
       caption = "\\emph{Human Rights Watch}: Ordinal logistic regression using the HRW human rights 
       indicator as dependent variable. In the first model (``Observed Lag''), the predictor is a 1-year lag
       of the observed HRW response variable. In the second model (``Static Latent Lag''), the predictor is a 1-year
       lag of the static estimates of SVAC prevalence, using a multiple imputation procedure that averages across 1,000 draws
        from the posterior distribution to account for the statistical uncertainty in the LVM estimates. 
       In the third model (``Dynamic Latent Lag''), the predictor is a 1-yar lag of the dynamic latent 
       estimates of SVAC prevalence, also using a multiple imputation procedure that averages across 1,000 draws. To assess
       model performance, we compare the log-Likelihood across models, as well as, the Spearman rank
       correlation coefficients between the dependent variable and the predicted values of each regression
       model.",
       float.pos = "h"
)

## ONLINE APPENDIX: Table V U.S. State Department 
state_obs <- regress_obs_values("state_prev")
state_static <- regress_latent_values(data, 'state_prev', 'static')
state_dynamic <- regress_latent_values(data, 'state_prev', 'dynamic')
texreg(l=list(state_obs, state_static, state_dynamic),
       file=arguments$xt_state,
       stars = stars_list,
       custom.model.names = custom_model_names,
       custom.coef.map = custom_coef_list,
       custom.gof.names = gof_names_list,
       digits = 3,
       label = "xt-state",
       caption = "\\emph{U.S.\ State Department}: Ordinal logistic regression using the USSD human rights 
       indicator as dependent variable. In the first model (``Observed Lag''), the predictor is a 1-year lag
       of the observed USSD response variable. In the second model (``Static Latent Lag''), the predictor is a 1-year
       lag of the static estimates of SVAC prevalence, using a multiple imputation procedure that averages across 1,000 draws
        from the posterior distribution to account for the statistical uncertainty in the LVM estimates. 
       In the third model (``Dynamic Latent Lag''), the predictor is a 1-yar lag of the dynamic latent 
       estimates of SVAC prevalence, also using a multiple imputation procedure that averages across 1,000 draws. To assess
       model performance, we compare the log-Likelihood across models, as well as, the Spearman rank
       correlation coefficients between the dependent variable and the predicted values of each regression
       model.",
       float.pos = "h"
)

## ONLINE APPENDIX: Table VI Maximum Observed Value 
max_obs <- regress_obs_values("max_prev")
max_static <- regress_latent_values(data, 'max_prev', 'static')
max_dynamic <- regress_latent_values(data, 'max_prev', 'dynamic')
texreg(l=list(max_obs, max_static, max_dynamic),
       file=arguments$xt_max,
       stars = stars_list,
       custom.model.names = custom_model_names,
       custom.coef.map = custom_coef_list,
       custom.gof.names = gof_names_list,
       digits = 3,
       label = "xt-max",
       caption = "\\emph{Maximum observed value}: Ordinal logistic regression using the maximum observed
       value across the three human rights 
       indicators as dependent variable. In the first model (``Observed Lag''), the predictor is a 1-year lag
       of the observed maximum value response variable. In the second model (``Static Latent Lag''), the predictor is a 1-year
       lag of the static estimates of SVAC prevalence, using a multiple imputation procedure that averages across 1,000 draws
        from the posterior distribution to account for the statistical uncertainty in the LVM estimates. 
       In the third model (``Dynamic Latent Lag''), the predictor is a 1-yar lag of the dynamic latent 
       estimates of SVAC prevalence, also using a multiple imputation procedure that averages across 1,000 draws. To assess
       model performance, we compare the log-Likelihood across models, as well as, the Spearman rank
       correlation coefficients between the dependent variable and the predicted values of each regression
       model.",
       float.pos = "h"
)

#end of script.