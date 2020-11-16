# Authors:     Ju
# Maintainers: Ju
#
# Purpose: Spearman rank correlation function, 1,000 draws from posterior distribution
# ==========================================================================

correlate_obs_estimates <- function(corr_data, prev_vec, est_type) {
  ## corr_data = data used for correlation
  ## prev_vec  = observed data vector
  ## est_type   = static/dynamic
  
  # a list to save datasets
  newdata <- list()
  
  draw_name         <- paste('draw', est_type, sep = '_')
  theta_name        <- paste('theta', est_type, sep = '_')
  theta_sd_name     <- paste('theta_sd', est_type, sep = '_')
  
  for(i in 1:1000) {
    newdata[[i]]                  <- corr_data
    newdata[[i]][[draw_name]] <- rnorm(cbind(rep(1,nrow(corr_data))), 
                                       mean=corr_data[[theta_name]], sd=corr_data[[theta_sd_name]])
  }
  
  # define a formula as a character string:
  cor_FML <- paste('~', prev_vec, '+', draw_name)
  print(paste('Now working on correlation between:', cor_FML))
  
  milm <- function(fml, midata) {
    corr_coef <- list()
    corr_p_val <- list()
    
    for(i in 1:length(midata)){
      corr_obj  <- cor.test(formula = as.formula(cor_FML), data = midata[[i]], method='spearman', exact=FALSE)
      corr_coef <- corr_obj$estimate
      corr_p_value <- corr_obj$p.value 
      
      corr_coef[[i]] <- corr_obj$estimate
      corr_p_val[[i]] <- corr_obj$p.value 
    }
    corr_coef <- mean(unlist(corr_coef), na.rm = TRUE)
    corr_p_val <- mean(unlist(corr_p_val), na.rm = TRUE)
    list("corr_coef" = corr_coef, "corr_p_val" = corr_p_val)    
  }
  corr_results <- milm(fml=FML, midata=newdata)
  return(corr_results)
}
#end of script.