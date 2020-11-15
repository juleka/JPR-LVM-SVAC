## Authors:      Ju
## Maintainers:  Ju
##
## --------------------------------------------
## Purpose: a collection of modelling functions
## --------------------------------------------

## This function generates the plots for FIGURE 4 in the ONLINE APPENDIX 
make_Rhat_plot <- function(stan_data_out, model_name) {
  
    output_file_name <- paste('output/gg-rhat-SVAC-', model_name, '.pdf', sep='')
    print(paste('plotting rhats to' , output_file_name))
  
    stan_rhat(stan_data_out, fill='grey', bins=30)
    ggsave(file=output_file_name, height=8, width=6)
}


## This function generates the plots for FIGURE 6 in the MAIN ARTICLE 
plot_cutpoints_by_source <- function(extracted_fit, model_name) {
  
  ## model_name: static, dynamic, static-all-years, dynamic-all-years

  ## for the beta cols:
  # state is 1
  # ai is 2
  # hrw is 3
  
  source_names <- c('state', 'ai', 'hrw')
  prev_colors <- c(CONSTANTS$prev_colors[[3]], CONSTANTS$prev_colors[[1]], CONSTANTS$prev_colors[[2]])
  
  
  for (sname in source_names) { 
    
    print(paste('Now working on cutpoint plot for:', sname))
    
    c_vec   <- paste('c', sname, sep='_')
  
    cut_point_obj  <- data.frame(category=rep(1, length(extracted_fit[[c_vec]][,1])), 
                                value= (extracted_fit[[c_vec]][,1] / 
                                          extracted_fit[['beta']][ , which(source_names==sname)]))
    cut_point_obj  <- rbind(cut_point_obj, data.frame(category=rep(2, length(extracted_fit[[c_vec]][,2])), 
                                                      value=(extracted_fit[[c_vec]][,2] / 
                                                               extracted_fit[['beta']][ , which(source_names==sname)])))
    cut_point_obj  <- rbind(cut_point_obj, data.frame(category=rep(3, length(extracted_fit[[c_vec]][,3])), 
                                                     value=(extracted_fit[[c_vec]][,3] / 
                                                               extracted_fit[['beta']][ , which(source_names==sname)])))
  
    outputfilename <- paste('output/bp-cp-', model_name, '-', sname, '.pdf', sep='')
  
    print(paste('Plotting cut points to', outputfilename))
    pdf(outputfilename, height=8, width=8)
    par(las=1, mar=c(4,5,1,1), ps=20)
    boxplot(value~category, data=cut_point_obj, xlab='Cut-point', ylab='Estimated latent prevalence',
            ylim = c(0,5),
            col=prev_colors[which(source_names==sname)])
    dev.off()
    }
}

##end of script.