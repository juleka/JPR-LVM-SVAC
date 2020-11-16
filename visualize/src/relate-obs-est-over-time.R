# Authors:     Ju
# Maintainers: Ju
#
# Purpose: calculate relationship between prev vars and latent estimates, Spearman rank, over time
#          Online Appendix Figures 6(a-b), 7(a-b), 8(a-b)
# ==========================================================================

library(argparse)
library(yaml)

parser    <- ArgumentParser()
parser$add_argument("--inputfile", type='character')
parser$add_argument("--corr_function", type='character')
parser$add_argument("--CONSTANTS", type='character')
parser$add_argument("--tl_corr_All_static", type='character')
parser$add_argument("--tl_corr_All_dynamic", type='character')
parser$add_argument("--tl_corr_GOV_static", type='character')
parser$add_argument("--tl_corr_GOV_dynamic", type='character')
parser$add_argument("--tl_corr_REB_static", type='character')
parser$add_argument("--tl_corr_REB_dynamic", type='character')
arguments <- parser$parse_args()

source(arguments$corr_function)
CONSTANTS <- yaml.load_file(arguments$CONSTANTS)

data <- read.csv(arguments$inputfile, header=TRUE, sep='|', stringsAsFactors = FALSE)
GOV <- data[data$actor_type==1,]
REB <- data[data$actor_type==3,]

stopifnot(nrow(data)==nrow(GOV)+nrow(REB))


data_list <- list(data, GOV, REB)
names(data_list) <- c('All', 'GOV', 'REB')
est_types <- list("static", "dynamic")
prev_vecs <- list("ai_prev", "hrw_prev", "state_prev")

corr.over.time <- setNames(data.frame(matrix(ncol = 6, nrow = 0)),
                           c("data", "year", "prev_vec", "est_type", "corr.coef", "p.value"))

for (idata in names(data_list)) {
  
  print(paste("Now working on data type:", idata))
  
  for (iest in est_types) {
    
    print(paste("Now working on estimate type:", iest))
    
    for (ivec in prev_vecs) {
      
      print(paste("Now working on HR indicator:", ivec))
      
      for (iyear in min(data$year):max(data$year)) {
        
        print(paste("Now working on year:", iyear))
        
        corr_data <- data_list[[idata]]

        corr_results <- correlate_obs_estimates(corr_data[corr_data$year==iyear,], ivec, iest)
        
        if (corr_results$corr_p_val>0.05 & !is.na(corr_results$corr_p_val)) {
          corr_results$corr_coef <- NA
          corr_results$corr_p_val <- "corr. not sig."
        }

        corr.over.time <- rbind(corr.over.time, data.frame(data=idata, year=iyear, 
                                                           prev_vec=ivec, est_type=iest, 
                                                           corr.coef=corr_results$corr_coef, 
                                                           p.value=corr_results$corr_p_val))
        }
    }
  }
}

print(corr.over.time)

plot_corr_obs_est_over_time <- function(corr_data, idata_type, iest_type) {
  
  corr_data <- corr_data[corr_data$data==idata_type & corr_data$est_type==iest_type,]
  print(corr_data)
  
  outputfile_name <- paste('tl_corr', idata_type, iest_type, sep='_')
  print(outputfile_name)
  
  xlab <- ''
  ylab <- "Correlation coefficient"
  
  pdf(arguments[[outputfile_name]], width=15, height=4)
  par(las=1, mar=c(3,5,2,6) + 0.1, xpd=TRUE, ps=20)
  plot(corr_data[corr_data$prev_vec=='ai_prev', 'year'], 
       corr_data[corr_data$prev_vec=='ai_prev', 'corr.coef'], 
       xlab = xlab, ylab = "", ylim=c(-.2,1),
       type = 'l', main = "", bty='n', col = CONSTANTS$prev_colors[1], lty=1, lwd=3)
  title(ylab = ylab, line = 3.75)
  lines(corr_data[corr_data$prev_vec=='hrw_prev', 'year'], 
        corr_data[corr_data$prev_vec=='hrw_prev', 'corr.coef'], 
        col=rgb(1,1,0,1), lty=1, lwd=3)
  lines(corr_data[corr_data$prev_vec=='state_prev', 'year'], 
        corr_data[corr_data$prev_vec=='state_prev', 'corr.coef'], 
        col=CONSTANTS$prev_colors[3], lty=1, lwd=3)
  abline(h=0, lty=3, xpd=FALSE)
  legend('topright', inset = c(-.085,-0.15), y.intersp = 1.5,
         c("AI", "HRW", "USSD"),
         lty = c(1, 1, 1),
         col = c(CONSTANTS$prev_colors[1], rgb(1,1,0,1), CONSTANTS$prev_colors[3]),
         lwd = c(3, 3, 3), bty = 'n')
  dev.off()
}

plot_corr_obs_est_over_time(corr.over.time, 'All', 'static')  ## online appendix Figure 6(a)
plot_corr_obs_est_over_time(corr.over.time, 'All', 'dynamic') ## online appendix Figure 6(b)

plot_corr_obs_est_over_time(corr.over.time, 'GOV', 'static')  ## online appendix Figure 7(a)
plot_corr_obs_est_over_time(corr.over.time, 'GOV', 'dynamic') ## online appendix Figure 7(b)

plot_corr_obs_est_over_time(corr.over.time, 'REB', 'static')  ## online appendix Figure 8(a)
plot_corr_obs_est_over_time(corr.over.time, 'REB', 'dynamic') ## online appendix Figure 8(b)
#end of script.