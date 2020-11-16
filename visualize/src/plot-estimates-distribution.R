## Authors:      Ju
## Maintainers:  Ju
##
## ------------------------------------------------------------
## Purpose: plot distribution of estimates against observed values, main article Figure 5(a-c)
## ------------------------------------------------------------

library(argparse)
library(yaml)

parser <- ArgumentParser()
parser$add_argument("--inputfile", type="character")
parser$add_argument("--CONSTANTS", type="character")
parser$add_argument("--outputlog", type="character")
arguments <- parser$parse_args()

sink(arguments$outputlog)

CONSTANTS <- yaml.load_file(arguments$CONSTANTS)
data <- read.csv(arguments$inputfile, header=TRUE, sep='|', stringsAsFactors = FALSE)

for (vec in CONSTANTS$prev_vecs) {
  
  print(paste('Now working on plotting estimates for:', vec))
  
  ## iterating over MAIN ARTICLE: FIGURE 5(a-c)
  outputfile_name <- paste('output/box-estimates-', vec, '.pdf', sep='')
  print(outputfile_name)
  
  pdf(outputfile_name)
  par(las=1, mar=c(4.5,5,1,1), ps=26)
  
  static_formula <- as.formula(paste('theta_static', vec, sep='~'))

  boxplot(formula = static_formula, data=data, 
          boxwex = 0.25, at = 0:3 - 0.15, col = 'gray', 
          # main = "", 
          xlab = "Reported prevalence", ylab = "",
          ylim = c(-1, 4), yaxs ="i", 
          xaxt='n', yaxt='n'
          )

  dynamic_formula <- as.formula(paste('theta_dynamic', vec, sep='~'))
  
  boxplot(formula = dynamic_formula, data=data, 
          boxwex  = 0.25, at = 0:3 + 0.15, col = "red",
          add = TRUE, xaxt='n')
  
  axis(1, at = c(0:3))
  
  if (vec == 'ai_prev') {
    title(ylab  = "Estimated latent prevalence")
  } 
  
  legend('bottomright', inset=c(-.00001,0.03),
           c("Static", "Dynamic"),
           fill = c('gray', 'red'),
           bty='n', y.intersp=1.5
           )
    
  dev.off()
  
  print(paste('Done with graphing boxplot.'))
  
}

#end of script.