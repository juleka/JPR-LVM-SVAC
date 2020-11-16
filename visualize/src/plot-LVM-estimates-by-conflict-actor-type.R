## Authors:      Ju
## Maintainers:  Ju
##
## ---------------------------------
## Purpose: plot LVM estimates, by conflict and actor type, 
##          Figure 7(a-d) main article, Figure 5(a-h) online appendix
## ---------------------------------

library(argparse)
library(yaml)

parser <- ArgumentParser()
parser$add_argument("--inputfile", type="character")
parser$add_argument("--CONSTANTS", type="character")
parser$add_argument("--out_log", type="character")
arguments <- parser$parse_args()

sink(arguments$out_log)
CONSTANTS <- yaml.load_file(arguments$CONSTANTS)

data <- read.csv(arguments$inputfile, header=TRUE, sep = '|', stringsAsFactors = FALSE)

# only visualize for select conflicts
data <- data[data$conflictid_new %in% CONSTANTS$conflicts_to_visualize, ]

# iterating over data to generate MAIN ARTICLE FIGURE 7(a-d), ONLINE APPENDIX FIGURE 5(a-h)
for (i in unique(data$conflictid_new)) {
  
  print(paste('Now working on conflict id: ', unique(data[data$conflictid_new==i, 'country']), i))
  cdata <- data[data$conflictid_new==i, ]
  
  for (at in unique(cdata$actor_type)) {
    
    catdata <- cdata[cdata$actor_type==at, ]
    xmin <- min(catdata$year)
    xmax <- max(catdata$year)
    
    ymax <- round(max(catdata$theta_upper_static, catdata$theta_upper_dynamic, na.rm = TRUE), digits = 0)
    ymin <- round(min(catdata$theta_low_static, catdata$theta_low_dynamic, na.rm = TRUE), digits = 0)
    #print(paste(ymin,ymax))
    country <- unique(catdata[catdata$conflictid_new==i, 'conflict_country'])
    country <- gsub('\\(', '', country)
    country <- gsub('\\)', '', country)
    country <- gsub("\\.", "", country)
    country <- gsub(",", "", country)
    country <- gsub("'", "-", country)
    country <- gsub(' ', '-', country)
    
    actor <- toupper(substring(unique(catdata$actor), 1,3))
    outputfile_name <- paste('output/pp-', actor, '-estimates-', country, '-', i, '.pdf', sep='')
    pdf(outputfile_name, width=8, height=4)
    
    par(las=1,  mar=c(2,4.5,1.5,8), xpd=TRUE, ps=20)
    print('set plotting parameters')
    # print(paste(unique(pdata$conflict_country),unique(pdata$conflictid_new)))
    plot(NULL, type='n', xaxt='n', yaxt='n', bty='n', xlab='', ylab='Latent prevalence',
         xlim=c(xmin, xmax), ylim = c(-3,4), 
         main="")#main=paste(unique(pdata$country),unique(pdata$conflictid_new))
    print('empty plot created')
    axis(1, at = seq(xmin, xmax, 1))
    #print("x-axis set")
    axis(2, at = seq(-3, 4, by = 1))
    #print("y-axis set")
    abline(h=0, lty=3, xpd=FALSE)

    if (nrow(catdata)<=5) {
      jitter <- 1*1/70
    } else {
      jitter <- 1*1/10
    }
      
    points(as.numeric(catdata$year)-jitter, catdata$theta_static, col='gray', pch=16)
    arrows(as.numeric(catdata$year)-jitter, catdata$theta_low_static, 
           as.numeric(catdata$year)-jitter, catdata$theta_upper_stati, length=0, angle=90, 
               col='gray', lwd=2.5)
    points(as.numeric(catdata$year)+jitter, catdata$theta_dynamic, col='red', pch=16)
    arrows(as.numeric(catdata$year)+jitter, catdata$theta_low_dynamic, 
           as.numeric(catdata$year)+jitter, catdata$theta_upper_dynamic, length=0, angle=90, 
           col='red', lwd=2.5)
        
    ##switch below depending on which actor you want legend for
    if (actor=='REB') {
    #if (actor=='GOV') {
        legend('topright', inset=c(-.3,0), 
             c('Static', 'Dynamic'),
             col=c('gray', 'red'),
             pch=c(16,16), lty = c(1, 1), lwd=c(2.5, 2.5),
             bty='n'#,  cex=1.5 #cex=.75#
             )
    }
    dev.off()
    print('done plotting')
  }
}
    
#end of script.