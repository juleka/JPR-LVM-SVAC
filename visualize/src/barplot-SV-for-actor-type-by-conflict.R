# Authors:     Ju
# Maintainers: Ju
#
# Purpose: visualize SV prevalence over time, by state and source, for SVAC data 
#          (Figure 2(a-d) main article, Figure 2(a-h) online appendix)
# ===============================================================================

library(argparse)
library(yaml)

parser <- ArgumentParser()
parser$add_argument("--inputfile", type="character")
parser$add_argument("--CONSTANTS", type="character")
parser$add_argument("--outputfile", type="character")
arguments <- parser$parse_args()

CONSTANTS <- yaml.load_file(arguments$CONSTANTS)

sink(arguments$outputfile)
data <- read.csv(arguments$inputfile, header = TRUE, sep = "|", stringsAsFactors = FALSE)
stopifnot(length(unique(data$actor_type))==2)

## ----------
##  plotting
## ----------

prev_vec <- CONSTANTS$prev_vecs
prev_colors <- c(CONSTANTS$prev_colors[[1]], CONSTANTS$prev_colors[[2]], CONSTANTS$prev_colors[[3]])

barplot_observed_values <- function(data) {
  
    for (ct in unique(data$conflict_type)) {
      
      if (ct == 'intra') {
        
        print(paste('Working on conflict type:', ct))
        
        for (i in unique(data[data$conflict_type==ct, 'conflictid_new'])) {
          
          for (j in unique(data[data$conflictid_new==i, 'actor_type'])) {
            
            subdata <- data[data$conflictid_new==i & data$actor_type==j,]
            subdata <- subdata[order(subdata$year),]

            if(nrow(subdata)>1) {
              
              country.name <- unique(subdata$conflict_country)
              print(paste('Now working on:', country.name, i, j))
            
              ##get ready for plotting
              title.main <- paste(country.name, j)
              title.main <- gsub("-", " ", title.main)
              title.main <- gsub(",", " ", title.main)
              title.main <- gsub("  ", " ", title.main)
              title.main <- unlist(strsplit(title.main, split=" "))
              title.main <- paste(as.character(unique(title.main)), collapse=" ")
            
              ##clean up country names a bit to improve file names
              country.name <- gsub("\\(", "", country.name)
              country.name <- gsub("\\)", "", country.name)
              country.name <- gsub("\\.", "", country.name)
              country.name <- gsub(",", "", country.name)
              country.name <- gsub("'", "-", country.name)
              country.name <- gsub(" ", "-", country.name)
            
              if (j==1) {
                outputfile.name <- paste('output/bp-GOV-obs-', country.name, '-', i, '.pdf', sep='')
              } 
              if (j==3) {
                outputfile.name <- paste('output/bp-REB-obs-', country.name, '-', i, '.pdf', sep='')
              }
            
              print(subdata)
              pdf(outputfile.name, width=8, height=4)
              par(las=1, mar=c(4,5,1,5), xpd=TRUE, ps=16)
              barplot(t(as.matrix(subdata[,c(prev_vec)])), beside=TRUE, space=c(0,2),
                      names.arg = subdata$year, col=prev_colors, 
                      ylab='Reported prevalence', xlab='', ylim=c(0,3), yaxt='n', las=2)
              axis(2, at=seq(0,3,1), labels = seq(0,3,1))
              if (j==3) {##set position of legend here 1: GOV, 3: REB
                  legend("topright", inset=c(-.15,0),
                     legend = c('AI', 'HRW', 'USSD'), fill = prev_colors, bty='n')
              }
              dev.off()
              print(paste('Wrote graph to', outputfile.name))
            }
          }
        }
      }
      
      if (ct == 'inter') {
        
        print(paste('Working on conflict type:', ct))
        
        for (i in unique(data[data$conflict_type==ct, 'conflictid_new'])) {
          
          for (j in unique(data[data$conflictid_new==i, 'actor'])) {
            
            subdata <- data[data$conflictid_new==i & data$actor==j,]
            subdata <- subdata[order(subdata$year),]

            if(nrow(subdata)>0) {
              
              country.name <- unique(subdata$conflict_country)
              
              ##get ready for plotting
              title.main <- paste(country.name, j)
              title.main <- gsub("-", " ", title.main)
              title.main <- gsub(",", " ", title.main)
              title.main <- gsub("  ", " ", title.main)
              title.main <- unlist(strsplit(title.main, split=" "))
              title.main <- paste(as.character(unique(title.main)), collapse=" ")
              
              ##clean up country names a bit to improve file names
              country.name <- gsub("\\(", "", country.name)
              country.name <- gsub("\\)", "", country.name)
              country.name <- gsub("\\.", "", country.name)
              country.name <- gsub(",", "", country.name)
              country.name <- gsub("'", "-", country.name)
              country.name <- gsub(" ", "-", country.name)
              
              outputfile.name <- paste('output/bp-INT-obs-', gsub(" ", "-", j), '-', i, '.pdf', sep='')
              
              pdf(outputfile.name, width=8, height=4)
              par(las=1, mar=c(4,4,1.5,5), xpd=TRUE)
              barplot(t(as.matrix(subdata[,c(prev_vec)])), beside=TRUE, space=c(0,2),
                      names.arg = subdata$year, col=prev_colors, 
                      ylab='Reported prevalence', xlab='', axes=FALSE, las=2)
              axis(2, at=c(0,1,2,3), labels=c(0,1,2,3))
              legend("topright", inset=c(-.15,0),
                      legend = c('AI', 'HRW', 'USSD'), fill = prev_colors, bty='n')
              dev.off()
              print(paste('Wrote graph to', outputfile.name))
            }
          }
        }
      }
    }
}

select_conflicts <- data[data$conflictid_new %in% CONSTANTS$conflicts_to_visualize, ]

## MAIN ARTICLE Figure 2(a)-(d), ONLINE APPENDIX: Figure 2(a)-(h)
barplot_observed_values(select_conflicts)

print("FINISHED RUNNING src/barplot-for-actor-type-by-conflict.R")
##end of script.