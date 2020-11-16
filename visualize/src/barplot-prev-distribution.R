# Authors:     Ju
# Maintainers: Ju
#
# Purpose: barplot general distribution of prev vectors (MAIN ARTICLE: FIGURE 1(a))
# ======================================================

library(argparse)
library(yaml)
library(reshape2)

parser    <- ArgumentParser()
parser$add_argument("--inputfile", type='character')
parser$add_argument("--CONSTANTS", type='character')
parser$add_argument("--out_plot", type='character')
arguments <- parser$parse_args()

CONSTANTS <- yaml.load_file(arguments$CONSTANTS)

data <- read.csv(arguments$inputfile, header = TRUE, sep = '|', stringsAsFactors = FALSE)
data$cc <- 1

prev_vecs <- CONSTANTS$prev_vecs
prev_colors <- c(CONSTANTS$prev_colors[[1]], CONSTANTS$prev_colors[[2]], CONSTANTS$prev_colors[[3]])

sumdist <- data.frame(Category = as.character(), total = as.integer(), 
                      perc = as.integer(), Source=as.character(),
                      stringsAsFactors = FALSE)

for (vec in prev_vecs) {
  print(vec)
  sdata <- data.frame(table(data[[vec]]))
  colnames(sdata) <- c('Category', 'total')
  sdata[['perc']] <- round(sdata$total/sum(sdata$total)*100, digits=0)
  sdata[['Source']] <- vec
  print(sdata)
  sumdist <- rbind(sumdist, sdata)
}

org_perc <- acast(sumdist, Category~Source, value.var="perc")

## MAIN ARTICLE: FIGURE 1(a)
pdf(arguments$out_plot, width = 6, height = 4)
par(las=1, mar=c(4,5,1,6), ps=20)
barplot(t(org_perc),  beside=TRUE, 
        width = .25, space=c(0,.5),
        col=prev_colors, 
        ylab = "Percent", #xlab = "Reported prevalence level",
        ylim = c(0,100),
        legend.text=c('AI', 'HRW', 'USSD'),
        args.legend=list(
          x='right',
          bty = "n", inset=c(-.3,0), xjust=0, y.intersp=1.5,
          title = "Source"
        ))
dev.off()
#end of script.