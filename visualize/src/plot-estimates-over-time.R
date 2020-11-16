## Authors:      Ju
## Maintainers:  Ju
##
## ------------------------------------------------------------
## Purpose: plot distribution of estimates over time (Main article: Figure 4)
## ------------------------------------------------------------

library(argparse)

parser <- ArgumentParser()
parser$add_argument("--inputfile", type="character")
parser$add_argument("--outputfile", type="character")
arguments <- parser$parse_args()

data <- read.csv(arguments$inputfile, 
                 header=TRUE, sep='|', stringsAsFactors = FALSE)

year_labels <- c("1989", "", "1991", "", "1993", "", "1995", 
                 "", "1997", "", "1999", "", "2001", "",
                 "2003", "", "2005", "", "2007", "", "2009",
                 "", "2011", "", "2013", "", "2015")

## MAIN ARTICLE: FIGURE 4
pdf(arguments$outputfile, width=17, height=6)
par(las=1, mar=c(2,5,1,1), ps = 20)
boxplot(theta_static ~ year, data=data,
        boxwex = 0.25, at = 1989:2015 - 0.15, col = 'gray', 
        # main = "", 
        xlab = "", ylab = "Estimated latent prevalence",
        ylim = c(-1, 4), yaxs ="i", 
        xaxt='n',
        yaxt='n'
)

boxplot(theta_dynamic ~ year, data=data,
        boxwex  = 0.25, at = 1989:2015 + 0.15, col = "red",
        add = TRUE, 
        xaxt='n')

axis(1, at = c(1989:2015), labels=year_labels)
abline(h=0, lty=3, xpd=FALSE)

legend('topleft', inset=c(-.00001,0.03),
       c("Static", "Dynamic"),
       fill = c('gray', 'red'),
       bty='n', y.intersp=1.5
)

dev.off()
#end of script.