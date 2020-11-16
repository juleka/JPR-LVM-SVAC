# Authors:     Ju
# Maintainers: Ju
#
# Purpose: calculate relationship between prev vars, Cohen's weighted kappa
#          Figure 3 and Table I main article, Figure 3(a-b) online appendix
# ==========================================================================

library(argparse)
library(xtable)
library(irr)

parser <- ArgumentParser()
parser$add_argument("--inputfile", type="character")
parser$add_argument("--xt_out_kappa", type="character")
parser$add_argument("--tl_irr_all", type="character")
parser$add_argument("--tl_irr_gov", type="character")
parser$add_argument("--tl_irr_reb", type="character")
arguments <- parser$parse_args()

data <- read.csv(arguments$inputfile, header = TRUE, sep = '|', stringsAsFactors = FALSE)

## calculate interrater agreement separately, by actor
GOV <- data[data$actor_type==1,]
REB <- data[data$actor_type==3,]

## --------------------------------------------
##   Cohen's kappa, weighted
## --------------------------------------------

#all data
all_kap_ai_hrw <- kappa2(data[,  c('ai_prev', 'hrw_prev')], weight = "equal")
all_kap_ai_state <- kappa2(data[,  c('ai_prev', 'state_prev')], weight = "equal")
all_kap_hrw_state <- kappa2(data[,  c('hrw_prev', 'state_prev')], weight = "equal")

# GOV
gov_kap_ai_hrw <- kappa2(GOV[,  c('ai_prev', 'hrw_prev')], weight = "equal")
gov_kap_ai_state <- kappa2(GOV[,  c('ai_prev', 'state_prev')], weight = "equal")
gov_kap_hrw_state <- kappa2(GOV[,  c('hrw_prev', 'state_prev')], weight = "equal")

# REB
reb_kap_ai_hrw <- kappa2(REB[,  c('ai_prev', 'hrw_prev')], weight = "equal")
reb_kap_ai_state <- kappa2(REB[,  c('ai_prev', 'state_prev')], weight = "equal")
reb_kap_hrw_state <- kappa2(REB[,  c('hrw_prev', 'state_prev')], weight = "equal")

## --------------------------------------------
##  make xtable of Cohen's weighted kappa
## --------------------------------------------

kappa.tab <- data.frame(corr.name=c("AI x HRW", "AI x USSD", "HRW x USSD"), 
                       all_coef=c(all_kap_ai_hrw$value, all_kap_ai_state$value, all_kap_hrw_state$value), 
                       all_pval=c(all_kap_ai_hrw$p.value, all_kap_ai_state$p.value, all_kap_hrw_state$p.value),
                       gov_coef=c(gov_kap_ai_hrw$value, gov_kap_ai_state$value, gov_kap_hrw_state$value), 
                       gov_kap_pval=c(gov_kap_ai_hrw$p.value, gov_kap_ai_state$p.value, gov_kap_hrw_state$p.value),
                       reb_coef=c(reb_kap_ai_hrw$value, reb_kap_ai_state$value, reb_kap_hrw_state$value), 
                       reb_pval=c(reb_kap_ai_hrw$p.value, reb_kap_ai_state$p.value, reb_kap_hrw_state$p.value))
xt.kappa <- xtable(kappa.tab, digits=3, caption="Cohen's weighted kappa coefficients (with p-values) as a measure of
                      interrater agreement between pairs of human rights indicators for the entire SVAC dataset, and for  
                      government and rebel group observations, respectively.",
                      label='xt-kappa')
names(xt.kappa) <- c("Human rights indicators", "All SVAC", "p-value", 
                        "Governments", "p-value", "Rebels", "p-value")
xt.kappa

## MAIN ARTICLE: Table I
print(xt.kappa, file=arguments$xt_out_kappa,
      table.placement='h', caption.placement='top',
      include.rownames=FALSE, hline.after=c(-1,0,3))

## ------------------------------------
##  plot irr coeff over time, by year and data (sub)set
## ------------------------------------

##little correlation function
rank.irr.two.vars <- function(data, irr.over.time, vec.A, vec.B) {
  for (yr in sort(unique(data$year))) {
    yr.subset <- data[data$year==yr, ]
    rank.irr <- kappa2(yr.subset[, c(vec.A, vec.B)], weight = 'equal')
    rank.irr.tab <- data.frame(year=yr, sources = paste(vec.A, 'x', vec.B),
                                irr.coef=rank.irr$value, p.value=rank.irr$p.value)
    if (rank.irr.tab$p.value>0.05 & !is.na(rank.irr.tab$p.value)) {
      rank.irr.tab$irr.coef <- NA
      rank.irr.tab$p.value <- "corr. not sig."
    }
    
    irr.over.time <- rbind(irr.over.time, rank.irr.tab)
  }
  return(irr.over.time)
}

grayshades <- gray.colors(3, start = .9, end=.3)

plot_irr_coef_over_time <- function(data, outputfile_argument) {
  
  irr.over.time <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                             c("year", "sources", "irr.coef", "p.value"))

  irr.over.time <- rank.irr.two.vars(data, irr.over.time, "ai_prev", "hrw_prev")
  irr.over.time <- rank.irr.two.vars(data, irr.over.time, "ai_prev", "state_prev")
  irr.over.time <- rank.irr.two.vars(data, irr.over.time, "hrw_prev", "state_prev")

  xlab <- ''
  ylab <- "Interrater agreement"

  pdf(arguments[[outputfile_argument]], width=15, height=4)
  par(las=1, mar=c(3,5,2,6) + 0.1, xpd=TRUE, ps=20)
  plot(irr.over.time[irr.over.time$sources=='ai_prev x hrw_prev', 'year'], 
       irr.over.time[irr.over.time$sources=='ai_prev x hrw_prev', 'irr.coef'], 
       xlab = xlab, ylab = "", ylim=c(-.2,1),
      type = 'l', main = "", bty='n', col = grayshades[1], lty=1, lwd=3)
  title(ylab = ylab, line = 3.75)
  lines(irr.over.time[irr.over.time$sources=='hrw_prev x state_prev', 'year'], 
        irr.over.time[irr.over.time$sources=='hrw_prev x state_prev', 'irr.coef'], 
        col=grayshades[2], lty=2, lwd=3)
  lines(irr.over.time[irr.over.time$sources=='ai_prev x state_prev', 'year'], 
        irr.over.time[irr.over.time$sources=='ai_prev x state_prev', 'irr.coef'], 
        col=grayshades[3], lty=3, lwd=5)
  abline(h=0, lty=3, xpd=FALSE)
  legend('topright', inset = c(-.085,-0.15), y.intersp = 1.5,
         c("AI x HRW", "HRW x USSD", "AI x USSD"),
         lty = c(1, 2, 3),
         col = c(grayshades[1],  grayshades[2], grayshades[3]),
         lwd = c(3, 3, 5), bty = 'n')
  dev.off()
}

plot_irr_coef_over_time(data, "tl_irr_all") ## main article: Figure 3
plot_irr_coef_over_time(GOV, "tl_irr_gov")  ## online apendix: Figure 3(a)
plot_irr_coef_over_time(REB, "tl_irr_reb")  ## online apendix: Figure 3(b)
#end.of.script.