## Authors:      Ju
## Maintainers:  Ju
##
## ------------------------------------------------------------
## Purpose: plot distribution of reported prevalence over time
##          Figure 1(b) main article, Figure 1(a)-(h) online appendix
## ------------------------------------------------------------

library(argparse)
library(reshape2)

parser <- ArgumentParser()
parser$add_argument("--inputfile", type="character")
parser$add_argument("--inputfile_nonagg", type="character")
parser$add_argument("--outputfile_mainarticle", type="character")
parser$add_argument("--outputfile_agg_max", type="character")
parser$add_argument("--outputfile_nonagg_max", type="character")
parser$add_argument("--outputfile_agg_ai", type="character")
parser$add_argument("--outputfile_agg_hrw", type="character")
parser$add_argument("--outputfile_agg_ussd", type="character")
parser$add_argument("--outputfile_nonagg_ai", type="character")
parser$add_argument("--outputfile_nonagg_hrw", type="character")
parser$add_argument("--outputfile_nonagg_ussd", type="character")
arguments <- parser$parse_args()

make_barplot_function <- function(input_data, vector_to_plot, outputfile_name, print_p_zero=FALSE) {
  prev_t_dist <- as.data.frame(table(input_data$year, input_data[[vector_to_plot]]), stringsAsFactors = FALSE)
  colnames(prev_t_dist) <- c('year', 'prev', 'Freq')
  n_zeros <- sum(prev_t_dist[prev_t_dist$prev==0, 'Freq'])
  zero_percent <- round(n_zeros/nrow(input_data), digits=4)*100
  # print(zero_percent)
  year_prev_sum  <- aggregate(prev_t_dist$Freq, by=list(prev_t_dist$year), sum)
  colnames(year_prev_sum) <- c('year', 'year_prev_sum')
  prev_t_dist <- merge(prev_t_dist, year_prev_sum, by='year')
  prev_t_dist$perc <- (prev_t_dist$Freq/prev_t_dist$year_prev_sum)*100
  
  year_perc <- acast(prev_t_dist, year~prev, value.var="perc")
  
  pdf(arguments[[outputfile_name]], width = 15, height = 4)
  par(las=1, mar=c(4,5,1,5), ps=20)
  barplot(t(year_perc), width = .25, space=.5, beside=FALSE, 
          col=gray.colors(4, start = .9, end=.3), # names.arg = year_labels, 
          ylab = "Percent", 
          #legend = c(0:3),
          legend.text=TRUE,
          args.legend=list(
            x='right',
            bty = "n", inset=c(-.08,0), xjust=0, y.intersp=1.5,
            title = "Reported \n Prevalence"
          ))
  if (print_p_zero==TRUE) {
    text(x=5, y=30, col='blue', cex=2, label = paste('N(0)=', zero_percent, "%", sep=''))
    }
}

data <- read.csv(arguments$inputfile, 
                 header=TRUE, sep='|', stringsAsFactors = FALSE)

## MAIN ARTICLE: FIGURE 1(b)
make_barplot_function(data, 'max_prev', 'outputfile_mainarticle')

## ----------------------
##   plots for Online Appendix
## ----------------------

## ONLINE APPENDIX: FIGURE 1(g)
make_barplot_function(data, 'max_prev', 'outputfile_agg_max', TRUE)

nonagg <- read.csv(arguments$inputfile_nonagg, header=TRUE, sep='|', stringsAsFactors = FALSE)
conf_years <- nonagg[nonagg$conflictyear==1,]

# set obs prev vars to zero, if NA
prev_vecs <- c('ai_prev', 'hrw_prev', 'state_prev')

for (pvec in prev_vecs) {
  conf_years[[pvec]] <- ifelse(is.na(conf_years[[pvec]]), 0, conf_years[[pvec]])
  print(table(conf_years[[pvec]], useNA = 'always'))
}

# max observed value prevalence
for (i in 1:nrow(conf_years)) {
  conf_years[i, 'max_prev'] <- max(conf_years[i, c('ai_prev', 'hrw_prev', 'state_prev')], na.rm = TRUE)
}
conf_years$max_prev <- ifelse(conf_years$max_prev==-Inf, NA, conf_years$max_prev)
print(table(conf_years$max_prev, useNA = 'always'))

## ONLINE APPENDIX: FIGURE 1(h)
make_barplot_function(conf_years, 'max_prev', 'outputfile_nonagg_max', TRUE)

make_barplot_function(data, 'ai_prev', 'outputfile_agg_ai', TRUE) ## ONLINE APPENDIX: Figure 1(a)
make_barplot_function(data, 'hrw_prev', 'outputfile_agg_hrw', TRUE) ## ONLINE APPENDIX: Figure 1(c)
make_barplot_function(data, 'state_prev', 'outputfile_agg_ussd', TRUE) ## ONLINE APPENDIX: Figure 1(e)
make_barplot_function(conf_years, 'ai_prev', 'outputfile_nonagg_ai', TRUE) ## ONLINE APPENDIX: Figure 1(b)
make_barplot_function(conf_years, 'hrw_prev', 'outputfile_nonagg_hrw', TRUE) ## ONLINE APPENDIX: Figure 1(d)
make_barplot_function(conf_years, 'state_prev', 'outputfile_nonagg_ussd', TRUE) ## ONLINE APPENDIX: Figure 1(f)

#end of script.