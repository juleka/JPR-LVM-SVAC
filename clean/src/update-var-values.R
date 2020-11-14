## Authors:      Ju
## Maintainers:  Ju
##
## ---------------------------------
## Purpose: create new conflict- and actor-based variable values
## ---------------------------------

library(argparse)
library(yaml)

parser <- ArgumentParser()
parser$add_argument("--inputfile", type="character")
parser$add_argument("--CONSTANTS", type="character")
parser$add_argument("--outputfile", type="character")
arguments <- parser$parse_args()

CONSTANTS <- yaml.load_file(arguments$CONSTANTS)
data <- read.csv(arguments$inputfile, header=TRUE, sep='|', stringsAsFactors = FALSE)
print(str(data))

data$actor <- gsub('Government of ', '', data$actor)
print(table(data$country))

#keep old conflict names from "country" column
data$conflict_name <- data$country

data$actor_country <- ifelse(data$conflict_type=='intra', data$country, NA )
data$actor_country <- ifelse(data$conflict_type=='inter', data$actor, data$actor_country )

data$conflict_country <- data$country


##cleaning function
update_from_CONSTANTS <- function(var_name, CONSTANTS_name) {
  for (i in names(CONSTANTS[[CONSTANTS_name]])) {
    data[[var_name]] <- ifelse(data[[var_name]]==i, CONSTANTS[[CONSTANTS_name]][[i]], data[[var_name]])
  }
  return(data[[var_name]])
}

#clean actor_country values
data$actor_country <- update_from_CONSTANTS('actor_country', 'fix_names')
#clean conflict_country values
data$conflict_country <- update_from_CONSTANTS('conflict_country', 'fix_names')

print(str(data))
data[data$conflictid_new==283, c('year', 'actor', 'country', 'conflict_name', 'conflict_country', 'actor_country')]
data[data$conflictid_new==439, c('year', 'actor', 'country', 'conflict_name', 'conflict_country', 'actor_country')]


write.table(data, file=arguments$outputfile, sep='|', row.names = FALSE)
#End of script.