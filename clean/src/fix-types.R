## Authors:      Ju
## Maintainers:  Ju
##
## ---------------------------------
## Purpose: correct conflict and actor types where necessary
## ---------------------------------

library(argparse)
parser <- ArgumentParser()
parser$add_argument("--inputfile", type="character")
parser$add_argument("--outputfile", type="character")
arguments <- parser$parse_args()

data <- read.csv(arguments$inputfile, header=TRUE, sep='|', stringsAsFactors = FALSE)

##fix data_type variable
print("The original CONFLICT type variable ('type') distributes as follows:")
table(data$type, useNA='always')
print("Recoding CONFLICT type variable for our purposes,")
print("treating intrastate and internationalized intrastate the same ('intra'):")
data$conflict_type <- ifelse(data$type==2, 'inter', NA) # inter-state
data$conflict_type <- ifelse(data$type==3, 'intra', data$conflict_type) #intrastate
data$conflict_type <- ifelse(data$type==4, 'intra', data$conflict_type) #internationalized intrastate
stopifnot(sort(unique(data$conflict_type))==c('inter', 'intra'))
table(data$conflict_type, useNA = 'always')
data$type <- NULL

##recode actor_type variable for INTERstate conflicts
print("Actor types for INTRAstate conflicts distribute as follows:")
print("1: government/state actors, 3: non-state actors")
table(data[data$conflict_type=='intra', 'actor_type'])
print("Actor types for INTERstate conflicts distribute as follows:")
table(data[data$conflict_type=='inter', 'actor_type'])
print("Recoding ACTOR type variable for INTERstate conflicts:")
print("in INTERstate conflicts, the actors are always of type state")
data$actor_type <- ifelse(data$conflict_type=='inter', 1, data$actor_type)
table(data[data$conflict_type=='inter', 'actor_type'])

write.table(data, file=arguments$outputfile, sep='|', row.names = FALSE)
#end of script.