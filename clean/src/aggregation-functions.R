# Authors:     Ju
# Maintainers: Ju
#
# Purpose: provide aggregation function
# =========================================================

aggregate_conflicts <- function(data=data) {
  stopifnot(length(unique(data$actor_type))==2)
  #stopifnot(length(unique(data$conflictid_new))==CONSTANTS$N_unique_conflicts)
  stopifnot(length(unique(data$year))==CONSTANTS$N_obs_years)
  stopifnot(length(unique(data$conflict_type))==2)
  
  #create empty data object
  actort2data <- data.frame(conflictid_new=as.integer(), conflict_type=as.character(),
                            actor_type=as.character(), actor=as.character(), 
                            year=as.integer(), conflict_country=as.character(), 
                            ai_prev=as.integer(), hrw_prev=as.integer(), state_prev=as.integer(), 
                            stringsAsFactors = FALSE)
  
  for (ct in unique(data$conflict_type)) {
    print(paste('Now working on conflict type:', ct))
    actort2data <- get_maximum_prev_value(data, actort2data, ct)
  }
  
  print(table(actort2data$ai_prev, useNA = 'always'))
  print(table(actort2data$hrw_prev, useNA = 'always'))
  print(table(actort2data$state_prev, useNA = 'always'))
  
  #the infinite values are for conflict-year observations that do not exist
  ## this happens when we aggregate for the entire, unique set of conflict-years
  actort2data <- actort2data[!is.infinite(actort2data$ai_prev) |
                               !is.infinite(actort2data$hrw_prev) |
                               !is.infinite(actort2data$state_prev), ]
  print(table(actort2data$ai_prev, useNA = 'always'))
  print(table(actort2data$hrw_prev, useNA = 'always'))
  print(table(actort2data$state_prev, useNA = 'always'))
  
  print(str(actort2data))
  print(table(actort2data$actor, useNA = 'always'))
  
  ##the ordering of the data is crucial for the panel_index_lag in the dynamic models:
  actort2data <- actort2data[order(actort2data$conflictid_new, actort2data$actor, actort2data$year),]
  return(actort2data)
}

get_maximum_prev_value <- function(data, actort2data, conflict_type) {
  ## data = conflict_years, or all_years
  ## actor2data = the empty data set to start with
  ## conflict_type =  inter or intra
  
  for (cidn in unique(data[data$conflict_type==conflict_type, 'conflictid_new'])) {
    
    print(paste('Now working on conflict:', cidn))
    
    agdata <- data.frame(conflictid_new=rep(cidn, length(unique(data$year))*2),
                         conflict_type=rep(conflict_type, length(unique(data$year))*2),
                         year=rep(unique(data[['year']]), 2),
                         conflict_country=rep(unique(data[data$conflictid_new==cidn, 'conflict_country']),
                                              length(unique(data$year))*2),
                         ai_prev=rep(NA, length(unique(data$year))*2), 
                         hrw_prev=rep(NA, length(unique(data$year))*2),
                         state_prev=rep(NA, length(unique(data$year))*2),
                         stringsAsFactors = FALSE)
    
    if (conflict_type=='intra') {
      agdata$actor_type <- c(rep(1, length(unique(data$year))), rep(3, length(unique(data$year))))
      agdata$actor      <- c(rep('Government', length(unique(data$year))), rep('Rebels', length(unique(data$year))))
      
      #iterate over actor_types, years, and prev_vecs
      for (t in unique(agdata$actor_type)) {
        #print(paste('Now working on actor type:', t))
        for (y in unique(agdata$year)) {
          # print(paste('Now working on year:', y))
          for (prev_vec in c('ai_prev', 'hrw_prev', 'state_prev')) {
            #print(paste('Now working on prevalence vector:', prev_vec))
            agdata[agdata$actor_type==t & agdata$year==y, prev_vec] <-
            max(data[data$conflictid_new==cidn & data$actor_type==t & data$year==y, prev_vec])
          }
        }
      }
    }
    
    if (conflict_type=='inter') {
      if (length(unique(data[data$conflictid_new==cidn, 'actor_country']))==2) {
        agdata$actor_type <- c(rep(1, length(unique(data$year))*2))
        agdata$actor <- c(rep(unique(data[data$conflictid_new==cidn, 'actor_country'])[1], length(unique(data$year))), 
                          rep(unique(data[data$conflictid_new==cidn, 'actor_country'])[2], length(unique(data$year))))
      } 
      if (length(unique(data[data$conflictid_new==cidn, 'actor_country']))==1) {
        agdata$actor_type <- c(rep(1, length(unique(data$year))))
        agdata$actor <- rep(unique(data[data$conflictid_new==cidn, 'actor_country'])[1], length(unique(data$year)))
        agdata <- agdata[!duplicated(agdata),]
      }
      
      for (a in unique(agdata$actor)) {
        #print(paste('Now working on actor type:', a))
        for (y in unique(agdata$year)) {
          #print(paste('Now working on year:', y))
          for (prev_vec in c('ai_prev', 'hrw_prev', 'state_prev')) {
            #print(paste('Now working on prevalence vector:', prev_vec))
            agdata[agdata$actor==a & agdata$year==y, prev_vec] <-
            max(data[data$conflictid_new==cidn & data$actor_country==a & data$year==y, prev_vec])
          }
        }
      }
    }
    
    actort2data <- rbind(actort2data, agdata)
    print(paste('Done w conflict.'))
  }
  actort2data <- actort2data[!duplicated(actort2data),]
  return(actort2data)
}

#end of script.