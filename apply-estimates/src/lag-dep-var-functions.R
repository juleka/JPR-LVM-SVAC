# Authors:     Ju
# Maintainers: Ju
# Copyright:   2018, Ju
#
# Purpose: functions to create lagged dependent variables
# =======================================================


create_prev_dep_var_lag <- function(data, prev_var) {
  lag_prev_var_name <- paste('lag', prev_var, sep='_')
  print(paste('Now lagging:', lag_prev_var_name))
  data[lag_prev_var_name] <- NA
  
  #ensure proper order for lag indexing
  data <- data[order(data$conflictid_new, data$actor, data$year),]
       
  for(ii in 2:nrow(data)){
    if(data$year[ii] - data$year[ii-1]==1){
      data[[lag_prev_var_name]][ii] <- data[[prev_var]][ii-1]
    }
  }
  if(grepl('prev', prev_var)==TRUE) {
    print(table(data[lag_prev_var_name], useNA = 'always'))
  }
  return(data)
}

populate_values_for_missing_years <- function(fill_data, lag_vec, pop_data) {
  
  ##fill_data: the dataset that has missing values that need filling
  ##lag_vec: the vector with missing values to be filled
  ##pop_data: the dataset with values available for population
  
  print(paste('Populating values for', lag_vec))
  if (grepl('prev', lag_vec)==TRUE) {
    print(table(fill_data[lag_vec], useNA = 'always'))
  }
  
  #subset of data with missing values
  fdata <- fill_data[is.na(fill_data[lag_vec]), ]
  # print(head(fdata))
  
  for (i in 1:nrow(fdata)) {
    
    icid   <- fdata[i, 'conflictid_new']
    iyear  <- fdata[i, 'year']
    iactor <- fdata[i, 'actor_type']
    
    ivalue <- pop_data[pop_data[['conflictid_new']]==icid &
                          pop_data[['year']]==iyear &
                          pop_data[['actor_type']]==iactor, lag_vec]
    
    fill_data[fill_data[['conflictid_new']]==icid & 
                fill_data[['year']]==iyear &
                fill_data[['actor_type']]==iactor, lag_vec ] <- ivalue
    
    # if(grepl('prev', lag_vec)==FALSE) {
    #   print(paste(icid, iyear, iactor, "populated."))
    # }
    
  }
  if (grepl('prev', lag_vec)==TRUE) {
    print(table(fill_data[lag_vec], useNA = 'always'))
  }  
  print(paste('nrow with remaining missing values is:', nrow(fill_data[is.na(fill_data[lag_vec]),])))
  return(fill_data)
}

#end of script.
