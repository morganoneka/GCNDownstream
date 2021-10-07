library(igraph)
library(tidyverse)
library(reshape)

# this function gets a list of all possible values for a column name
get_all_values <- function(data_list, column_name){
  return(unique(do.call(rbind, data_list)[[column_name]]))
}

# return only a certain category
subset_category <- function(data_list, column_name, value){
  return(lapply(data_list, function(x){
    return(x[which(x[,column_name] == value), ])
  }))
}

# only get significant results based on whatever statistic is most important to you
significant_only <- function(data_list, p_column, value){
  return(lapply(data_list, function(x){
    return(x[which(x[,p_column] <= value), ])
  }))
}

# identify unique column values
get_unique_terms <- function(data_list, column_name){
  combined <- do.call(rbind, data_list)
  counts <- as.data.frame(table(combined[,column_name]), stringsAsFactors=FALSE)
  unique <- counts[which(counts$Freq == 1), "Var1"]
  return(lapply(data_list, function(x){
    return(x[which(x[,column_name] %in% unique), ])
  }))
}

# subset data_list based on a list of column names
only_columns_of_interest <- function(data_list, columns){
  return(lapply(data_list, function(x) x[,columns]))
}

# output occurrence info for a column in a human-readable format
readable_counts <- function(data_list, column_name){
  
  # get only significant values for that column
  tmp_analysis <- significant_only(subset_category(fac_raw_unique, "Category", column_name), "PValue", 0.05)
  
  # combine all info together
  combo = do.call(rbind,lapply(1:length(tmp_analysis), function(i){
    tmp_analysis[[i]]$Region = i
    return(tmp_analysis[[i]])
  }))
  
  # create table of counts
  counts = as.data.frame(table(combo$Term))
  
  # create human-readable string of regions involved
  #TODO: not everyone will have a "Region" column so figure out a way to make this work with other people's data
  regions = as.data.frame(cbind(unique(combo$Term), unlist(lapply(unique(combo$Term), function(x){
    as.character(paste(combo[which(combo$Term == x), "Region"], collapse=",", sep=","))
  }))))
  
  merged_info = merge(counts, regions, by.x="Var1", by.y="V1")
  return(merged_info)
}
