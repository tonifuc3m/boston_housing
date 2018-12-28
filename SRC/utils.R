na2mode <- function(train, data, att_name){
  
  # Replace NAs of a factor column by the mode of that column
  # in the train set
  
  ### INPUT:
  # train_data: train dataframe
  # all_data: dataframe where I want to replace NAs
  # att_name: column name (must exist in train_data and all_data)
  
  ### OUTPUT: 
  # all_data: dataframe with no NAs in the specified column
  
  # Get attribute position
  c = names(data)
  pos <- which(c %in% att_name)
  
  c.train = names(train_data)
  pos.train <- which(c.train %in% att_name)
  
  # Mode using only train data
  tt <- table(train[[pos.train]])
  mode_val <- names(tt[which.max(tt)])
  
  # Substitute NA by mode
  data[[pos]][is.na(data[[pos]])] <- mode_val
  
  return(data)
}


na2mean <- function(train_data, all_data, att_name){
  
  # Replace NAs of a numeric column by the mean of that column
  # in the train set
  
  ### INPUT:
  # train_data: train dataframe
  # all_data: dataframe where I want to replace NAs
  # att_name: column name (must exist in train_data and all_data)
  
  ### OUTPUT: 
  # all_data: dataframe with no NAs in the specified column
  
  # Get attribute position
  c = names(all_data)
  pos <- which(c %in% att_name)
  
  c.train = names(train_data)
  pos.train <- which(c.train %in% att_name)
  
  # Mean using only train data
  mean_val <- mean(train_data[[pos.train]], na.rm = TRUE)
  
  # Substitute NA by mode
  all_data[[pos]][is.na(all_data[[pos]])] <- mean_val
  
  return(all_data)
}

correlation_filter <- function(data, threshold){
  
  # Eliminates highly correlated columns in a dataframe
  
  ### INPUT:
  # data: dataframe
  # threshold: double
  
  ### OUTPUT: 
  # data.new: dataframe
  
  tmp <- cor(data)
  tmp[upper.tri(tmp)] <- 0
  diag(tmp) <- 0
  
  data.new <- 
    data[,!apply(tmp,2,
                 function(x) (any(x > threshold) | any(x < -threshold)))]
  return(data.new)
  
}
