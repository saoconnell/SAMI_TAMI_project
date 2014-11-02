###---------------------------------------------------------------------------------
###  Report Load Runner Data
###     return the trimmed dataset
###---------------------------------------------------------------------------------

trim_LR <- function(in_data, start_time, end_time, test.Data.Dir) {
  
  
  #start_time <- start_time + warmUpTime
  #end_time <- start_time + step_duration
  
  analysisName <- paste0(in_data$type[1], "_", in_data$test[1])
  
  ## REMOVE THE WARMUP AND COOLDOWN TRANSACTIONS
  
  in_data <- subset(in_data, trans_start_time > start_time & trans_end_time < end_time)
  
  
  ###------------------------------
  ## REPORT:
  ###------------------------------
  
  #test.Data.Dir <- paste0(rootDir, "/", TEST_TYPE, "/", test_details$test[t], "/", "LR_RAW_DATA/")
  
  ####  TRIM THE DATA TO 90%, REMOVE THE OUTLIERS
  listTranNames <- levels(as.factor(in_data$trans_name)) 
  
  for (n in listTranNames) {
    ss <- subset(in_data, trans_name == n)
    remove_indx <- trimOutliers(ss$trans_resp_time)
    
    if (exists("trimmed_data")) {
      trimmed_data <- rbind(trimmed_data, ss[-remove_indx,])
    } else {
      trimmed_data <- ss[-remove_indx,]
    }
    
  }
  
  
  return(trimmed_data)
  
}