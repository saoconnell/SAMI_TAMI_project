###---------------------------------------------------------------------------------
###  Sample Load Runner Data
###     For each transaction, trim the data of outliers, and then sample across
###       each test return the same number of samples per test.
###       up sampling the smaller tests and down sampling the larger tests.
###---------------------------------------------------------------------------------

sample_LR <- function(in_data, start_time, end_time, test.Data.Dir, number_of_samples) {
  
  
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
    
    ## REMOVE THE OUTLIERS
    t.data <- ss[-remove_indx,]
    
    if (number_of_samples != 0) {
      ## CREATE A SAMPLE WITH REPLACEMENT, REPLACEMENT IS NEEDED SINCE SOME TESTS HAVE FEW SAMPLES
      sample_index <- sample(1:nrow(t.data), number_of_samples, replace=TRUE)
      
      ## CREATE A SAMPLE SET WITH JUST THE SELECT SAMPLES
      s.data <- t.data[sample_index,]
      
    } else {
      
      ## IF THE NUMBER OF SAMPLES == 0, THEN RETURN THE COMPLETE DATA
      s.data <- t.data
      
    }
    
    ##  SAVE THE DATA
    if (exists("sampled_data")) {
      sampled_data <- rbind(sampled_data, s.data)
    } else {
      sampled_data <- s.data
    }
    
  }
    
  return(sampled_data)
  
}