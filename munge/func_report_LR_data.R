###---------------------------------------------------------------------------------
###  Report Load Runner Data
###     For each test cerate the summary reports for all the transactions 
###       in each of the test
###     Write the results to the test directory
###---------------------------------------------------------------------------------

report_LR <- function(in_data, start_time, end_time, test.Data.Dir) {
  
  
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
  
  summ_data <- summarizeData(in_data, "FULL_DATA")
  summ_data <- rbind(summ_data, summarizeData(trimmed_data, "TRIM_DATA"))
  
  current_dir <- getwd()
  
  setwd(test.Data.Dir)
  
  ## REPORT THE SUMMARY OF FULL VS. TRIMMED DATA
  options(width=160)
  sink(file=paste(dataDir, analysisName, "_REPORT_DENSITY", ".txt", sep=""))
  summ_data[order(summ_data$TransNames),]
  sink()
  options(width=80)
  
  ss <- subset(summ_data, label == 'TRIM_DATA')
  write.csv(ss, file=paste0(analysisName,'_TRIM_STATS.CSV'))
  
  ss <- subset(summ_data, label == 'FULL_DATA')
  write.csv(ss, file=paste0(analysisName,'_FULL_STATS.CSV'))
  
  ss <- subset(summ_data)
  write.csv(ss, file=paste0(analysisName,'_ALL_STATS.CSV'))
  
  setwd(current_dir)
  
  return(summ_data)
  
}