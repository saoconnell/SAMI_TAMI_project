charts_LR <- function(in_data, start_time, end_time, test.Data.Dir) {
  
  
  ##-----------------------------------------------------------------------------------------
  ##-----------------------------------------------------------------------------------------
  ## PLOT  -  Unfilter and Filter Response time per transaction, with Response time data
  ##-----------------------------------------------------------------------------------------
  ##-----------------------------------------------------------------------------------------
  
  ####
  ### --------------------  CREATE LABELS FOR THE XAXIS ------------------------------------
  ####
  analysisName <- paste0(in_data$type[1], "_", in_data$test[1])
  
  
  graphDir <- paste0(test.Data.Dir, "/graphics/")
  dir.create(graphDir, showWarnings = TRUE, recursive = FALSE)
  
  current.Dir <- getwd()
  
  setwd(test.Data.Dir)
  
  
  #in_data$trans_resp_time[in_data$trans_resp_time > 1000] <- mean(in_data$trans_resp_time)
  
  by_break <- 5
  myLabels <- seq(trunc(start_time, units='min'), trunc(end_time+900, units='min'), by='5 min')

  my_cols <- return_Set1("91")
  red <- my_cols[1]
  blue <- my_cols[2]
  
  srt_data <- sort.data.frame(~scenario_elapse_time,in_data)
  
  output = 'png'
  
  ###
  ### CREATE AN ALL TRANSACTION CHART
  ###
  ###  CLEAN UP THE OUTLYING RESPONSE TIMES TO SHOW DETAIL IN THE CHARTS
    
  srt_data$trans_name <- gsub("&", " ", srt_data$trans_name)
  
  listTranNames <- levels(as.factor(srt_data$trans_name))
  
  for (i in 1:2 ) {
    
    cat("PASS = ", i, "\n")
    
    if (i == 2) {
      
      cat("  CREATING ALL TRANSACTIONS\n")
      in_data$trans_name <- "All_Transactions"
      srt_data$trans_name <- "All_Transactions"
      listTranNames <- levels(as.factor(in_data$trans_name))
    }
    
    
    
    for (Trn in listTranNames) {
      #ss <- subset(srt_data, FQ_test_name == Trn)
      
      ss <- subset(srt_data, trans_name == Trn)
      
      cat("Transaction = ", Trn, " number of rows = ", nrow(ss), "\n")
      if (nrow(ss) >= 15) {
        remove_indx <- trimOutliers(ss$trans_resp_time)
        
        openGdevice(type=output, file=paste(graphDir, analysisName, "DENSITY",  Trn, sep="_"), w=10, h=8, r=108, ps=12)
        
        avg <- round(mean(ss$trans_resp_time),2)
        trim_avg <- round(mean(ss$trans_resp_time[-remove_indx]),2)
        med  <- round(median(ss$trans_resp_time),2)
        trim_med <- round(median(ss$trans_resp_time[-remove_indx]),2)
        
        #par(mfrow= c(2,2))
        nf <- layout(matrix(c(1,2,3,3),2,2,byrow=TRUE), c(2,2), c(2,2), TRUE)
        nf <- layout(matrix(c(1,1,2,2,3,3,3,3,4,4,4,4),3,4,byrow=TRUE), c(1,1,1,1), c(1,1,1,1), TRUE)
        layout.show(nf)
        #par(mar=c(.5,.5,.5,.5))
        
        plot(density(ss$trans_resp_time),
             main=paste("FULL_DATA:", Trn, "\n", sep=""), #cex.main=.8, cex.sub=.7,
             sub=paste("FULL: mean=", avg, "median=", med, "TRIM: mean=", trim_avg, "median=", trim_med, sep=" "))
        
        axis(2, at = NULL, labels = FALSE, tick = FALSE)
        
        legend("topright", c("FULL: mean", "FULL: median", "TRIM: mean", "TRIM: median"),
               title="Metrics",
               lty=c(1,1,2,2),
               lwd=c(1,1,3,3),
               col=c('red','black','red','black'),
               cex=.6
        )
        segments(avg, 0,  avg, 100, col='red')
        segments(med, 0,  med, 100, col='black')
        segments(trim_avg, 0,  trim_avg, 100, col='red', lty=2, lwd=3)
        segments(trim_med, 0,  trim_med, 100, col='black', lty=2, lwd=3)
        
        if (length(ss$trans_resp_time[-remove_indx]) > 4) {
          plot(density(ss$trans_resp_time[-remove_indx]),
               main=( paste("TRIM_DATA:", Trn, "\n", sep="")), #cex.main=.8, cex.sub=.7,
               sub=paste("FULL: mean=", avg, "median=", med, "TRIM: mean=", trim_avg, "median=", trim_med, sep=" "))
          legend("topright", c("FULL: mean", "FULL: median", "TRIM: mean", "TRIM: median"),
                 title="Metrics",
                 lty=c(1,1,2,2),
                 lwd=c(1,1,3,3),
                 col=c('red','black','red','black'),
                 cex=.6
          )
          segments(avg, 0,  avg, 100, col='red')
          segments(med, 0,  med, 100, col='black')
          segments(trim_avg, 0,  trim_avg, 100, col='red', lty=2, lwd=3)
          segments(trim_med, 0,  trim_med, 100, col='black', lty=2, lwd=3)
        } else {
          cat("NOT ENOUGH POINTS = ", Trn, "\n")
          plot(0,0, main="NOT ENOUGH DATA")
        }
        
        plot(myLabels,rep(0,length(myLabels)), pch='', ylim=c(0,max(ss$trans_resp_time)),
             main=( paste(Trn, sep="")), #cex.main=.6, cex.sub=.6,
        )
        lines(ss$trans_start_time, ss$trans_resp_time, type='s')  	
        
        
        legend("topright", c("FULL", "TRIMMED"),
               title="Resp Time",
               lty=c(1,1),
               lwd=c(1,1),
               pch=20,
               col=c(red,blue),
               cex=.6
        )
        #points(ss$scenario_elapse_time, ss$trans_resp_time, cex=1.5, pch=20, col=red)
        points(ss$trans_start_time, ss$trans_resp_time, cex=1, pch=20, col=red)
        points(ss$trans_start_time[remove_indx], ss$trans_resp_time[remove_indx], cex=1.1, pch=20, col=blue)
        
        
        by_break <- 1
        myLabels <- seq(trunc(start_time, units='min'), trunc(end_time+900, units='min'), by=paste0(by_break, ' min'))
        
        ss$one <- cut(ss$trans_start_time, breaks=myLabels, include.lowest=TRUE, right=TRUE)
        plot(myLabels[1:length(as.numeric(table(ss$one)))], as.numeric(table(ss$one)), type='s', 
             main=Trn,
             ylab=paste0("Number of Transactions/", by_break, " mins"),
             xlab="Time")
        
        dev.off()
        
      } ## END IF
      
    }
    
  }
  
  setwd(current.Dir)
  
}