###---------------------------------------------------------
###   CheckLoadRunnerRawData:  
###   Author: Stephen O'Connell
###   Date: 06/20/2012
###   Description:
###
###       Loads the Raw Data exported from a Load Runner Test and checks the density of the 
###          of the response times to validate the distribution of the response times
###
###       graphically displays the results for visual verification
###
###   Change History: (why is always just because...)
###        Who         When            What
###        SAOB        2012-06-20     Initial Development
###        SAOB        2013-02-25     Improved the Chart to add time series for trans/min
###---------------------------------------------------------

##-----------------------------------------------------------------------------------------

## CLEANUP
rm(list=ls())
#memory.limit(size=3072)

##-------------
## LOAD required functions
##-------------
#require(sig)
require(Hmisc)
require(lattice)
require(latticeExtra)
require(Cairo)
require(reshape)
require(stringr)

##-------------
## LOAD SAOB SetUp functions
##-------------
setUpFunc_Dir <- c("c:\\Users\\SAOb\\Documents\\workspace\\R\\setUp\\")
setUpFunctions <- c('setupFunc.R')
source(paste(setUpFunc_Dir, setUpFunctions, sep=""))

##--------------------------------------------------
## FUNCTION: trimOutliers
##--------------------------------------------------
trimOutliers <- function(x, low=.05, high=.95) {
  high_q <- as.numeric(quantile(x, prob=high)[1])
  low_q <- as.numeric(quantile(x, prob=low)[1])
  high_indx <- which(x >= high_q)
  low_indx <- which(x <= low_q)
  indx <- c(high_indx, low_indx)
  return(indx)
}

###-----------------------------------------
###  SUMMARIZE THE FULL AND TRIMMED DATA
###-----------------------------------------
summarizeData <- function(df, label='DATA') {
  byTran <- tapply(df$trans_resp_time, df$trans_name, sao.func)
  byTran <- do.call("rbind", byTran)
  TransNames <- row.names(byTran)
  byTran <- cbind(TransNames, data.frame(round(byTran,3)))
  rownames(byTran) <- NULL
  #byTran$TransNames <- paste(byTran$TransNames, label, sep="_")
  byTran$label <- label
  
  return(byTran)
  
}

###-----------------------------------------
###  SAO STAT SUMMARY FUNCTION
###-----------------------------------------
"sao.func" <- function(a)
{
  #
  # my.func -- commonly used statistical metrics
  #
  if (is.integer(a)) a <- as.numeric(a)
  data <- c(length(a), mean(a), sqrt(var(a)), quantile(a, c(0., .25, 0.5, .75, 0.90, 0.95,
                                                            1.), na.rm=TRUE))
  names(data) <- c("Count", "Mean", "SD", "Min", "p25", "Median", "p75", "p90", "p95", "Max")
  data
}


###------------------------------------------------------
###   SET PROCESSING PARAMETERS
###------------------------------------------------------
###  CREATE A NAME FOR THE ANALYSIS, COULD BE DATA TIME, OR A LABEL
###  RUN TIME TAKEN FROM THE LOAD RUNNER REPORT
#ROOT_DIR <- "20130725_GD_GIS/PERF_TEST/MAINTENANCE"
#ROOT_DIR <- "20130725_GD_GIS/PERF_TEST/WEB"
#ROOT_DIR <- "20140314_BSM/PERF_TEST/ENOC"
ROOT_DIR <- "20140624_SAMI/PERF_TESTS/ROUND_2_QA"
runTime <-  "27/06/2014 08:08:00 - 27/06/2014 09:22:16"
##  ANALYSIS NAME

analysisName <- "STEP_USERS_57"

###------------------------------------------------------
###   ENVIRONMENT SETUP
###------------------------------------------------------
dataDir <- (paste("C:/Users/SAOb/Documents/performance_data/", ROOT_DIR, "/",analysisName, "/LR_RAW_DATA/", sep=''))
#dataDir <- (paste("C:/Users/SAOb/Documents/performance_data/20130307_ED_GIS/PERF_TESTS/WEBR/PROD_PEAK_HOUR_CALIBRATE_TEST_1/", sep=''))
#dataDir <- (paste("C:/Users/SAOb/Documents/performance_data/20130307_ED_GIS/PERF_TESTS/EDER/PQ_SCR_2_20130822_RUN_1/LR_RAW_DATA/", sep=''))
graphDir <- (paste(dataDir,  "graphics", '/', sep=""))
dir.create(graphDir, showWarnings = TRUE, recursive = FALSE)
setwd(dataDir)
getwd()


###   CREATE A START AND END FOR SUBSETTING THE DATA AROUND A SPECIFIC TEST, SUBSTRACT 20 MINUTES
###     THE START OF THE TEST AND ADD 2O MINUTES TO THE END OF THE TEST
start_time <- as.POSIXct(strptime(substring(runTime,1,19), format="%d/%m/%Y %H:%M:%S"))
end_time <- as.POSIXct(strptime(substring(runTime,23,41), format="%d/%m/%Y %H:%M:%S"))



## CONVERT MINUTES TO SECONDS
warmUpTime <- 5 * 60
coolDownTime <- 0  * 60

step_duration <- 55 * 60

# start_time <- start_time + warmUpTime
# end_time   <- end_time   - coolDownTime
# 
# (end_time - start_time)
# start_time
# end_time



start_time
end_time



###------------------------------------------------------
###   LOAD LR Raw Data Files
###------------------------------------------------------

list.files()

files <- list.files()
rm(in_data, tmp_data)
options(stringsAsFactors=FALSE)
for (f in files) {
  
  cat("THIS IS THE FILE = ", f, "\n")
  
  if(grepl("^Raw_Data", f)) {
    #tmp_data <- read.csv(f, header=TRUE,  stringsAsFactors=FALSE, sep=',')
    tmp_data <- read.csv(f, header=FALSE, skip=1, stringsAsFactors=FALSE, sep=',')
    
    if(exists("in_data")) {
      in_data <- rbind(in_data, tmp_data)
    } else {
      in_data <- tmp_data
    }
  }
  
}

str(in_data)

###
###  WHICH HEADER DEPENDS ON HOW THE DATA WAS EXTRACTED, NEED TO LOOK AT THE FILE IN EXCEL TO DETERMINE
###
if (length(in_data) == 10) {
  header <- c("vuser_id","group_name","trans_end_status","script_name","trans_hi_path","host_name","emulation_loc",
              "scenario_elapse_time", "trans_resp_time","trans_name")
} else {
  
  header <- c("group_name","trans_end_status","script_name","trans_hi_path","host_name","emulation_loc", 
              "scenario_elapse_time", "trans_resp_time","trans_name")
}

#Group Name	Transaction End Status	Location Name	Script Name	Transaction Hierarchical Path	Host Name	Scenario Elapsed Time	Transaction Response Time	Transaction Name
#header <- c("group_name","trans_end_status","loc_name","script_name","trans_hi_path","host_name","scenario_elapse_time", "trans_resp_time","trans_name")

#Group Name	Transaction End Status	Script Name	Transaction Hierarchical Path	Host Name	Scenario Elapsed Time	Transaction Response Time	Transaction Name
#header <- c("group_name","trans_end_status","script_name","trans_hi_path","host_name","scenario_elapse_time", "trans_resp_time","trans_name")
names(in_data) <- header

describe(in_data)
unique(in_data$vuser)

HOLD_in_data <- in_data
#in_data <- Hold_in_data
###-----------------------------------------------------------
###   DATA CLEAN UP, ADDTIONAL ATTRIBUTION, AND FILTERING
###-----------------------------------------------------------

###------------------------------
## CLEAN-UP:
###------------------------------

# REMOVE ',' FROM THE NUMBERS
in_data$scenario_elapse_time <- gsub(',', '', in_data$scenario_elapse_time)
in_data$trans_resp_time <- gsub(',', '', in_data$trans_resp_time)
in_data$scenario_elapse_time <- as.numeric(in_data$scenario_elapse_time)
in_data$trans_resp_time <- as.numeric(in_data$trans_resp_time)

# FIX SCRIPT NAMES WITH INVALID CHARACTERS
in_data$trans_name <- gsub(":", "_", in_data$trans_name)
in_data$trans_name <- gsub("&", " ", in_data$trans_name)

###------------------------------
## ATTRIBUTION:
###------------------------------

### CREATE START AND END TIMES FOR EACH TRANSACTION
in_data$trans_end_time <- start_time + in_data$scenario_elapse_time
in_data$trans_start_time <- in_data$trans_end_time - in_data$trans_resp_time


###------------------------------
## FILTERING:
###------------------------------

##---- 
### SAOB: 2012-06-22:  I MOVE THIS CODE FROM THE SET PARAMETERS TO THE FILTERING SECION
## ADJUST THE START AND END TIME BY THE WARM UP AND COOL DOWN PERIOD
##  THIS WILL BE USED TO EXTRACT ONLY STEADY STATE TRANSACTIONS.
#start_time <- start_time + warmUpTime
#end_time   <- end_time   - coolDownTime


start_time <- start_time + warmUpTime
end_time <- start_time + step_duration



## REMOVE THE WARMUP AND COOLDOWN TRANSACTIONS

in_data <- subset(in_data, trans_start_time > start_time & trans_end_time < end_time)


###------------------------------
## REPORT:
###------------------------------

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


### SUMMARIZE DATA
describe(in_data)


##------------------------------------------------------
## SAVE DATA
##------------------------------------------------------

save(in_data, summ_data, trimmed_data, file=paste(dataDir, analysisName, "_Rdata_DENSITY", ".Rdata", sep=""))
##load(file=paste(dataDir, analysisName, "_Rdata_DENSITY", ".Rdata", sep=""))




##-----------------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------------
## PLOT  -  Unfilter and Filter Response time per transaction, with Response time data
##-----------------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------------

####
### --------------------  CREATE LABELS FOR THE XAXIS ------------------------------------
####

hold_all_data <- in_data
#in_data <- hold_all_data
describe(in_data)

subset(in_data, trans_resp_time > 1000)
in_data$trans_resp_time[in_data$trans_resp_time > 1000] <- mean(in_data$trans_resp_time)
by_break <- 5
myLabels <- seq(trunc(start_time, units='min'), trunc(end_time+900, units='min'), by='5 min')
#myLabels <- substring(as.character(myLabels), 12, 19)
#blankNums <- seq(from=2, to=length(myLabels), by=3)
#blankNums <- c(blankNums, seq(from=3, to=length(myLabels), by=3))
#myLabels[c(blankNums)] <- ' '

my_cols <- return_Set1("91")
red <- my_cols[1]
blue <- my_cols[2]

srt_data <- sort.data.frame(~scenario_elapse_time,in_data)

output = 'png'

###
### CREATE AN ALL TRANSACTION CHART
###
###  CLEAN UP THE OUTLYING RESPONSE TIMES TO SHOW DETAIL IN THE CHARTS
#subset(in_data, trans_resp_time > 24 &  trans_name == "Select Search")
#srt_data$trans_resp_time[srt_data$trans_resp_time > 25 &  srt_data$trans_name == "Select Search"] <- 25
#
#unique(in_data$trans_name)
#subset(in_data, trans_resp_time > 25 &  trans_name == "ZoomIn ")
#srt_data$trans_resp_time[srt_data$trans_resp_time > 10 &  srt_data$trans_name == "ZoomIn "] <- 10

srt_data$trans_name[srt_data$trans_name == "ArcMap - Log Out"] <- "Arc Map - Log Out"

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



## cumm_table <- as.data.frame(cbind(as.character(myLabels[1:length(as.numeric(table(ss$one)))]), as.numeric(table(ss$one))))
## 
## names(cumm_table) <- c("time","num_trans")
## cumm_table$num_trans <- as.numeric(cumm_table$num_trans)
## cumm_table <- cbind(cumm_table, cumsum(cumsum=cumm_table$num_trans))
## names(cumm_table) <- c("time","num_trans", "cumm_sum")
## str(cumm_table)
## 
## c(0,cumsum(cumm_table$num_trans))

#myLabels <- seq(trunc(start_time, units='min'), trunc(end_time+900, units='min'), by=paste0(by_break, ' min'))
#
#ss <- srt_data
#ss$one <- cut(ss$trans_start_time, breaks=myLabels, include.lowest=TRUE, right=TRUE)
#plot(myLabels[1:length(as.numeric(table(ss$one)))], as.numeric(table(ss$one)), type='s', 
#		main="All Transactions",
#		ylab=paste0("Number of Transactions/", by_break, " mins"),
#		xlab="Time")
#



##------------------------------------------------------
## WRAPUP:
##------------------------------------------------------
