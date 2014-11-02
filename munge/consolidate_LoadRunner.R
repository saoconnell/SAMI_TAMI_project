###---------------------------------------------------------
###   consolidate_LoadRunner: 
###      - Reads the Load Runner raw data files from each test
###      - Consolidates the data into one dataframe
###      - produces a graphics
###         - by user Count
###         - by by Test
###
###   Author: Stephen O'Connell
###   Date: 08/03/2012
###   Description:
###   Change History: (why is always just because...)
###        Who         When            What
###       SAOB        08/03/2012      Initial development
###---------------------------------------------------------

##-----------------------------------------------------------------------------------------

## CLEANUP
rm(list=ls())
#memory.limit(size=3072)

##-------------
## LOAD required functions
##-------------
library(Hmisc)
library(reshape)
library(lattice)
library(latticeExtra)
library(Cairo)
library(data.table)
library(reshape)
library(plyr)

##-------------
## LOAD SAOB SetUp functions
##-------------
setUpFunc_Dir <- c("c:\\Users\\SAOb\\Documents\\workspace\\R\\setUp\\")
setUpFunctions <- c('setupFunc.R')
source(paste(setUpFunc_Dir, setUpFunctions, sep=""))

source("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/munge/func_report_LR_data.R")
source("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/munge/func_charts_LR_data.R")
source("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/munge/func_sample_LR_data.R")

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


##-------------
## READ DATA
##-------------
dev.off()

rootDir <- paste("C:/Users/SAOb/Documents/performance_data/20140624_SAMI/PERF_TESTS/", sep="")

TEST_TYPE <- "ROUND_2_QA"
## READ THE TEST DETAILS
test_details <- read.csv(paste0(rootDir, "test_details.csv"), header=TRUE, stringsAsFactor=FALSE)
test_details <- subset(test_details, type == TEST_TYPE)

####  READ ALL THE RAW DATA INTO ON DATAFRAME
if (exists("data.list")) {rm(data.list)}

for (t in 1:length(test_details$test)) {
  
  
  ##DATA DIRECTORY
  dataDir <- paste("C:/Users/SAOb/Documents/performance_data/20140624_SAMI/PERF_TESTS/", TEST_TYPE, "/", test_details$test[t], "/LR_RAW_DATA/", sep="")
  
  setwd(dataDir)
  
  cat("\nDATA DIR = ", dataDir, "\n")

  test_files <- list.files(recursive = TRUE)  
  print(test_files)
  
  for (f in 1:length(test_files)) {
  
    if (grepl('Raw_Data.csv$', test_files[f])) {
      cat("TEST FILE = ", test_files[f], "\n")
      
      temp <- read.csv(test_files[f], header=FALSE, skip=1, stringsAsFactors=FALSE, sep=',')
      temp <- cbind(test=test_details$test[t], 
                    type=test_details$type[t], 
                    temp)
      
      if (!exists("data.list")) {
        data.list <- list()  
        data.list[[paste0(test_details$test[t], "_", test_details$type[t])]] <- temp
      } else {
        data.list[[paste0(test_details$test[t], "_", test_details$type[t])]] <- temp
      }
    }
  }
}

## CONSOLCIATE ALL THE VMWARE DATA
all_LR_data <- as.data.frame(rbindlist(data.list))

header <- c("test","type","group_name","trans_end_status","script_name","trans_hi_path","emulation_loc","host_name",
            "scenario_elapse_time", "trans_resp_time","trans_name")

names(all_LR_data) <- header

### CHECK THE STRCUTURE OF THE DATA FRAME
str(all_LR_data)

###-----------------------------------------------------------
###   DATA CLEAN UP, ADDTIONAL ATTRIBUTION, AND FILTERING
###-----------------------------------------------------------

###------------------------------
## CLEAN-UP:
###------------------------------

# REMOVE ',' FROM THE NUMBERS
all_LR_data$scenario_elapse_time <- gsub(',', '', all_LR_data$scenario_elapse_time)
all_LR_data$trans_resp_time <- gsub(',', '', all_LR_data$trans_resp_time)
all_LR_data$scenario_elapse_time <- as.numeric(all_LR_data$scenario_elapse_time)
all_LR_data$trans_resp_time <- as.numeric(all_LR_data$trans_resp_time)

# FIX SCRIPT NAMES WITH INVALID CHARACTERS
all_LR_data$trans_name <- gsub(" ", "_", all_LR_data$trans_name)
all_LR_data$trans_name <- gsub(":", "_", all_LR_data$trans_name)
all_LR_data$trans_name <- gsub("&", "_", all_LR_data$trans_name)

###  SAVE THE DATA
save(all_LR_data, file=paste0(rootDir, TEST_TYPE, "/all_LR_data.Rdata"))

str(all_LR_data)


###------------------------------------------------------------------------------
###   CHARTS - PROCESS ALL THE DATA FOR EACH TEST
###------------------------------------------------------------------------------
pp_ggplot2 <- ggplot2like()
pp_ggplot2$strip.border      <- list(col="grey60")
pp_ggplot2$axis.line         <- list(col="grey60")
pp_ggplot2$axis.text         <- list(col="black")

output <- "png"

set.seed(1234)
SS.list <- list()
report.list <- list()
sample.list <- list()
trim.list <- list()
source("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/munge/func_report_LR_data.R")
source("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/munge/func_charts_LR_data.R")
source("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/munge/func_sample_LR_data.R")
source("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/munge/func_trim_LR_data.R")
for (t in 1:length(test_details$test)) {
  
  
  ## SUBSET THE DATA BY RUN DATE TIME
  runTime <-  test_details$time[t]
  
  temp_ss <- subset(all_LR_data, test == test_details$test[t])
  
  if (nrow(temp_ss) > 0) {
    ### CREATE START AND END TIMES FOR EACH TRANSACTION
    start_time <- as.POSIXct(strptime(substring(runTime,1,19), format="%d/%m/%Y %H:%M:%S"))
    
    temp_ss$trans_end_time <- start_time + temp_ss$scenario_elapse_time
    temp_ss$trans_start_time <- temp_ss$trans_end_time - temp_ss$trans_resp_time
    
    
    ###   CREATE A START AND END FOR SUBSETTING THE DATA AROUND A SPECIFIC TEST, SUBSTRACT 20 MINUTES
    ###     THE START OF THE TEST AND ADD 2O MINUTES TO THE END OF THE TEST
    startTime <- as.POSIXct(strptime(substring(runTime,1,19), format="%d/%m/%Y %H:%M:%S")) + (test_details$warmup[t] * 60)
    endTime <- startTime + (test_details$duration[t] * 60)
    
    ## DEBUG STUFF
    cat("start_time = ", as.POSIXct(startTime, origin = "1970-01-01"), " end_time = ", as.POSIXct(endTime, origin = "1970-01-01"), " test time = ", test_details$time[t], "\n")
    print(as.POSIXct(startTime, origin = "1970-01-01"))
    print(as.POSIXct(endTime, origin = "1970-01-01"))
    
    str(temp_ss)
    
    ## SUBSET OUT JUST THE TRANSACTIONS THAT OCCURED BETWEEN THESE TIMES.
    SS.list[[test_details$test[t]]] <- subset(temp_ss, trans_start_time >= startTime & trans_end_time <= endTime)
    
    
    ## TEST DIRECTORY FOR OUTPUT
    test.Data.Dir <- paste0(rootDir, "/", TEST_TYPE, "/", test_details$test[t], "/", "LR_RAW_DATA/")
    
    ## REPORT
    report.list[[test_details$test[t]]] <- report_LR(SS.list[[test_details$test[t]]], startTime, endTime, test.Data.Dir)
    
    report.list[[test_details$test[t]]] <- cbind(test=test_details$test[t], 
                                                 user_count=strsplit(test_details$test[t], "_")[[1]][2], 
                                                 report.list[[test_details$test[t]]])

    ## TRIM
    trim.list[[test_details$test[t]]] <- trim_LR(SS.list[[test_details$test[t]]], startTime, endTime, test.Data.Dir)
    
    trim.list[[test_details$test[t]]] <- cbind(test=test_details$test[t], 
                                                 user_count=strsplit(test_details$test[t], "_")[[1]][2], 
                                                 trim.list[[test_details$test[t]]])
    
    
    ## CHARTS
    charts_LR(SS.list[[test_details$test[t]]], startTime, endTime, test.Data.Dir)
    
    
    ## SAMPLE DATA
    #sample.list[[test_details$test[t]]] <- sample_LR(SS.list[[test_details$test[t]]], startTime, endTime, test.Data.Dir, 100)
    sample.list[[test_details$test[t]]] <- sample_LR(SS.list[[test_details$test[t]]], startTime, endTime, test.Data.Dir, 100)
    
    sample.list[[test_details$test[t]]] <- cbind(test=test_details$test[t], 
                                                 user_count=strsplit(test_details$test[t], "_")[[1]][2], 
                                                 sample.list[[test_details$test[t]]])
    
    ## ADD THE TEST AND TYPE TO THE SUBSET AND SAVE IN THE DATA.LIST
    SS.list[[test_details$test[t]]] <- cbind(test=test_details$test[t], 
                                        user_count=strsplit(test_details$test[t], "_")[[1]][2], 
                                        SS.list[[test_details$test[t]]])
  }
  
} 

## COMBINE ALL THE SUB DATA FRAMES
all_ss <- as.data.frame(rbindlist(SS.list))

all_summ_data <- as.data.frame(rbindlist(report.list))

all_sample_data <- as.data.frame(rbindlist(sample.list))

all_trimmed_data <- as.data.frame(rbindlist(trim.list))

str(all_sample_data)
str(all_trimmed_data)
str(all_sample_data)
###------------------------------------------------------------------------------
###  Compare Metrics Across tests, need to reduce samples by warm-up time and
###     trim to a specific duration
###------------------------------------------------------------------------------
###############################

## THE FOLLOWING TRANSACTIONS ARE CONSISTENTLY IN THE TOP 20 BY 90th PERCENTILE
top.transaction.list <- c("19_General_Temporal_02_click_Play",
                      "Load_TAMI",
                      "10_Transmission_Gas_Valves_03_zoom_5_mile",
                      "16_General_Disaster_Select_Sources_02_select_All_Sources",
                      "10_Transmission_Gas_Valves_02_show_Main_Class_Identification",
                      "20_General_Temporal_Forcasts_02_click_Play",
                      "12_Dispatch_Field_Order_02_zoom_1_mile",
                      "15_Dispatch_GSR_03_zoom_1_mile",
                      "13_Dispatch_Odor_Calls_02_zoom_1_mile",
                      "09_Transmission_Automated_Valves_02_zoom_35_miles",
                      "07_Transmission_Routes_03_View_PopUp",
                      "11_Dispatch_GSR_Locations_01_select_GSR_locations",
                      "05_Distribution_Regulator_Sta_Diagram_02_zoom_1_mile",
                      "02_Distribution_GEMs_Maps_Pan_01_select_GEMS_Map",
                      "03_Distribution_GEMs_Maps_Plat_Map_03_zoom_1000ft",
                      "04_Distribution_Regulator_Station_02_zoom_1_mile",
                      "06_Transmission_Stations_02_zoom_1_mile",
                      "03_Distribution_GEMs_Maps_Plat_Map_02_selecet_100scale",
                      "07_Transmission_Routes_01_select_Routes",
                      "01_Distribution_Address_Search_01_search_Address")


##---------------------------------------------------------------
## CONSOLIDATE TEST, BY USER COUNT
##   BOX PLOTS OF METRICS BY USER COUNT
##---------------------------------------------------------------
## ALL servers



## SUBSET ONE OF THE THREE DATA SETS FOR PLOTTING 
PLOT.DATA <- "TRIMMED_DATA"
ss <- subset(all_trimmed_data, trans_name %in% top.transaction.list)


topD <- d_ply(ss, .(trans_name), function(df) {
  
  mtrc <- df$trans_name[1]
  
  df <- sort.data.frame(df,~+trans_name+trans_start_time)
  #openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/",  "LR_by_user_count",  mtrc, sep="_"), w=10, h=10, r=108, ps=14)
  openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/",  "LR", PLOT.DATA, "_by_user_count",  mtrc, sep="_"), w=10, h=10, r=108, ps=14)
  
  print(bwplot(trans_resp_time ~ user_count, data=df, 
               type='l', 
               #layout=c(1,1),
               as.table=TRUE,
               #strip=FALSE,
               #strip.left = TRUE,
               par.strip.text = list(cex = 1.1),
               par.settings=pp_ggplot2,
               
               #main=paste("SAMI/TAMI Load Test\n", mtrc, "\nBy User Count"),
               main=paste("SAMI/TAMI Load Test\n", mtrc, "\n", PLOT.DATA, "By User Count"),
               xlab="Number of Concurrent Users",
               ylab="Value\n**Observe scale limits**",
               axis = axis.grid,
               scales=list(y=list(relation="same",  rot=0, cex=1.1, alternating=3),
                           x = list(rot = 45, tick.number=24, cex=1.1))
  )
  )
  
  dev.off()
  
})
  

##---------------------------------------------------------------
## CONSOLIDATE TEST, BY TEST
##   BOX PLOTS OF METRICS BY TEST
##---------------------------------------------------------------
## ALL Servers

#topD <- d_ply(all_ss, .(trans_name), function(df) {
topD <- d_ply(ss, .(trans_name), function(df) {  
  
  mtrc <- df$trans_name[1]
  
  df <- sort.data.frame(df,~+trans_name+trans_start_time)
  openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/",  "LR", PLOT.DATA, "_by_test",  mtrc, sep="_"), w=10, h=10, r=108, ps=14)
  
  print(bwplot(trans_resp_time ~ test, data=df, 
               type='l', 
               #layout=c(1,1),
               as.table=TRUE,
               #strip=FALSE,
               #strip.left = TRUE,
               par.strip.text = list(cex = 1.1),
               par.settings=pp_ggplot2,
               
               #main=paste("SAMI/TAMI Load Test\n", mtrc, "\nBy User Count"),
               main=paste("SAMI/TAMI Load Test\n", mtrc, "\n", PLOT.DATA, "By Test"),
               xlab="Test, Number of Concurrent Users",
               ylab="Value\n**Observe scale limits**",
               axis = axis.grid,
               scales=list(y=list(relation="same",  rot=0, cex=1.1, alternating=3),
                           x = list(rot = 45, tick.number=24, cex=1.1))
  )
  )
  
  dev.off()
  
})


#################################### E N D ##############################################
#################################### E N D ##############################################
#################################### E N D ##############################################
t.ss <- subset(all_ss, combined.Metric == "Processor_Total.Processor_Time")

levels(t.ss$test)

lvl <- c("T1_38Users","T2_38Users","T3_57Users","T4_57Users","T5_76Users","T6_76Users","T7_95Users","T8_95Users",
         "T9_114Users","T10_114Users","T11_133Users","T12_152Users","T13_190Users") 
t.ss$test <- factor(t.ss$test, lvl)

mean.t.ss <- ddply(t.ss, .(server, test), function(df) {sao.func(df$value)[2]})

bwplot(value ~ test | server, data=t.ss, 
       type='l', 
       #layout=c(1,1),
       #panel=panel.average.on.bwplot,
       as.table=TRUE,
       #strip=FALSE,
       #strip.left = TRUE,
       par.strip.text = list(cex = .95),
       par.settings=pp_ggplot2,
       
       main=paste("SAMI/TAMI Load Test\n", "Processor_Total.Processor_Time", "\nBy Test"),
       xlab="Number of Concurrent Users",
       ylab="Value\n**Observe scale limits**",
       axis = axis.grid,
       scales=list(y=list(relation="same",  rot=0, cex=.7, alternating=3),
                   x = list(rot = 45, tick.number=24, cex=.7))
) +
  
  as.layer(xyplot(Mean ~ as.numeric(as.factor(test)) | server, data=mean.t.ss, type = 'p', col = 'red'), x.same = TRUE) +
  as.layer(xyplot(Mean ~ as.numeric(as.factor(test)) | server, data=mean.t.ss, type = 'l', col = 'red'), x.same = TRUE)


###---------------------------------------------------------------------------------
###  Report Load Runner Data
###     For each test cerate the summary reports for all the transactions 
###       in each of the test
###     Write the results to the test directory
###---------------------------------------------------------------------------------

p1 <- all_LR_data$trans_resp_time[all_LR_data$test == "T3_57Users" & all_LR_data$trans_name == "01_Distribution_Address_Search_01_search_Address"]
p2 <- all_LR_data$trans_resp_time[all_LR_data$test == "T8_95Users" & all_LR_data$trans_name == "01_Distribution_Address_Search_01_search_Address"]
length(p1)
length(p2)

sao.func(p1)
sao.func(p2)

t <- 

  
  t.test(p1,p2, alternative="two.sided", paired=FALSE, var.equal=FALSE, conf.level=.95)



str(t)


test.User.Count <- unique(as.character(all_trimmed_data$test))

base.Case <- test.User.Count[1]
#test.User.Count <- test.User.Count[2:length(test.User.Count)]
transaction.list <- unique(as.character(all_trimmed_data$trans_name))

transaction.list <- c("19_General_Temporal_02_click_Play",
                      "Load_TAMI",
                      "10_Transmission_Gas_Valves_03_zoom_5_mile",
                      "16_General_Disaster_Select_Sources_02_select_All_Sources",
                      "10_Transmission_Gas_Valves_02_show_Main_Class_Identification",
                      "20_General_Temporal_Forcasts_02_click_Play",
                      "12_Dispatch_Field_Order_02_zoom_1_mile",
                      "15_Dispatch_GSR_03_zoom_1_mile",
                      "13_Dispatch_Odor_Calls_02_zoom_1_mile",
                      "09_Transmission_Automated_Valves_02_zoom_35_miles",
                      "07_Transmission_Routes_03_View_PopUp",
                      "11_Dispatch_GSR_Locations_01_select_GSR_locations",
                      "05_Distribution_Regulator_Sta_Diagram_02_zoom_1_mile",
                      "02_Distribution_GEMs_Maps_Pan_01_select_GEMS_Map",
                      "03_Distribution_GEMs_Maps_Plat_Map_03_zoom_1000ft",
                      "04_Distribution_Regulator_Station_02_zoom_1_mile",
                      "06_Transmission_Stations_02_zoom_1_mile",
                      "03_Distribution_GEMs_Maps_Plat_Map_02_selecet_100scale",
                      "07_Transmission_Routes_01_select_Routes",
                      "01_Distribution_Address_Search_01_search_Address")

results <- list()
rm(result, df_results)
for (tc in test.User.Count) {
  cat("test case = ", tc, "\n")
  
  for (trn in transaction.list) {
    
    if (!is.na(trn)) {
      cat("    trans = ", trn, "\n")
      
      #base <- all_trimmed_data$trans_resp_time[all_trimmed_data$trans_name == trn & all_trimmed_data$user_count == base.Case]
      #p1 <- all_trimmed_data$trans_resp_time[all_trimmed_data$trans_name == trn & all_trimmed_data$user_count == tc]
      
      base <- all_trimmed_data$trans_resp_time[all_trimmed_data$trans_name == trn & all_trimmed_data$test == base.Case]
      p1 <- all_trimmed_data$trans_resp_time[all_trimmed_data$trans_name == trn & all_trimmed_data$test == tc]
      
      if (length(base) != 0 & length(p1) != 0) {
      
        t <- t.test(base, p1, alternative="two.sided", paired=FALSE, var.equal=FALSE, conf.level=.95)
        
        result <- data.frame(test=tc,trans_name=trn,low=t$conf.int[1], high=t$conf.int[2])
        
        if (exists("df_results")) {
          df_results <- rbind(df_results, result)
        } else {
          df_results <- result
        }
        
        results[[tc]][[trn]] <- t$conf.int
      }
      
    }
    
  }
  
}

df_results

plot.data <- subset(df_results, test == "T2_38Users")
dev.off()
#p <- par(mai=c(0,6,0,0), cex.lab=.3)
#ar(op)
plot(plot.data$low, plot.data$trans_name, type='p', xlim=c(min(plot.data$low), max(plot.data$high)), yaxt = 'n', las = 1)
points(plot.data$high, plot.data$trans_name, col='red')
abline(v=0, lwd=3)
axis(2, at=1:20, label=transaction.list, las =2, cex.lab=.3)
for (i in 1:20) {
  segments(plot.data$low[i], i, plot.data$high[i], i, col='green')
}





## EVERY TO EVERY
results <- list()
rm(result, df_results)
for (tc in test.User.Count) {
  cat("test case = ", tc, "\n")

  for (tc1 in test.User.Count) {
    cat("test case = ", tc, "\n")
    
    base.Case = tc1
    
    for (trn in transaction.list) {
      
      label <- paste0(tc1, "  compared to  ", tc)
      file_label <- paste0(tc1, "_vs_", tc)
      cat(" label = ", label, "\n")
      
      if (!is.na(trn)) {
        #cat("    trans = ", trn, "\n")
        
        #base <- all_trimmed_data$trans_resp_time[all_trimmed_data$trans_name == trn & all_trimmed_data$user_count == base.Case]
        #p1 <- all_trimmed_data$trans_resp_time[all_trimmed_data$trans_name == trn & all_trimmed_data$user_count == tc]
        
        base <- all_trimmed_data$trans_resp_time[all_trimmed_data$trans_name == trn & all_trimmed_data$test == base.Case]
        p1 <- all_trimmed_data$trans_resp_time[all_trimmed_data$trans_name == trn & all_trimmed_data$test == tc]
        
        if (length(base) != 0 & length(p1) != 0) {
          
          t <- t.test(base, p1, alternative="two.sided", paired=FALSE, var.equal=FALSE, conf.level=.90)
          #t <- t.test(p1, base,  alternative="two.sided", paired=FALSE, var.equal=FALSE, conf.level=.90)
          
          result <- data.frame(file_label=file_label, label=label, test=tc,trans_name=trn,low=t$conf.int[1], high=t$conf.int[2])
          
          if (exists("df_results")) {
            df_results <- rbind(df_results, result)
          } else {
            df_results <- result
          }
          
          results[[tc]][[trn]] <- t$conf.int
        }
        
      }
      
    }
  }
  
}

for (f in 1:length(df_results$file_label)) {
  cat(" CI PLOT FOR = ", df_results$file_label[f], "\n")
  plot.label <- df_results$file_label[f]
  
  openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/CI_plots/",  "LR", PLOT.DATA, "CI_Plots",  df_results$file_label[f], sep="_"), w=10, h=6, r=108, ps=14)
  #openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/CI_plots/",  "LR", PLOT.DATA, "CI_Plots_free_x_axis",  df_results$file_label[f], sep="_"), w=10, h=6, r=108, ps=14)
  print(segplot(trans_name ~ low + high, 
        xlim=c(-3,3),
        data = subset(df_results, file_label == plot.label),
        draw.bands = FALSE, centers=rep(0,20), 
        xlab="Confidence Interval, in seconds",
        main=paste0("Test Case Comparison\n", df_results$label[f])))
  dev.off()
}