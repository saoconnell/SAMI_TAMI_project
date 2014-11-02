###---------------------------------------------------------
###   consolidate_VM_metrics: 
###      - Reads the raw data from VMware VIrtual center
###      - Consolidates the data into one dataframe
###      - produces a graphics
###         - by metric, by server
###         - by server, by metric
###
###   Author: Stephen O'Connell
###   Date: 07/03/2012
###   Description:
###   Change History: (why is always just because...)
###        Who         When            What
###       SAOB        07/03/2012      Initial development
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


#####-------------------------------------------------------------------
#####  CREATE PERFMON VARIABLES FROM THE HEADER
#####-------------------------------------------------------------------

create_perfmon_melt_DF <- function(filename) {
  
  create_perfmon_header <- function(filename) {
    
    H <- read.csv(filename, header=FALSE, stringsAsFactors=FALSE, nrow=1)
    Hl <- as.list(H[1,2:length(H)])
    
    Ht <- sapply(Hl, function(x) strsplit(x, '\\\\'))
    tmpH <- do.call('rbind', Ht)
    
    header_cleanup <- function(h) {
      h_3 <- gsub("\\.", "_", h[3])
      
      #h_4 <- gsub("\\(Intel\\[R\\] PRO_1000 MT Network Connection", "", h[4])
      h_4 <- gsub("BCM5709C NetXtreme II GigE \\[NDIS VBD Client\\]", "", h[4])
      h_4 <- gsub(" ", "_", h_4)
      h_4 <- gsub("\\-", "_", h_4)
      h_4 <- gsub("[)(]", "", h_4)
      h_4 <- gsub("\\[", "", h_4)
      h_4 <- gsub("\\]", "", h_4)
      h_4 <- gsub(":", "_", h_4)
      
      h_5 <- gsub("/", "_", h[5])
      h_5 <- gsub("^% ", "", h_5)
      h_5 <- gsub(" ", "_", h_5)
      h_5 <- gsub("\\-", "_", h_5)
      h_5 <- gsub("\\[", "", h_5)
      h_5 <- gsub("\\]", "", h_5)
      h_5 <- gsub("\\*", "_", h_5)
      h_5 <- gsub("\\.", "", h_5)
      
      rc <- paste(h_3, h_4, h_5, sep='.')
      return(rc)
      
    }
    
    n <- vector()
    
    for (indx in 1:nrow(tmpH)) {
      n[indx] <- header_cleanup(tmpH[indx,])
    }
    
    names <- c("date_time", n)
    return(names)
    
  }
  
  header <- create_perfmon_header(filename)
  
  
  D <- read.csv(filename, header=FALSE, skip=2, stringsAsFactors=FALSE, na.strings = " ")

  D[is.na(D)] <- 0
    
  names(D) <- header
  
  mdf <- melt(D, ("date_time")) 
  
  #mdf[is.na[mdf]] <- 0
  
  mdf[is.na(mdf)] <- 0
  
  mdf$value <- as.numeric(as.character(mdf$value))
    
  mdf$variable <- as.character(mdf$variable)
  
  system.time(mdf$server <- sapply(mdf$variable, function(x) {strsplit(x, "\\.")[[1]][1]}))
  system.time(mdf$metric_group <- sapply(mdf$variable, function(x) {strsplit(x, "\\.")[[1]][2]}))
  system.time(mdf$metric <- sapply(mdf$variable, function(x) {strsplit(x, "\\.")[[1]][3]}))
  system.time(mdf$POSIX <- perfmon_date2POSIX(mdf$date_time))

  return(mdf)
  
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
  dataDir <- paste("C:/Users/SAOb/Documents/performance_data/20140624_SAMI/PERF_TESTS/", TEST_TYPE, "/", test_details$test[t], "/PERFMON/", sep="")
  
  setwd(dataDir)
  
  cat("\nDATA DIR = ", dataDir, "\n")

  test_files <- list.files(recursive = TRUE)  
  print(test_files)
  
  for (f in 1:length(test_files)) {
  
    if (grepl('perfmon.csv$', test_files[f])) {
      cat("TEST FILE = ", test_files[f], "\n")
      
      temp <- create_perfmon_melt_DF(test_files[f])
      temp <- cbind(test=test_details$test[t], 
                    type=test_details$type[t], 
                    temp)
      
      server <- unique(temp$server)
      
      if (!exists("data.list")) {
        data.list <- list()  
        data.list[[paste0(test_details$test[t], "_", test_details$type[t], "_", server)]] <- temp
      } else {
        data.list[[paste0(test_details$test[t], "_", test_details$type[t], "_", server)]] <- temp
      }
    }
  }
}

## CONSOLCIATE ALL THE VMWARE DATA
all_perfmon_data <- as.data.frame(rbindlist(data.list))

### CHECK THE STRCUTURE OF THE DATA FRAME
str(all_perfmon_data)
unique(all_perfmon_data$server)
###  SAVE THE DATA
save(all_perfmon_data, file=paste0(rootDir, TEST_TYPE, "/all_perfmon_data.Rdata"))
#load(file=paste0(rootDir, TEST_TYPE, "/all_perfmon_data.Rdata"))




###------------------------------------------------------------------------------
###   CHARTS - PROCESS ALL THE DATA FOR EACH TEST
###------------------------------------------------------------------------------
pp_ggplot2 <- ggplot2like()
pp_ggplot2$strip.border      <- list(col="grey60")
pp_ggplot2$axis.line         <- list(col="grey60")
pp_ggplot2$axis.text         <- list(col="black")

output <- "png"

unique(all_perfmon_data$variable)


for (t in 1:length(test_details$test)) {
  
  test.Data.Dir <- paste0(rootDir, "/", TEST_TYPE, "/", test_details$test[t], "/", "PERFMON/")
  graphDir <- paste(test.Data.Dir, "graphics/", sep="")
  cat("this is the test case = ", test.Data.Dir, "\n")
  
  if(!file.exists(graphDir)) {
  
    dir.create(graphDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  
  }
  
  ## SUBSET THE DATA BY RUN DATE TIME
  runTime <-  test_details$time[t]
  
  ###   CREATE A START AND END FOR SUBSETTING THE DATA AROUND A SPECIFIC TEST, SUBSTRACT 20 MINUTES
  ###     THE START OF THE TEST AND ADD 2O MINUTES TO THE END OF THE TEST
  startTime <- as.POSIXct(strptime(substring(runTime,1,19), format="%d/%m/%Y %H:%M:%S")) - (20 * 60)
  endTime <- as.POSIXct(strptime(substring(runTime,23,41), format="%d/%m/%Y %H:%M:%S")) + (20 * 60)
  
  ss <- subset(all_perfmon_data, POSIX >= startTime & POSIX <= endTime)
  str(ss)
    
  topD <- d_ply(ss, .(variable), function(df) {
    
    mtrc <- df$variable[1]
    
    df <- sort.data.frame(df,~+server+POSIX)
    openGdevice(type=output, file=paste(graphDir, TEST_TYPE, test_details$test[t], "_", server, "_",  mtrc, sep="_"), w=10, h=10, r=108, ps=14)
    
    print(xyplot(value ~ POSIX | server, data=df, 
                 type='l', 
                 #layout=c(1,1),
                 as.table=TRUE,
                 #strip=FALSE,
                 #strip.left = TRUE,
                 par.strip.text = list(cex = .95),
                 par.settings=pp_ggplot2,
                 
                 main=paste(mtrc),
                 sub=paste(startTime, " to ", endTime),
                 xlab="Time",
                 ylab="Value\n**Observe scale limits**",
                 axis = axis.grid,
                 scales=list(y=list(relation="same",  rot=0, cex=.7, alternating=3),
                             x = list(rot = 45, tick.number=24, cex=.7))
      )
    )
    
    dev.off()
    
  })

}


###------------------------------------------------------------------------------
###  Compare Metrics Across tests, need to reduce samples by warm-up time and
###     trim to a specific duration
###------------------------------------------------------------------------------
###############################

SS <- list()

for (t in 1:length(test_details$test)) {
  
  
  ## SUBSET THE DATA BY RUN DATE TIME
  runTime <-  test_details$time[t]
  
  ###   CREATE A START AND END FOR SUBSETTING THE DATA AROUND A SPECIFIC TEST, SUBSTRACT 20 MINUTES
  ###     THE START OF THE TEST AND ADD 2O MINUTES TO THE END OF THE TEST
  startTime <- as.POSIXct(strptime(substring(runTime,1,19), format="%d/%m/%Y %H:%M:%S")) + (test_details$warmup[t] * 60)
  endTime <- startTime + (test_details$duration[t] * 60)
  cat("start_time = ", as.POSIXct(startTime, origin = "1970-01-01"), " end_time = ", as.POSIXct(endTime, origin = "1970-01-01"), " test time = ", test_details$time[t], "\n")
  print(as.POSIXct(startTime, origin = "1970-01-01"))
  print(as.POSIXct(endTime, origin = "1970-01-01"))
  
  #paste0(test_details$test[t], "_", test_details$type[t], "_", server)
  SS[[test_details$test[t]]] <- subset(all_perfmon_data, POSIX >= startTime & POSIX <= endTime)
  
  SS[[test_details$test[t]]] <- cbind(test=test_details$test[t], 
                                      user_count=strsplit(test_details$test[t], "_")[[1]][2], 
                                      SS[[test_details$test[t]]])

} 

all_ss <- as.data.frame(rbindlist(SS))

all_ss$combined.Metric <- paste0(all_ss$metric_group, ".", all_ss$metric)
unique(all_ss$combined.Metric)


lvl <- c("T1_38Users","T2_38Users","T3_57Users","T4_57Users","T5_76Users","T6_76Users","T7_95Users","T8_95Users",
         "T9_114Users","T10_114Users","T11_133Users","T12_152Users","T13_190Users") 
all_ss$test <- factor(all_ss$test, lvl)

##---------------------------------------------------------------
## CONSOLIDATE TEST, BY USER COUNT
##   BOX PLOTS OF METRICS BY ESX HOST
##---------------------------------------------------------------
## ALL servers

#ss <- subset(all_ss, Entity %in% esx.hosts)

topD <- d_ply(all_ss, .(combined.Metric), function(df) {
  
  mtrc <- df$combined.Metric[1]
  
  df <- sort.data.frame(df,~+server+POSIX)
  openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/",  "perfmon_SERVERS_by_user_count",  mtrc, sep="_"), w=10, h=10, r=108, ps=14)
  
  print(bwplot(value ~ user_count | server, data=df, 
               type='l', 
               #layout=c(1,1),
               as.table=TRUE,
               #strip=FALSE,
               #strip.left = TRUE,
               par.strip.text = list(cex = .95),
               par.settings=pp_ggplot2,
               
               main=paste("SAMI/TAMI Load Test\n", mtrc, "\nBy User Count"),
               xlab="Number of Concurrent Users",
               ylab="Value\n**Observe scale limits**",
               axis = axis.grid,
               scales=list(y=list(relation="same",  rot=0, cex=.7, alternating=3),
                           x = list(rot = 45, tick.number=24, cex=.7))
  )
  )
  
  dev.off()
  
})
  

##---------------------------------------------------------------
## CONSOLIDATE TEST, BY TEST
##   BOX PLOTS OF METRICS BY SERVER
##---------------------------------------------------------------
## ALL Servers

topD <- d_ply(all_ss, .(combined.Metric), function(df) {
  
  mtrc <- df$combined.Metric[1]
  
  df <- sort.data.frame(df,~+server+POSIX)
  openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/",  "all_SERVERS_by_test",  mtrc, sep="_"), w=10, h=10, r=108, ps=14)

  mean.df <- ddply(df, .(server, test), function(df) {sao.func(df$value)[2]})
  
#   my.key <- simpleKey(title=mtrc,
#                  text="Metric Mean",
#                  space="bottom",
#                  points=list(pch=c(20), col=c('red')),
#                  col='red',
#                  cex.title=.7, cex=1)
  
  print(bwplot(value ~ test | server, data=df, 
         type='l', 
         #layout=c(1,1),
         as.table=TRUE,
         #strip=FALSE,
         #strip.left = TRUE,
         par.strip.text = list(cex = .95),
         par.settings=pp_ggplot2,
         
         main=paste("SAMI/TAMI Load Test\n", mtrc, "\nBy Test"),
         xlab="Number of Concurrent Users",
         ylab="Value\n**Observe scale limits**",
         axis = axis.grid,
         #key = my.key,
         scales=list(y=list(relation="same",  rot=0, cex=.7, alternating=3),
                     x = list(rot = 45, tick.number=24, cex=.7))
    ) +
    
    as.layer(xyplot(Mean ~ as.numeric(as.factor(test)) | server, data=mean.df, type = 'p', col = 'red'), x.same = TRUE) +
    as.layer(xyplot(Mean ~ as.numeric(as.factor(test)) | server, data=mean.df, type = 'l', col = 'red'), x.same = TRUE)
    )
  
  
  dev.off()
  
})



#   print(bwplot(value ~ test | server, data=df, 
#                type='l', 
#                #layout=c(1,1),
#                as.table=TRUE,
#                #strip=FALSE,
#                #strip.left = TRUE,
#                par.strip.text = list(cex = .95),
#                par.settings=pp_ggplot2,
#                
#                main=paste("SAMI/TAMI Load Test\n", mtrc, "\nBy Test"),
#                xlab="Number of Concurrent Users",
#                ylab="Value\n**Observe scale limits**",
#                axis = axis.grid,
#                scales=list(y=list(relation="same",  rot=0, cex=.7, alternating=3),
#                            x = list(rot = 45, tick.number=24, cex=.7))
#   )
#   )

#################################### E N D ##############################################
#################################### E N D ##############################################
#################################### E N D ##############################################
t.ss <- subset(all_ss, combined.Metric == "Processor_Total.Processor_Time")

levels(t.ss$test)

