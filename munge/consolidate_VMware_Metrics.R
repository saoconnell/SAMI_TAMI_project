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
require(Hmisc)
require(reshape)
require(lattice)
require(latticeExtra)
require(Cairo)
library(data.table)
library(reshape)
library(plyr)

##-------------
## LOAD SAOB SetUp functions
##-------------
setUpFunc_Dir <- c("c:\\Users\\SAOb\\Documents\\workspace\\R\\setUp\\")
setUpFunctions <- c('setupFunc.R')
source(paste(setUpFunc_Dir, setUpFunctions, sep=""))


##-------------
## READ DATA
##-------------
dev.off()

##DATA DIRECTORY
dataDir <- paste("C:/Users/SAOb/Documents/performance_data/20140624_SAMI/PERF_TESTS", sep="")

VMWare.DataDir <- paste("C:/Users/SAOb/Documents/performance_data/20140624_SAMI/PERF_TESTS/ROUND_2_QA/All_VMware_data/", sep="")

test_files <- list.files()

####  READ ALL THE RAW DATA INTO ON DATAFRAME
if (exists("data.list")) {rm(data.list)}

for (f in 1:length(test_files)) {
  
  if (grepl('csv$', test_files[f])) {
    cat("TEST FILE = ", test_files[f], "\n")
    
    vmware_data <- read.csv(test_files[f],  stringsAsFactors=FALSE, sep=',')
    
    if (!exists("data.list")) {
      ## header=FALSE, skip=1,
      data.list <- list()
      data.list[[test_files[f]]] <- vmware_data
    } else {
      data.list[[test_files[f]]] <- vmware_data
    }
    
  }
}

## CONSOLCIATE ALL THE VMWARE DATA
all_vm_data <- as.data.frame(rbindlist(data.list))

### CHECK THE STRCUTURE OF THE DATA FRAME
str(all_vm_data)

###  CONVERT THE TIMESTAMP TO A POSIX DATE
all_vm_data$POSIX <- as.POSIXct(strptime(as.character(all_vm_data$Timestamp), format="%m/%d/%Y %I:%M:%S %p"))

###  SAVE THE DATA
save(all_vm_data, file=paste0(VMWare.DataDir,"all_vm_data.Rdata"))


####
## REMOVE THE VM HOST gsmiappqa03
all_vm_data <- subset(all_vm_data, Entity != 'gsmiappqa03')


###------------------------------------------------------------------------------
###   CHARTS - PROCESS ALL THE DATA FOR EACH TEST
###------------------------------------------------------------------------------
pp_ggplot2 <- ggplot2like()
pp_ggplot2$strip.border      <- list(col="grey60")
pp_ggplot2$axis.line         <- list(col="grey60")
pp_ggplot2$axis.text         <- list(col="black")

output <- "png"


## READ THE TEST DETAILS
test_details <- read.csv(paste0(dataDir, "test_details.csv"), header=TRUE, stringsAsFactor=FALSE)
str(test_details)
test_details[5,]

esx.metrics <- c("cpu.usage.average",
                  "cpu.ready.summation",
                  "mem.usage.average",
                  "disk.usage.average",
                  "net.usage.average",
                  "mem.swapinRate.average",
                  "mem.swapoutRate.average",
                  "disk.maxTotalLatency.latest")

vm.metrics  <- c("cpu.ready.summation",
                  "mem.usage.average",
                  "mem.vmmemctl.average",
                  "mem.consumed.average",
                  "mem.overhead.average",
                  "disk.usage.average",
                  "net.usage.average",
                  "disk.maxTotalLatency.latest",
                  "disk.numberReadAveraged.average",
                  "disk.numberWriteAveraged.average")


esx.hosts <- grep("esx", unique(all_vm_data$Entity), value=TRUE)
vm.hosts <- grep("esx", unique(all_vm_data$Entity), value=TRUE, invert=TRUE)




setwd(VMWare.DataDir)

TEST_TYPE <- "ROUND_2_QA"

test_details <- subset(test_details, type == TEST_TYPE)

for (t in 1:length(test_details$test)) {
  
  test.Data.Dir <- paste0(dataDir, "/", TEST_TYPE, "/", test_details$test[t], "/", "VMWARE/")
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
  
  ss <- subset(all_vm_data, POSIX >= startTime & POSIX <= endTime)
  
  ## ALL ESX DATA
  esx.ss <- subset(ss, Entity %in% esx.hosts)
  
  topD <- d_ply(esx.ss, .(MetricId), function(df) {
    
    mtrc <- df$MetricId[1]
    
    df <- sort.data.frame(df,~+Entity+POSIX)
    openGdevice(type=output, file=paste(graphDir, TEST_TYPE, test_details$test[t], "_ESX_",  mtrc, sep="_"), w=10, h=10, r=108, ps=14)
    
    print(xyplot(Value ~ POSIX | Entity, data=df, 
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


  
  ## ALL VM DATA
  vm.ss <- subset(ss, Entity %in% vm.hosts & Instance == "")
  
  topD <- d_ply(vm.ss, .(MetricId), function(df) {
    
    mtrc <- df$MetricId[1]
    
    df <- sort.data.frame(df,~+Entity+POSIX)
    openGdevice(type=output, file=paste(graphDir, TEST_TYPE, test_details$test[t], "_VM_",  mtrc, sep="_"), w=10, h=10, r=108, ps=14)
    
    print(xyplot(Value ~ POSIX | Entity, data=df, 
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
  
  test.Data.Dir <- paste0(dataDir, "/", TEST_TYPE, "/", test_details$test[t], "/", "VMWARE/")
  
  
  ## SUBSET THE DATA BY RUN DATE TIME
  runTime <-  test_details$time[t]
  
  ###   CREATE A START AND END FOR SUBSETTING THE DATA AROUND A SPECIFIC TEST, SUBSTRACT 20 MINUTES
  ###     THE START OF THE TEST AND ADD 2O MINUTES TO THE END OF THE TEST
  startTime <- as.POSIXct(strptime(substring(runTime,1,19), format="%d/%m/%Y %H:%M:%S")) + (test_details$warmup[t] * 60)
  endTime <- startTime + (test_details$duration[t] * 60)
  cat("start_time = ", as.POSIXct(startTime, origin = "1970-01-01"), " end_time = ", as.POSIXct(endTime, origin = "1970-01-01"), " test time = ", test_details$time[t], "\n")
  print(as.POSIXct(startTime, origin = "1970-01-01"))
  print(as.POSIXct(endTime, origin = "1970-01-01"))
  SS[[test_details$test[t]]] <- subset(all_vm_data, POSIX >= startTime & POSIX <= endTime)
  
  SS[[test_details$test[t]]] <- cbind(test=test_details$test[t], 
                                      user_count=strsplit(test_details$test[t], "_")[[1]][2], 
                                      SS[[test_details$test[t]]])
  

} 

all_ss <- as.data.frame(rbindlist(SS))
str(SS[[test_details$test[t]]])

##---------------------------------------------------------------
## CONSOLIDATE TEST, BY USER COUNT
##   BOX PLOTS OF METRICS BY ESX HOST
##---------------------------------------------------------------
## ALL ESX DATA

esx.ss <- subset(all_ss, Entity %in% esx.hosts)

topD <- d_ply(esx.ss, .(MetricId), function(df) {
  
  mtrc <- df$MetricId[1]
  
  df <- sort.data.frame(df,~+Entity+POSIX)
  openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/",  "all_ESX_by_user_count",  mtrc, sep="_"), w=10, h=10, r=108, ps=14)
  
  print(bwplot(Value ~ user_count | Entity, data=df, 
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
  

##---------------------------------------------------------------
## CONSOLIDATE TEST, BY USER COUNT
##   BOX PLOTS OF METRICS BY ESX HOST
##---------------------------------------------------------------
## ALL ESX DATA

vm.ss <- subset(all_ss, Entity %in% vm.hosts)

topD <- d_ply(vm.ss, .(MetricId), function(df) {
  
  mtrc <- df$MetricId[1]
  
  df <- sort.data.frame(df,~+Entity+POSIX)
  openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/",  "all_VM_by_user_count",  mtrc, sep="_"), w=10, h=10, r=108, ps=14)
  
  print(bwplot(Value ~ user_count | Entity, data=df, 
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


##---------------------------------------------------------------
## CONSOLIDATE TEST, BY TEST
##   BOX PLOTS OF METRICS BY ESX HOST
##---------------------------------------------------------------
## ALL ESX DATA

esx.ss <- subset(all_ss, Entity %in% esx.hosts)

topD <- d_ply(esx.ss, .(MetricId), function(df) {
  
  mtrc <- df$MetricId[1]
  
  df <- sort.data.frame(df,~+Entity+POSIX)
  openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/",  "all_ESX_by_test",  mtrc, sep="_"), w=10, h=10, r=108, ps=14)
  
  print(bwplot(Value ~ test | Entity, data=df, 
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


##---------------------------------------------------------------
## CONSOLIDATE TEST, BY TEST
##   BOX PLOTS OF METRICS BY VM HOST
##---------------------------------------------------------------
## ALL VM DATA

vm.ss <- subset(all_ss, Entity %in% vm.hosts)

topD <- d_ply(vm.ss, .(MetricId), function(df) {
  
  mtrc <- df$MetricId[1]
  
  df <- sort.data.frame(df,~+Entity+POSIX)
  openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/",  "all_VM_by_test",  mtrc, sep="_"), w=10, h=10, r=108, ps=14)
  
  print(bwplot(Value ~ test | Entity, data=df, 
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


#################################### E N D ##############################################
#################################### E N D ##############################################
#################################### E N D ##############################################

### METRICS BOX PLOT BY TEST
## BY TEST

vm.ss <- subset(ss, Entity %in% vm.hosts & Instance == "")

topD <- d_ply(vm.ss, .(MetricId), function(df) {
  
  mtrc <- df$MetricId[1]

  
  df <- sort.data.frame(df,~+Entity+POSIX)
  openGdevice(type=output, file=paste(graphDir, TEST_TYPE, test_details$test[t], "_VM_",  mtrc, sep="_"), w=10, h=10, r=108, ps=14)
  
  print(bwplot(Value ~ test, data=df, 
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




















min(all_vm_data$POSIX)

###  RUN TIME TAKEN FROM THE LOAD RUNNER REPORT
runTime <-  "27/07/2012 16:00:00 - 27/07/2012 20:00:00"

###   CREATE A START AND END FOR SUBSETTING THE DATA AROUND A SPECIFIC TEST, SUBSTRACT 20 MINUTES
###     THE START OF THE TEST AND ADD 2O MINUTES TO THE END OF THE TEST
startTime <- as.POSIXct(strptime(substring(runTime,1,19), format="%d/%m/%Y %H:%M:%S")) - (20 * 60)
endTime <- as.POSIXct(strptime(substring(runTime,23,41), format="%d/%m/%Y %H:%M:%S")) + (20 * 60)

startTime <- min(all_vm_data$POSIX)
endTime   <- max(all_vm_data$POSIX)

###   SUBSET ON THE START AND END TIMES OF THE TEST
ss <- subset(all_vm_data, (POSIX >= startTime & POSIX <= endTime))

ss$Timestamp <- as.character(ss$Timestamp)

ss <- sort.data.frame(ss,~+POSIX)

max(ss$POSIX)

levels(ss$MetricId)
str(ss)


##-----------------
## PLOT - by server, variable
##-----------------

ss_1 <- subset(ss)
ss_1$MetricID <- factor(ss_1$MetricId)
ss_1$Instance <- factor(ss_1$Instance)

topD <- d_ply(ss_1, .(MetricId), function(df) {
  
  mtrc <- df$MetricId[1]
  
  openGdevice(type=output, file=paste(graphDir, analysisDateTime, '_SERVER_',  mtrc, sep=""), w=10, h=10, r=108, ps=14)
  
  print(xyplot(Value ~ POSIX | Entity, data=df, 
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
dev.off()


##-----------------
## PLOT - ALL SERVERS, Instance Data 
##-----------------

ss_1 <- subset(ss)
ss_1$MetricID <- factor(ss_1$MetricId)
ss_1$Instance <- factor(ss_1$Instance)

ss_2 <- subset(ss_1, Instance != "")

topD <- d_ply(ss_2, .(Instance), function(df) {
  
  Inst <- df$Instance[1]
  
  topD <- d_ply(df, .(MetricId), function(df1) {
    
    mtrc <- df1$MetricId[1]
    
    openGdevice(type=output, file=paste(graphDir, analysisDateTime, '_INSTANCE_SERVER_',  Inst, '_', mtrc, sep=""), w=10, h=10, r=108, ps=14)
    
    print(xyplot(Value ~ POSIX | Entity, data=df1, 
                 type='l', 
                 #layout=c(1,1),
                 as.table=TRUE,
                 #strip=FALSE,
                 #strip.left = TRUE,
                 par.strip.text = list(cex = .95),
                 par.settings=pp_ggplot2,
                 main=paste("Instance:", Inst, "\nMetric:", mtrc),
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
})

dev.off()




##-----------------
## PLOT - by variable, server
##-----------------
ss_2 <- subset(ss, MetricId %in% c("cpu.usage.average","disk.usage.average","datastore.totalwritelatency.average","datastore.totalreadlatency.average","net.usage.average"))

ss_2$MetricId <- as.character(ss_2$MetricId)

### FINE TUNE THE AMOUNTS TO ADJUST THE SCALE ON THE CHARTS
#describe(ss_2$Value[ss_2$MetricId == "disk.maxtotallatency.latest"])
#quantile(ss_2$Value[ss_2$MetricId == "disk.maxtotallatency.latest"], prob=.95) * 1.15
#
#ss_2$Value[ss_2$MetricId == "disk.maxtotallatency.latest" & ss_2$Value > 20] <- 20

ss_2$MetricID <- factor(ss_2$MetricId, levels=c("cpu.usage.average","disk.usage.average","datastore.totalwritelatency.average","datastore.totalreadlatency.average","net.usage.average"))


topD <- d_ply(ss_2, .(Entity), function(df) {
  
  varb <- df$MetricId[1]
  srv <- df$Entity[1]
  unit <- df$Unit[1]
  openGdevice(type=output, file=paste(graphDir, analysisDateTime, '_by_server_by_group_of_metrics_', srv, sep=""), w=10, h=10, r=108, ps=14)
  
  print(xyplot(Value ~ POSIX | MetricId, data=df, 
               type='l', 
               layout=c(1,5),
               as.table=TRUE,
               strip=FALSE,
               strip.left = TRUE,
               par.strip.text = list(cex = .95),
               par.settings=pp_ggplot2,
               main=paste(srv),
               sub=paste(startTime, " to ", endTime),
               xlab="Time",
               ylab="Value\n**Observe scale limits**",
               axis = axis.grid,
               scales=list(y=list(relation='free', rot=0, cex=.7, alternating=3),
                           x = list(rot = 45, tick.number=24, cex=.7))
  )
  )
  
  dev.off()
  
})


##-----------------
## PLOT - by variable, server
##-----------------
ss_3 <- subset(ss, MetricId %in% c("cpu.ready.summation","mem.usage.average","cpu.usage.average","virtualdisk.numberreadaveraged.average",
                                   "virtualdisk.numberwriteaveraged.average",
                                   "virtualdisk.totalreadlatency.average",
                                   "virtualdisk.totalwritelatency.average",
                                   "datastore.numberreadaveraged.average",   
                                   "datastore.numberwriteaveraged.average",  
                                   "datastore.totalreadlatency.average",     
                                   "datastore.totalwritelatency.average",    
                                   "disk.maxtotallatency.latest",            
                                   "disk.numberreadaveraged.average",        
                                   "disk.numberwriteaveraged.average"))

ss_3$MetricID <- factor(ss_3$MetricId, c("cpu.ready.summation","mem.usage.average","cpu.usage.average","virtualdisk.numberreadaveraged.average",
                                         "virtualdisk.numberwriteaveraged.average",
                                         "virtualdisk.totalreadlatency.average",
                                         "virtualdisk.totalwritelatency.average",
                                         "datastore.numberreadaveraged.average",   
                                         "datastore.numberwriteaveraged.average",  
                                         "datastore.totalreadlatency.average",     
                                         "datastore.totalwritelatency.average",    
                                         "disk.maxtotallatency.latest",            
                                         "disk.numberreadaveraged.average",        
                                         "disk.numberwriteaveraged.average"))

topD <- d_ply(ss_3, .(Entity), function(df) {
  
  
  topD <- d_ply(df, .(MetricId), function(df) {
    
    srv <- df$Entity[1]
    metric <- df$MetricId[1]
    unit <- df$Unit[1]
    
    
    
    openGdevice(type=output, file=paste(graphDir, analysisDateTime, '_', srv, "_", metric, sep=""), w=10, h=5.5, r=108, ps=14)
    
    print(xyplot(Value ~ POSIX | Instance, data=df, 
                 type='l', 
                 #layout=c(1,1),
                 as.table=TRUE,
                 #strip=FALSE,
                 #strip.left = TRUE,
                 par.strip.text = list(cex = .8),
                 par.settings=pp_ggplot2,
                 main=paste(srv, "\n", metric),
                 sub=paste(startTime, " to ", endTime),
                 xlab="Time",
                 ylab=paste(unit),
                 axis = axis.grid,
                 scales=list(y=list(rot=0, cex=.5, alternating=3),
                             x = list(rot = 45, tick.number=24, cex=.85))
    )
    )
    
    dev.off()
    
  })
  
})
dev.off()
