###---------------------------------------------------------
###   healthchek_PerfMon:  ANALYSIS AND PLOTTING
###   Author: Stephen O'Connell
###   Date: MM/DD/YYYY
###   Description:
###   Change History: (why is always just because...)
###        Who         When            What
###
###---------------------------------------------------------

##-----------------------------------------------------------------------------------------
rm(list=ls(all.names = TRUE))
gc()
memory.limit(size=3072)
memory.size(max=TRUE)

require(reshape)
require(lattice)
require(latticeExtra)
require(Hmisc)
require(Cairo)

##-------------
## LOAD SAOB SetUp functions
##-------------
setUpFunc_Dir <- c("c:\\Users\\SAOb\\Documents\\workspace\\R\\setUp\\")
setUpFunctions <- c('setupFunc.R')
source(paste(setUpFunc_Dir, setUpFunctions, sep=""))
## CLEANUP

##-------------
## READ DATA
##-------------
dev.off()

rootDir <- paste("C:/Users/SAOb/Documents/performance_data/20140624_SAMI/PERF_TESTS/", sep="")

TEST_TYPE <- "ROUND_2_QA"
application <- "SAMI_TAMI"
## READ THE TEST DETAILS

test_details <- read.csv(paste0(rootDir, "test_details.csv"), header=TRUE, stringsAsFactor=FALSE)
test_details <- subset(test_details, type == TEST_TYPE)


load(file=paste0(rootDir, TEST_TYPE, "/all_perfmon_data.Rdata"))
str(all_perfmon_data)

unique(all_perfmon_data$test)

TEST_CASE <- "T13_190Users" 

##-------------
## READ DATA
##-------------

dataDir <- (paste0(rootDir, TEST_TYPE, "/", TEST_CASE, "/PERFMON"))
graphDir <- (paste0(dataDir,  "/graphics", '/'))

dir.create(graphDir, showWarnings = TRUE, recursive = FALSE)


setwd(dataDir)



in_data <- subset(all_perfmon_data, test == TEST_CASE)

str(in_data)

##########---------------------------------------------------------------------------------------
##########------------           CLEAN UP THE DATA                             -----------------
##########---------------------------------------------------------------------------------------
#
### READ THE SERVER LIST
## CLEAN THE IP ADDRESS


in_data$server <- gsub("_", ".", in_data$server)

in_data$grp_metric <- paste(in_data$metric_group, in_data$metric, sep="--")

##########---------------------------------------------------------------------------------------
##########------------           FILTER UP THE DATA                             -----------------
##########---------------------------------------------------------------------------------------

health_check = unique(in_data$grp_metric)
# health_check <- c("System--Processor_Queue_Length",
# 		"Processor_Total--Processor_Time",
# 		"PhysicalDisk_Total--Disk_Time",
# 		"PhysicalDisk_Total--Avg_Disk_Queue_Length",
# 		"Network_Interface__2--Bytes_Total_sec",
# 		"Network_Interface--Bytes_Total_sec",
# 		"Network_Interface__2--Output_Queue_Length",
# 		"Network_Interface--Output_Queue_Length",
# 		"Memory--Pages_sec",
# 		"Memory--Available_MBytes",
# 		"SQLServer_Buffer_Manager--Buffer_cache_hit_ratio")
# 
# "Network_InterfaceVMware_PCI_Ethernet_Adapter__2--Bytes_Received_sec",
# "Network_InterfaceVMware_PCI_Ethernet_Adapter--Bytes_Received_sec",
# "Network_InterfaceVMware_PCI_Ethernet_Adapter__2--Bytes_Sent_sec",
# "Network_InterfaceVMware_PCI_Ethernet_Adapter--Bytes_Sent_sec",
# "Processor_Total--Processor_Time",


##2013-03-20 12:38:00 AM
#start_time <- as.POSIXct("2013-03-19 21:38:00")
##2013-03-20 2:55:00 AM
#end_time   <- as.POSIXct("2013-03-19 23:55:00")
start_time <- min(in_data$POSIX)
end_time   <- max(in_data$POSIX)


in_data <- subset(in_data, (POSIX >= start_time & POSIX <= end_time) & grp_metric %in% health_check & !grepl("template", variable))


##########---------------------------------------------------------------------------------------
##########------------           CODE USED TO ANNOTATE THE METRICS              -----------------
##########---------------------------------------------------------------------------------------
getwd()
server_list <- read.csv(paste0(rootDir, application, "_server_reference_list.TXT"), stringsAsFactors=FALSE)
str(server_list)

in_data$server_name <- ''
in_data$funct <- ''
in_data$location <- ''
in_data$cpu_cnt <- ''
in_data$mem_gb  <- ''
in_data$os  <- ''
in_data$storage  <- ''

in_data$server <- as.factor(in_data$server)
str(in_data)


system.time(fix_table <- ddply(in_data, .(server), function(df) {
  
  srv_name <- df$server[1]
  df$server_name <- server_list$Name[server_list$Name == srv_name]
  df$funct <- server_list$Function[server_list$Name == srv_name]
  df$location <- server_list$Location[server_list$Name == srv_name]
  df$cpu_cnt <- server_list$cpus[server_list$Name == srv_name]
  df$mem_gb <- server_list$memoryGB[server_list$Name == srv_name]
  df$os <- server_list$OS[server_list$Name == srv_name]
  df$storage <- server_list$storage[server_list$Name == srv_name]
  cat("server_name ", server_list[server_list$Name == srv_name,2], "  server_ip = ", srv_name, "\n")
  return(df)
  
}) )

str(fix_table)
in_data <- fix_table


##########---------------------------------------------------------------------------------------
##########------------           CODE TO SUMMARIZE TYPES OF DATA                -----------------
##########---------------------------------------------------------------------------------------

options(stringsAsFactors = FALSE)

### NETWORK SUMMARY
if(exists("network")) {rm(network)}

network <- ddply(subset(in_data, metric == 'Bytes_Received_sec' | metric == 'Bytes_Sent_sec'), .(POSIX, server), function(df) {
  date_time <- df$date_time[1]
  variable <- 'Network_Total_Bytes'
  
  value <- sum(df$value)
  
  test       <- as.character(df$test)[1]
  type       <- as.character(df$type)[1]
  server       <- as.character(df$server)[1]
  metric_group <- 'Network_Total_Bytes'
  metric       <- 'Network_Total_Bytes'
  POSIX        <- df$POSIX[1]
  grp_metric   <- 'Network_Total_Bytes'
  server_name  <- df$server_name[1]
  funct        <- df$funct[1]
  location     <- df$location[1]
  cpu_cnt      <- df$cpu_cnt[1]
  mem_gb       <- df$mem_gb[1]
  os           <- df$os[1]
  storage      <- df$storage[1]
  
  row <- cbind(test, type, date_time, variable, value, server, metric_group, metric, POSIX, grp_metric, server_name, funct, location, cpu_cnt, mem_gb, os, storage)
  
  return(row)
  
})

network$value <- as.numeric(network$value)
network$POSIX <- perfmon_date2POSIX(network$date_time)

in_data <- rbind(in_data,network)


##### DSIK SUMMARY
if(exists("disk_IO")) {rm(disk_IO)}

disk_IO <- ddply(subset(in_data, metric == "Disk_Reads_sec" | metric == "Disk_Writes_sec"), .(POSIX, server), function(df) {
  date_time <- df$date_time[1]
  variable <- 'Total_Disk_IO'
  
  value <- sum(df$value)

  test       <- as.character(df$test)[1]
  type       <- as.character(df$type)[1]
  
  server       <- as.character(df$server)[1]
  metric_group <- 'Total_Disk_IO'
  metric       <- 'Total_Disk_IO'
  POSIX        <- df$POSIX[1]
  grp_metric   <- 'Total_Disk_IO'
  server_name  <- df$server_name[1]
  funct        <- df$funct[1]
  location     <- df$location[1]
  cpu_cnt      <- df$cpu_cnt[1]
  mem_gb       <- df$mem_gb[1]
  os           <- df$os[1]
  storage      <- df$storage[1]
  
  row <- cbind(test, type, date_time, variable, value, server, metric_group, metric, POSIX, grp_metric, server_name, funct, location, cpu_cnt, mem_gb, os, storage)
  
  return(row)
  
})

disk_IO$value <- as.numeric(disk_IO$value)
disk_IO$POSIX <- perfmon_date2POSIX(disk_IO$date_time)

in_data <- rbind(in_data, disk_IO)


##########---------------------------------------------------------------------------------------
##########------------           SUMMARY STATS                                  -----------------
##########---------------------------------------------------------------------------------------



stuff <- ddply(in_data, .(server), function(df) {
  ddply(df, .(grp_metric), function(df2) {
    sao.func(df2$value)
  })
})

head(stuff)

stuff_p95 <- stuff[,c(1,2,8)]

xtable3 <- xtabs(p95 ~ server + grp_metric, data = stuff_p95)
write.csv(xtable3, file=paste(analysisDateTime, paste0(analysisDateTime,'_STATS_p95.CSV')))
write.csv(stuff, file=paste0(analysisDateTime,'_STATS.CSV'))




##########---------------------------------------------------------------------------------------
##########------------                SAVE THE DATA                             -----------------
##########---------------------------------------------------------------------------------------


save.image(file=paste(analysisDateTime, "_DATA.Rdata", sep=""))

#application <- "DC2009" 
#analysisDateTime <- paste(application, "20130505", sep="_")
#
#load(paste("archive/20130505/", analysisDateTime,  "_DATA.Rdata", sep=""))
#start_time <- min(in_data$POSIX)
#end_time   <- max(in_data$POSIX)


##########---------------------------------------------------------------------------------------
##########------------                CHARTS                                    -----------------
##########---------------------------------------------------------------------------------------


####---------------------------------
####  SETUP
####---------------------------------

#output <- "pdf"
#
#pp_ggplot2 <- ggplot2like()
#pp_ggplot2$strip.border      <- list(col="grey60")
#pp_ggplot2$axis.line         <- list(col="grey60")
#pp_ggplot2$axis.text         <- list(col="black")
#

####------
####   POWERPOINT
####------

output <- "png"

xyarea_colors <- brewer.pal(11,"Spectral")
xyarea_colors <- brewer.pal(11,"Set3")
xyarea_colors <- brewer.pal(8,"Dark2")



## GET A SAMPLE LINE FOR FORMATING THE LINE IN MY_KEY
sym.line <- Rows(trellis.par.get("superpose.line"), 1:2)

## CUSTOMIZE THE LINES IN THE KEY AND THEN UPDATE MY_KEY

sym.line$col <- xyarea_colors[1:2]
sym.line$lwd <- c(2,2)
my_key$lines <- sym.line

## SETUP POWER POINT THEME - MAKES THE CHARTS LOOK BETTER IN POWER POINT
pp_ggplot2 <- ggplot2like()
pp_ggplot2$par.ylab.text <- list(cex=1.75)
pp_ggplot2$par.xlab.text <- list(cex=1.75)
pp_ggplot2$par.zlab.text <- list(cex=1.75)
pp_ggplot2$par.sub.text  <- list(cex=1.25)
pp_ggplot2$par.main.text <- list(cex=1.5)
## SMALL LINE - 15 second data
##pp_ggplot2$superpose.line <- list(lwd=.9)
## BIG LINES - 5 minute data
pp_ggplot2$superpose.line <- list(lwd=2)
pp_ggplot2$axis.text      <- list(cex=1.25)


##-----------------
## PLOT - by variable, server
##-----------------

### PLOT FILTER
ss <- subset(in_data, !grepl("Network_In", grp_metric) & !grepl("Terminal_Services_Session", grp_metric) & !grepl("\\?", metric))

str(ss)

ss$grp_metric <- as.factor(as.character(ss$grp_metric))

str(in_data)
unique(ss$variable)
unique(ss$grp_metric)

dev_open <- FALSE
if (output == 'pdf' & dev_open == FALSE) {
  
  openGdevice(type=output, file=paste(graphDir, analysisDateTime, '_ALL', sep=""), w=14.25, h=7.15, r=108, ps=14)
  
}



topD <- d_ply(ss, .(grp_metric), function(df) {
  
  grp_mtrc <- df$grp_metric[1]
  serverName = df$server[1]
  
  if (output != 'pdf') {
    openGdevice(type=output, file=paste(graphDir, TEST_CASE, '_', grp_mtrc, sep=""), w=14.25, h=7.15, r=108, ps=14)
  }
  
  print(xyplot(value ~ POSIX | server, data=df, 
               type='l', 
               #layout=c(1,3),
               as.table=TRUE,
               #strip=FALSE,
               #strip.left = TRUE,
               par.strip.text = list(cex = .8),
               par.settings=pp_ggplot2,
               col=xyarea_colors[2:2],
               main=paste(grp_mtrc),
               sub=paste(start_time, " to ", end_time),
               xlab="Time",
               ylab="Value\n**Observe scale limits**",
               axis = axis.grid,
               scales=list(y=list(rot=0, cex=.85, alternating=3),
                           x = list(rot = 45, tick.number=24, cex=.85))
  )
  )
  
  if (output != 'pdf') {dev.off()}
  
})
dev.off()



######-------------------------------------------------------------
######                HEALTH CHECK CHARTS
######-------------------------------------------------------------

output <- "png"

xyarea_colors <- brewer.pal(11,"Spectral")
xyarea_colors <- brewer.pal(11,"Set3")
xyarea_colors <- brewer.pal(8,"Dark2")



## GET A SAMPLE LINE FOR FORMATING THE LINE IN MY_KEY
sym.line <- Rows(trellis.par.get("superpose.line"), 1:2)

## CUSTOMIZE THE LINES IN THE KEY AND THEN UPDATE MY_KEY

sym.line$col <- xyarea_colors[1:2]
sym.line$lwd <- c(2,2)
my_key$lines <- sym.line

## SETUP POWER POINT THEME - MAKES THE CHARTS LOOK BETTER IN POWER POINT
pp_ggplot2 <- ggplot2like()

pp_ggplot2 <- ggplot2like()
pp_ggplot2$strip.border      <- list(col="grey60")
pp_ggplot2$axis.line         <- list(col="grey60")
pp_ggplot2$axis.text         <- list(col="black")

health_check <- c("Processor_Total--Processor_Time",
                  "System--Processor_Queue_Length",
                  "Memory--Committed_Bytes_In_Use",					
                  "Network_Total_Bytes",
                  "Total_Disk_IO"
)	

ss <- subset(in_data, (POSIX >= start_time & POSIX <= end_time) & grp_metric %in% health_check)
str(ss)
health_check_labels <- c("CPU%",
                         "CPU_Queue",
                         "Memory",					
                         "Network_Bytes_sec",
                         "Total_Disk_IO"
                         
                         
)	

ss$grp_metric <- factor(ss$grp_metric, levels=health_check, labels=health_check_labels)

##-----------------
## PLOT - by variable, server
##-----------------
topD <- d_ply(ss, .(server), function(df) {
  
  serv <- df$server[1]
  servName <- df$server_name[1]
  loc <- df$location[1]
  funct <- df$funct[1]
  cpu_cnt <- df$cpu_cnt[1]
  mem_gb <- df$mem_gb[1]
  os <- df$os[1]
  openGdevice(type=output, file=paste(graphDir, TEST_CASE, '__', servName, sep=""), w=10, h=7.5, r=108, ps=14)
  
  print(xyplot(value ~ POSIX | grp_metric, data=df, 
               type='l', 
               layout=c(1,5),
               as.table=TRUE,
               #strip=FALSE,
               #strip.left = TRUE,
               strip=FALSE,
               strip.left = TRUE,
               par.strip.text = list(cex = .7),
               par.settings=pp_ggplot2,
               col=xyarea_colors[2:2],						
               main=paste(loc, "-", servName, "\n", funct, "\n", "CPU:", cpu_cnt, "MEM:", mem_gb, "OS:", os),
               sub=paste(start_time, " to ", end_time),
               xlab="Time",
               ylab="Value\n**Observe scale limits**",
               axis = axis.grid,
               scales=list(y=list(relation="free", rot=0, cex=.8, alternating=3),
                           x = list(rot = 45, tick.number=24, cex=.8))
  )
  )
  
  dev.off()
  
})




#stopCluster(cl)

## ss <- subset(in_data, server == "10_254_70_138" & metric == "Avg_Disk_sec_Write"  )
## ss <- subset(in_data, metric == "Processor_Queue_Length"  )
## ss <- subset(in_data, metric == "Buffer_cache_hit_ratio"  )
## unique(ss$server)
## describe(ss)
## describe(in_data)
## plot(ss$POSIX, ss$value, type='l')

# in_data <- fix_table

#### HEALTH CHECK VARIABLES FROM  CCTR
#health_check <- c("System--Processor_Queue_Length",
#		"Processor_Total--Processor_Time",
#		"PhysicalDisk_Total--Disk_Time",
#		"PhysicalDisk_Total--Avg_Disk_Queue_Length",
#		"Network_Interface__2--Bytes_Total_sec",
#		"Network_Interface--Bytes_Total_sec",
#		"Network_Interface__2--Output_Queue_Length",
#		"Network_Interface--Output_Queue_Length",
#		"Memory--Pages_sec",
#		"Memory--Available_MBytes",
#		"SQLServer_Buffer_Manager--Buffer_cache_hit_ratio")
