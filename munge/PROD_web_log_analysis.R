###---------------------------------------------------------
###   web_log_analysis.R:  Parsing the USL logs
###   Author: Stephen O'Connell
###   Date: 08/5/2014
###   Description:
###   Change History: (why is always just because...)
###        Who         When            What
###        SAOB        08/05/2014      Initial pass
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
library(dplyr)


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
  byTran <- tapply(df$time.taken, df$stem, sao.func)
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

###------------------------------------------------------
###   SET PLOT PROCESSING PARAMETERS
###------------------------------------------------------
###  CREATE A NAME FOR THE ANALYSIS, COULD BE DATA TIME, OR A LABEL
###  RUN TIME TAKEN FROM THE LOAD RUNNER REPORT
##  ANALYSIS NAME



##-------------
## READ DATA
##-------------
dev.off()

rootDir <- paste("C:/Users/SAOb/Documents/performance_data/20140624_SAMI/PERF_TESTS/", sep="")

TEST_TYPE <- "ROUND_2_QA"
## READ THE TEST DETAILS
test_details <- read.csv(paste0(rootDir, "test_details.csv"), header=TRUE, stringsAsFactor=FALSE)
test_details <- subset(test_details, type == TEST_TYPE)

#analysisDateTime <- paste(analysisName, format(Sys.time(), "%Y%m%d"), sep="_") 

dataDir <- (paste(rootDir, TEST_TYPE, "/PROD_weblogs/", sep=''))

graphDir <- (paste(dataDir,  "graphics", '/', sep=""))

dir.create(graphDir, showWarnings = TRUE, recursive = FALSE)


setwd(dataDir)
getwd()

if (exists("test_files")) {rm(test_files)}
if (exists("all_IIS_data"))    {rm(all_IIS_data)}

test_files <- list.files(recursive = TRUE)


for (f in 1:length(test_files)) {
  
  if (grepl('log$', test_files[f])) {
    cat("TEST FILE = ", test_files[f], "\n")
    
    temp <- read.delim(test_files[f], header=FALSE, skip=4, stringsAsFactors=FALSE, sep=' ', strip.white=TRUE)
    
    temp <- cbind(source="PROD", server=strsplit(test_files[f], "\\/")[[1]][1], temp)
    
    if (!exists("data.list")) {
      data.list <- list()  
      data.list[[test_files[f]]] <- temp
    } else {
      data.list[[test_files[f]]] <- temp
    }
  }
}
rm(all_IIS_data)
all_IIS_data <- tbl_df(rbindlist(data.list))
rm(data.list)


#date time s-ip cs-method cs-uri-stem cs-uri-query s-port cs-username c-ip cs(User-Agent) sc-status sc-substatus sc-win32-status time-taken
header <- c("source","server","date","time","s.ip","cs.method","cs.uri.stem","cs.uri.query","s.port","cs.username","c.ip","cs.user.agent","sc.status","sc.substatus","sc.win32.status","time.taken")
#date time s-ip cs-method cs-uri-stem cs-uri-query s-port cs-username c-ip cs(User-Agent) sc-status sc-substatus sc-win32-status sc-bytes cs-bytes time-taken
#header <- c("date","time","s.ip","cs.method","cs.uri.stem","cs.uri.query","s.port","cs.username","c.ip","cs.user.agent","sc.status","sc.substatus","sc.win32.status","sc.bytes","cs.bytes","time.taken")
names(all_IIS_data) <- header


##########---------------------------------------------------------------------------------------
##########------------           CLEAN UP THE DATA                             -----------------
##########---------------------------------------------------------------------------------------
#


## CONVERT THE TIMESTAMP TO A POSIX TIME (epoch), USEFUL FOR DOING MATH ON DATES
all_IIS_data$POSIX <- as.POSIXct(strptime(paste(all_IIS_data$date, all_IIS_data$time), format="%Y-%m-%d %H:%M:%S")) - (7 * 3600)

max(all_IIS_data$POSIX)

all_IIS_data$uname <- as.factor(all_IIS_data$cs.username)

all_IIS_data$stem  <- as.factor(all_IIS_data$cs.uri.stem)

all_IIS_data$sc.status <- as.numeric(all_IIS_data$sc.status)


##########---------------------------------------------------------------------------------------
##########------------           FILTER UP THE DATA                             -----------------
##########---------------------------------------------------------------------------------------

start_time = min(all_IIS_data$POSIX)
end_time   = max(all_IIS_data$POSIX)


### REMOVE THE "DO NOTHING URLS"
hold_data <- all_IIS_data
#all_IIS_data <- hold_data

## CURRENT FILTER DOES NOT REMOVE ANYTHING
all_IIS_data$remove_index <- (all_IIS_data$cs.method == 'GET' & all_IIS_data$cs.uri.stem == '/' & all_IIS_data$cs.user.agent == 'a10hm/1.0')
all_IIS_data <- subset(all_IIS_data, remove_index == FALSE)


##########---------------------------------------------------------------------------------------
##########------------           CODE USED TO ANNOTATE THE METRICS              -----------------
##########---------------------------------------------------------------------------------------

## CREATE CUTS OF THE DATE BY 1,5,15 and 60 MINUTES
all_IIS_data$Breaks_1min <- cut(all_IIS_data$POSIX, seq(trunc(start_time, units='hours'), trunc(end_time+59, units='mins'), by='1 min'))
all_IIS_data$Breaks_5min <- cut(all_IIS_data$POSIX, seq(trunc(start_time, units='hours'), trunc(end_time+299, units='mins'), by='5 min'))
all_IIS_data$Breaks_15min <- cut(all_IIS_data$POSIX, seq(trunc(start_time, units='hours'), trunc(end_time+899, units='mins'), by='15 min'))
all_IIS_data$Breaks_1hour <- cut(all_IIS_data$POSIX, seq(trunc(start_time, units='hours'), trunc(end_time+3599, units='hours'), by='60 min'))

by_break <- 1
myLabels <- seq(trunc(min(all_IIS_data$POSIX), units='min'), trunc(max(all_IIS_data$POSIX)+900, units='min'), by=paste0(by_break, ' min'))
head(myLabels)
all_IIS_data$one <- cut(all_IIS_data$POSIX, breaks=myLabels, include.lowest=TRUE, right=TRUE)


##########---------------------------------------------------------------------------------------
##########------------           CODE TO SUMMARIZE TYPES OF DATA                -----------------
##########---------------------------------------------------------------------------------------

###------------------------------
## REPORT:
###------------------------------


###----------
###  FIND THE TOP 20 URLS IN THE COMPLETE DATASET
###----------

setwd(paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/report_data/PROD_weblogs_reports/", sep="_"))

####  TRIM THE DATA TO 90%, REMOVE THE OUTLIERS

###  GET THE TOP URLS
top.url <- sort.data.frame(as.data.frame(table(all_IIS_data$stem)),~-Freq)

## DEBUG
# head(top.url)
# head(subset(top.url, grepl("png", Var1) & time.taken > 1000))
# nrow(subset(all_IIS_data, grepl("png", stem) & time.taken > 250))

## REMOVE THE png FILES
top.url <- subset(top.url, !grepl("png", Var1))

top.url[top.url$Freq > 5000,]

plot(1:nrow(top.url), top.url$Freq, type='l')

## SELECT ONLY THOSE GREATER THAN 10000
###  ******  THIS WILL NEED TO CHANGE IF PROCESSING ANY OTHER LOGS
###  ******  THIS WILL NEED TO CHANGE IF PROCESSING ANY OTHER LOGS
###  ******  THIS WILL NEED TO CHANGE IF PROCESSING ANY OTHER LOGS
listTranNames <- (subset(top.url, Freq > 5000))


## CREATE A VECTOR OF TRANSACTION NAMES FOR THE TRIM AND SUM
listTranNames <- as.character(listTranNames$Var1)


##****************
##****************
#### VERY EXPENSIVE PROCESS
##****************
##****************
rm(data.list)
for (n in listTranNames) {
  ss <- subset(all_IIS_data, stem == n)
  remove_indx <- trimOutliers(ss$time.taken)
  
  if (exists("data.list")) {
    c <- c + 1
    data.list[[c]] <- ss[-remove_indx,]
  } else {
    c <- 1
    data.list <- list()
    data.list[[c]] <- ss[-remove_indx,]
  }
  
}

trimmed_data <- as.data.frame(rbindlist(data.list))

####----------------------------------------------------------
####  SAVE THE DATA FILES
####----------------------------------------------------------
save(all_IIS_data, trimmed_data, file=(paste0(dataDir, "all_IIS_data.RData")))
#load(file="all_IIS_weblogs.RData")

ss_all_IIS_data <- subset(all_IIS_data, stem %in% listTranNames)

str(ss_all_IIS_data)
summ_data <- summarizeData(ss_all_IIS_data, "FULL_DATA")
summ_data <- rbind(summ_data, summarizeData(trimmed_data, "TRIM_DATA"))

current_dir <- getwd()

## REPORT THE SUMMARY OF FULL VS. TRIMMED DATA
options(width=180)
sink(file=paste(dataDir, "_REPORT_DENSITY", ".txt", sep=""))
summ_data[order(summ_data$TransNames),]
sink()
options(width=80)

ss <- subset(summ_data, label == 'TRIM_DATA')
write.csv(ss, file=paste0('_TRIM_STATS.CSV'))

ss <- subset(summ_data, label == 'FULL_DATA')
write.csv(ss, file=paste0('_FULL_STATS.CSV'))

ss <- subset(summ_data)
write.csv(ss, file=paste0('_ALL_STATS.CSV'))

setwd(current_dir)


###  TOP 30 OVERALL URLS BY, FROM THE all_IIS_data 
ss <- subset(summ_data, label == 'TRIM_DATA')

## THE TOP 30 BY p90, DESCENDING 
ss <- sort.data.frame(ss,~-p90)
top30.p90.urls <- as.character(ss$TransNames[1:30])

## THE TOP 30 BY p90, DESCENDING 
ss <- sort.data.frame(ss,~-Count)
top30.Count.urls <- as.character(ss$TransNames[1:30])


## DEBUG

output <- 'png'
for (top in 1:length(top30.p90.urls)) {
  openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/PROD_weblog_graphics/",  "top30_urls_DENSITY",  top, gsub("/", "_", top30.p90.urls[top]), sep="_"), w=10, h=10, r=108, ps=14)
  x <- subset(trimmed_data, stem == top30.p90.urls[top] & time.taken < 3000, select=time.taken)
  x <- x$time.taken
  plot(density(x), main=top30.p90.urls[top])
  abline(v=mean(x), col='green')
  abline(v=quantile(x, prob=.5), col='blue')
  abline(v=quantile(x, prob=.9), col='red')
  legend("topright", c("mean", "median", "p90"),
         title="Metrics",
         lty=c(1,1,1),
         lwd=c(1,1,1),
         col=c('green', 'blue','red'),
         cex=.6
  )
  dev.off()
}




##########---------------------------------------------------------------------------------------
##########------------           SUMMARY STATS                                  -----------------
##########---------------------------------------------------------------------------------------

setwd(paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/report_data/PROD_weblogs_reports/", sep="_"))
## UNIQUE USERS
options(width=200)
sink(file=paste("_UNIQUE_USERS", ".txt", sep=""))
cat("\n\nUNIQUE USERS:\n")
unique(all_IIS_data$cs.username)

cat("\n\nNUMBER OF UNIQUE USERS:\n")
length(unique(all_IIS_data$cs.username))
cat("\n\nUNIQUE USERS, OCCURANCES:\n")
sort.data.frame(as.data.frame(table(all_IIS_data$uname)),~-Freq)
sink()
options(width=80)

### UNIQUE URLS
options(width=200)
sink(file=paste("_UNIQUE_URLS", ".txt", sep=""))
cat("\n\nUNIQUE URLs:\n")

sort.data.frame(as.data.frame(table(all_IIS_data$stem)),~-Freq)
sink()
options(width=80)

sum(as.data.frame(table(all_IIS_data$stem))[2])

### UNIQUE URLS WITH NON-BLANK NAME
options(width=200)
sink(file=paste("_UNIQUE_URLS_WITH_NON_BLANK_USERNAME", ".txt", sep=""))
cat("\n\nNUMBER OF UNIQUE URLS WITH NON BLANK USERNAME:\n")
ss <- subset(all_IIS_data, cs.username != '-')
ss$stem <- as.factor(as.character(ss$stem))
as.data.frame(table(ss$stem))
sink()
options(width=80)


### UNIQUE URLS WITH NON-BLANK NAME
options(width=200)
sink(file=paste("_UNIQUE_IP_ADDRESSES", ".txt", sep=""))
cat("\n\nNUMBER OF UNIQUE IP ADDRESS:\n")
as.data.frame(table(as.factor(all_IIS_data$c.ip)))
sink()
options(width=80)



### UNIQUE HTTP RETURN CODES
options(width=200)
sink(file=paste("_HTTP_RETURN_CODES", ".txt", sep=""))
cat("\n\nHTTP_RETURN_CODES:\n")
table(all_IIS_data$sc.status)
sink()
options(width=80)


### NUMBER_HITS_15mins
options(width=160)
sink(file=paste("_NUMBER_HITS_PER_15MINS", ".txt", sep=""))
cat("\n\nNUMBER OF HITS PER 15MINS:\n")
as.data.frame(table(all_IIS_data$Breaks_15min))
sink()
options(width=80)


### NUMBER_HITS_5mins
options(width=160)
sink(file=paste("_NUMBER_HITS_PER_5MINS", ".txt", sep=""))
cat("\n\nNUMBER OF HITS PER 5MINS:\n")
as.data.frame(table(all_IIS_data$Breaks_5min))
sink()
options(width=80)

### NUMBER_HITS_1mins
options(width=160)
sink(file=paste("_NUMBER_HITS_PER_1MINS", ".txt", sep=""))
cat("\n\nNUMBER OF HITS PER 5MINS:\n")
as.data.frame(table(all_IIS_data$Breaks_1min))
sink()
options(width=80)


### LIST OF URLSWITH 500 ERRORS
options(width=200)
sink(file=paste("_LIST_OF_URLS_WITH_500_ERRORS", ".txt", sep=""))
cat("\n\nLIST OF URLS WITH 500 ERRORS:\n")
print(subset(all_IIS_data, sc.status == 500))
sink()
options(width=80)


##########---------------------------------------------------------------------------------------
##########------------           SAVE THE DATA                                  -----------------
##########---------------------------------------------------------------------------------------
getwd()
setwd(dataDir)
save(all_IIS_data, trimmed_data, file="ROUND_2_QA_all_IIS_weblogs.RData")
#load(file="ROUND_2_QA_all_IIS_weblogs.RData")


##########---------------------------------------------------------------------------------------
##########------------                CHARTS                                    -----------------
##########---------------------------------------------------------------------------------------

###------------------------------------------------------------------------------
###   CHARTS - PROCESS ALL THE DATA FOR EACH TEST
###------------------------------------------------------------------------------
pp_ggplot2 <- ggplot2like()
pp_ggplot2$strip.border      <- list(col="grey60")
pp_ggplot2$axis.line         <- list(col="grey60")
pp_ggplot2$axis.text         <- list(col="black")

output <- "png"

###------
### SUBSET THE DATA AROUND THE TEST PERIODS
###------
str(trimmed_data)

by_break <- 1
myLabels <- seq(trunc(min(trimmed_data$POSIX), units='min'), trunc(max(trimmed_data$POSIX)+900, units='min'), by=paste0(by_break, ' min'))
trimmed_data$one <- cut(trimmed_data$POSIX, breaks=myLabels, include.lowest=TRUE, right=TRUE)

trimmed_data$stem <- as.factor(as.character(trimmed_data$stem))

ss30_p90_all_IIS_data <- subset(trimmed_data, stem %in% top30.p90.urls)
ss30_Count_all_IIS_data <- subset(trimmed_data, stem %in% top30.Count.urls)

#top30.p90.urls[sort(top30.p90.urls) %in% sort(top30.Count.urls)]
#top30.Count.urls[sort(top30.p90.urls) %in% sort(top30.Count.urls)]
length(levels(as.factor(as.character(trimmed_data$stem))))

all_data <- ss30_p90_all_IIS_data

SS.list <- list()

all_data$DATE <- format(all_data$POSIX, "%Y-%m-%d")


###----------------------------------------------------------
###  NOT NEEDED FOR THE PROD LOGS, USING DATE
###----------------------------------------------------------
# for (t in 1:length(prod_dates)) {
#   
#   
#   ## SUBSET THE DATA BY RUN DATE TIME
#   runTime <-  test_details$time[t]
#   
#   temp_ss <- all_data
#   
#   if (nrow(temp_ss) > 0) {
#     ### CREATE START AND END TIMES FOR EACH TRANSACTION
# #     start_time <- as.POSIXct(strptime(substring(runTime,1,19), format="%d/%m/%Y %H:%M:%S"))
# #     
# #     temp_ss$trans_end_time <- start_time - temp_ss$time.taken
# #     temp_ss$trans_start_time <- temp_ss$trans_end_time + temp_ss$time.taken
#     
#     
#     ###   CREATE A START AND END FOR SUBSETTING THE DATA AROUND A SPECIFIC TEST, SUBSTRACT 20 MINUTES
#     ###     THE START OF THE TEST AND ADD 2O MINUTES TO THE END OF THE TEST
#     startTime <- as.POSIXct(strptime(substring(runTime,1,19), format="%d/%m/%Y %H:%M:%S")) + (test_details$warmup[t] * 60)
#     endTime <- startTime + (test_details$duration[t] * 60)
#     
#     ## DEBUG STUFF
#     cat("start_time = ", as.POSIXct(startTime, origin = "1970-01-01"), " end_time = ", as.POSIXct(endTime, origin = "1970-01-01"), " test time = ", test_details$time[t], "\n")
#     print(as.POSIXct(startTime, origin = "1970-01-01"))
#     print(as.POSIXct(endTime, origin = "1970-01-01"))
#     
#     #str(temp_ss)
#     
#     ## SUBSET OUT JUST THE TRANSACTIONS THAT OCCURED BETWEEN THESE TIMES.
#     SS.list[[test_details$test[t]]] <- subset(temp_ss, POSIX >= startTime & POSIX <= endTime)
#         
#     ## ADD THE TEST AND TYPE TO THE SUBSET AND SAVE IN THE DATA.LIST
#     SS.list[[test_details$test[t]]] <- cbind(test=test_details$test[t], 
#                                              user_count=strsplit(test_details$test[t], "_")[[1]][2], 
#                                              SS.list[[test_details$test[t]]])
#   }
#   
# } 
# 
# ## COMBINE ALL THE SUB DATA FRAMES
# all_ss <- as.data.frame(rbindlist(SS.list))

all_ss <- all_data
str(all_ss)
nrow(all_IIS_data)
nrow(trimmed_data)
nrow(all_data)

##---------------------------------------------------------------
## CONSOLIDATE TEST, BY USER COUNT
##   BOX PLOTS OF METRICS BY ESX HOST
##---------------------------------------------------------------
## ALL servers

#ss <- subset(all_ss, Entity %in% esx.hosts)
all_ss$stem <- as.factor(as.character(all_ss$stem))
topD <- d_ply(all_ss, .(stem), function(df) {
  
  mtrc <- df$stem[1]
  
  df <- sort.data.frame(df,~+POSIX)
  openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/PROD_weblog_graphics/",  "_respone_time_by_date",  gsub("/", "_", mtrc), sep="_"), w=10, h=10, r=108, ps=14)
  
  print(bwplot(time.taken ~ DATE, data=df, 
               type='l', 
               #layout=c(1,1),
               as.table=TRUE,
               #strip=FALSE,
               #strip.left = TRUE,
               par.strip.text = list(cex = .95),
               par.settings=pp_ggplot2,
               
               main=paste("SAMI/TAMI Load Test\n", gsub("/", "_", mtrc), "\nBy DATE"),
               xlab="DATE",
               ylab="Time Taken/ms",
               axis = axis.grid,
               scales=list(y=list(relation="same",  rot=0, cex=.7, alternating=3),
                           x = list(rot = 45, tick.number=24, cex=.7))
  )
  )
  
  dev.off()
  
})

# 
# ##---------------------------------------------------------------
# ## CONSOLIDATE TEST, BY USER COUNT
# ##   BOX PLOTS OF METRICS BY ESX HOST
# ##---------------------------------------------------------------
# ## ALL servers
# 
# #ss <- subset(all_ss, Entity %in% esx.hosts)
# all_ss$stem <- as.factor(as.character(all_ss$stem))
# topD <- d_ply(all_ss, .(stem), function(df) {
#   
#   mtrc <- df$stem[1]
#   
#   df <- sort.data.frame(df,~+POSIX)
#   openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/",  "_iis_by_test",  gsub("/", "_", mtrc), sep="_"), w=10, h=10, r=108, ps=14)
#   
#   print(bwplot(time.taken ~ test, data=df, 
#                type='l', 
#                #layout=c(1,1),
#                as.table=TRUE,
#                #strip=FALSE,
#                #strip.left = TRUE,
#                par.strip.text = list(cex = .95),
#                par.settings=pp_ggplot2,
#                
#                main=paste("SAMI/TAMI Load Test\n", gsub("/", "_", mtrc), "\nBy Test"),
#                xlab="Number of Concurrent Users",
#                ylab="Value\n**Observe scale limits**",
#                axis = axis.grid,
#                scales=list(y=list(relation="same",  rot=0, cex=.7, alternating=3),
#                            x = list(rot = 45, tick.number=24, cex=.7))
#   )
#   )
#   
#   dev.off()
#   
# })
# 


##### ----------------------------------------------------
#####   TRANSACTION PER MIN
##### ----------------------------------------------------

##---------------------------------------------------------------
## CONSOLIDATE TEST, BY USER COUNT
##   BOX PLOTS OF METRICS BY ESX HOST
##---------------------------------------------------------------
## ALL servers

all_IIS_data$DATE <- format(all_IIS_data$POSIX, "%Y-%m-%d")

unique(all_IIS_data$cs.username)

unique_users <- ddply(all_IIS_data, .(DATE), function(df) {
              cat("date = ", df$DATE[1], "\n")
              unique_usernames <- unique(df$cs.username)
              cnt <- length(unique_usernames)
              cat("cs.username = ", unique_usernames, " cnt = ", cnt, "\n")
              return(cnt)
              })

all_ss$stem <- as.factor(as.character(all_ss$stem))
topD <- d_ply(all_ss, .(DATE), function(df) {
  
  mtrc <- as.character(df$DATE[1])
  
  df <- sort.data.frame(df,~+POSIX)
  
  openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/PROD_weblog_graphics/",  "_DATE",  by_break, mtrc, sep="_"), w=10, h=10, r=108, ps=14)
  
  myLabels <- seq(trunc(min(df$POSIX), units='min'), trunc(max(df$POSIX)+900, units='min'), by=paste0(by_break, ' min'))
  
  df$one <- cut(df$POSIX, breaks=myLabels, include.lowest=TRUE, right=TRUE)
  
  x <- myLabels[1:length(as.numeric(table(df$one)))]
  y <- as.numeric(table(df$one))
  
  y_mean <- mean(y[y > quantile(y, prob=.05)])
  
  plot(x, y, type='s', ylim=c(0,300),
       main=paste("Web Server Hits, per minute\n", "SAMI/TAMI Test:", mtrc, "\n", min(all_ss$POSIX), "to", max(all_ss$POSIX)),
       ylab=paste0("Number of Transactions/", by_break, " mins"),
       xlab="Time")
  
  abline(h=quantile(y, prob=.9), col='blue', lwd=2)
  #abline(h=mean(y), col='red', lwd=2)
  abline(h=y_mean, col='red', lwd=2, lty=1)
  
  
  num_users <- unique_users$V1[unique_users$DATE == mtrc]
  tx_user   <- y_mean / num_users
  
  cat(mtrc, " mean ", y_mean, " number of users ", num_users, " tx/user ", tx_user, "\n")

  legend("topright", c("count", paste0("mean: ", round(y_mean,0)), paste0("p90: ", round(quantile(y, prob=.9), 0))),
         title="Legend",
         lty=c(1,1,1),
         lwd=c(2,2,2),
         col=c('black', 'blue','red'),
         cex=1
  )
  
  
  dev.off()
  
})




openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/hits_per_minute_ALL_TESTS", sep=""), w=10, h=10, r=108, ps=14)
by_break <- 1

ss <- subset(all_IIS_data, POSIX > min(all_ss$POSIX) & POSIX < max(all_ss$POSIX))

myLabels <- seq(trunc(min(ss$POSIX), units='min'), trunc(max(ss$POSIX)+900, units='min'), by=paste0(by_break, ' min'))

ss$one <- cut(ss$POSIX, breaks=myLabels, include.lowest=TRUE, right=TRUE)

plot(myLabels[1:length(as.numeric(table(ss$one)))], as.numeric(table(ss$one)), type='s', 
     main=paste("Web Server Hits, per minute\n", "All SAMI/TAMI Tests\n", min(all_ss$POSIX), "to", max(all_ss$POSIX)),
     ylab=paste0("Number of Transactions/", by_break, " mins"),
     xlab="Time")
dev.off()

#openGdevice(type=output, file=paste("C:/Users/SAOb/Documents/workspace/RStudio_Projects/SAMI_TAMI/graphics/hits_per_minute_ALL_TESTS", sep=""), w=10, h=10, r=108, ps=14)
by_break <- 1
myLabels <- seq(trunc(min(all_ss$POSIX), units='min'), trunc(max(all_ss$POSIX)+900, units='min'), by=paste0(by_break, ' min'))

all_ss$one <- cut(all_ss$POSIX, breaks=myLabels, include.lowest=TRUE, right=TRUE)
plot(myLabels[1:length(as.numeric(table(all_ss$one)))], as.numeric(table(all_ss$one)), type='s', 
     main=paste("Web Server Hit\n", "All Tests\n", min(all_ss$POSIX), "to", max(all_ss$POSIX)),
     ylab=paste0("Number of Transactions/", by_break, " mins"),
     xlab="Time")

#dev.off()


###-------------------------------------------------------------
###  Inter-Arrival time analysis
###-------------------------------------------------------------

temp_IIS <- all_IIS_data

temp_IIS$arrival.time <- as.POSIXct(strptime(
                                        paste0(temp_IIS$date, " ", temp_IIS$time, ".000"), 
                                        format="%Y-%m-%d %H:%M:%OS"))
hold_IIS <- temp_IIS
#all_IIS_data <- hold_IIS
#all_IIS_data$prior.time.taken <- c(all_IIS_data$time.taken[1],all_IIS_data$time.taken[1:nrow(all_IIS_data)-1]) / 1000


temp_IIS$arrival.time <- temp_IIS$arrival.time - (temp_IIS$time.taken / 1000)
time_index <- order(temp_IIS$arrival.time)

temp_IIS <- all_IIS_data[time_index,]
temp_IIS$prior.arrival.time <- c(temp_IIS$arrival.time[1],temp_IIS$arrival.time[1:nrow(temp_IIS)-1])
temp_IIS$interarrival.time <- as.double(temp_IIS$arrival.time - temp_IIS$prior.arrival.time)


SS.list <- list()

for (t in 1:length(test_details$test)) {
  
  
  ## SUBSET THE DATA BY RUN DATE TIME
  runTime <-  test_details$time[t]
    
  
  ###   CREATE A START AND END FOR SUBSETTING THE DATA AROUND A SPECIFIC TEST, SUBSTRACT 20 MINUTES
  ###     THE START OF THE TEST AND ADD 2O MINUTES TO THE END OF THE TEST
  startTime <- as.POSIXct(strptime(substring(runTime,1,19), format="%d/%m/%Y %H:%M:%S")) + (test_details$warmup[t] * 60)
  endTime <- startTime + (test_details$duration[t] * 60)
  
  ## DEBUG STUFF
  cat("start_time = ", as.POSIXct(startTime, origin = "1970-01-01"), " end_time = ", as.POSIXct(endTime, origin = "1970-01-01"), " test time = ", test_details$time[t], "\n")
  print(as.POSIXct(startTime, origin = "1970-01-01"))
  print(as.POSIXct(endTime, origin = "1970-01-01"))
  
  #str(temp_ss)
  
  ## SUBSET OUT JUST THE TRANSACTIONS THAT OCCURED BETWEEN THESE TIMES.
  SS.list[[test_details$test[t]]] <- subset(temp_IIS, POSIX >= startTime & POSIX <= endTime)
  
  ## ADD THE TEST AND TYPE TO THE SUBSET AND SAVE IN THE DATA.LIST
  SS.list[[test_details$test[t]]] <- cbind(test=test_details$test[t], 
                                           user_count=strsplit(test_details$test[t], "_")[[1]][2], 
                                           SS.list[[test_details$test[t]]])
  
  
  
  
} 

for (t in test_details$test) {
  
  interarrival <- SS.list[[t]]$interarrival.time[SS.list[[t]]$interarrival.time > 0]
  mu <- mean(interarrival)
  sd <- sd(interarrival)
  v  <- var(interarrival) 
  
  plot(density(SS.list[[t]]$interarrival.time))
  
  cat("test = ", t, " mu = ", mu, " sd = ", sd, " delta = ", mu/sd, " var = ", v, " other sd = ", sqrt(v), "\n")
  
}
.01 / .01
.01 / .06
1/6
1/60



####---------------------------------------------------------------------
####  FAILED ATTEMPT AT LATTICE VBY DATE
####---------------------------------------------------------------------

myLabels <- seq(trunc(min(all_IIS_data$POSIX), units='min'), trunc(max(all_IIS_data$POSIX)+900, units='min'), by=paste0(by_break, ' min'))

all_IIS_data$one <- cut(all_IIS_data$POSIX, breaks=myLabels, include.lowest=TRUE, right=TRUE)

x <- myLabels[1:length(as.numeric(table(all_IIS_data$one)))]
y <- as.numeric(table(all_IIS_data$one))
head(x)

df <- as.data.frame(cbind(DATE=format(x, "%Y-%m-%d"), y=y, x=x))
df$x <- format(as.POSIXct(as.numeric(as.character(df$x)),  origin = "1970-01-01"), "%H%M")
df$y <- as.numeric(df$y)

str(df)
xyplot(y ~ x | DATE, data=df, 
             type='l', 
             #layout=c(1,1),
             as.table=TRUE,
             #strip=FALSE,
             #strip.left = TRUE,
             par.strip.text = list(cex = .95),
             par.settings=pp_ggplot2,
             
             main=paste("SAMI/TAMI Production Data\n","By DATE"),
             xlab="DATE",
             ylab="Transaction/minute",
             axis = axis.grid,
             scales=list(y=list(relation="same",  rot=0, cex=.7, alternating=3),
                         x = list(rot = 45, tick.number=24, cex=.7))
)

