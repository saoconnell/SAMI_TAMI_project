###---------------------------------------------------------
###   TEMPLATE_1:  ANALYSIS AND PLOTTING
###   Author: Stephen O'Connell
###   Date: MM/DD/YYYY
###   Description:
###   Change History: (why is always just because...)
###        Who         When            What
###
###---------------------------------------------------------

##-----------------------------------------------------------------------------------------

## CLEANUP
rm(list=ls())
memory.limit(size=3072)

##-------------
## LOAD required functions
##-------------
require(Hmisc)
require(lattice)
require(latticeExtra)
require(Cairo)
require(reshape)
require(stringr)
require(ggplot2)

##-------------
## LOAD SAOB SetUp functions
##-------------
setUpFunc_Dir <- c("c:\\data\\workspace\\R\\setUp\\")
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

##-------------
## READ DATA
##-------------
dev.off()

TEST_NAME <- c("BASELINE",
               "TEST_3_2MM",
               "TEST_3_7MM",
               "TEST_4_2MM",
               "TEST_4_7MM",
               "TEST_5_2MM",
               "DEL_NONPFL_AFTER",
               "DEL_NONPFL_BEFORE",
               "NEW_JOBDOC")

TEST_CASE <- c("TEST2")


for (tn in TEST_NAME) {
  for (tc in TEST_CASE) {
    cat("TEST_NAME = ", tn, " TEST_CASE = ", tc, "\n")
  }
}

#TEST_NAME <- "ROUND1_500K"
#TEST_CASE <- "TEST1"

logDir <- (paste("C:/Data/performance_data/20120312_ECM_Documentum/performance_tests/20120802_SCALABILITY/reports/", sep=""))
graphDir <- (paste("C:/Data/performance_data/20120312_ECM_Documentum/performance_tests/20120802_SCALABILITY/graphics/", sep=""))


setwd(logDir)

dir.create(graphDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")  
getwd()

getsizes()

### SEE SECTION OF CODE AT "THIS IS THE CHARTS FOR ECM TESTING"
###     FOR SUMMARIZATION CHARTS OF ECM TRANS DATA
load("ALL_LRdata.Rdata")
load("ALL_PERFMON_data.Rdata")
load("subset_LRraw_data.Rdata")

ls()

str(ss_summ_data)

###--------------------------------------------------------
###   DATA PREP
###--------------------------------------------------------


##-------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------
##-------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------
###   Reports
##-------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------
##-------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------

str(all_summ_data)
unique(subset(all_summ_data, label == 'FULL_DATA')["TransNames"])
levels(all_summ_data$TransNames)

all_summ_data$brkPnts <- cut(all_summ_data$Median, breaks=c(0,2,5,10, 1000), labels=c("green","amber","red", "Red"))
all_summ_data$brkPnts <- as.character(all_summ_data$brkPnts)
all_summ_data$brkPnts <- ifelse(all_summ_data$brkPnts == 'Red', 'red', all_summ_data$brkPnts)
all_summ_data$one <- 1
all_summ_data$brkPnts <- factor(all_summ_data$brkPnts, labels=c("green","amber","red"))
str(all_summ_data)

head(all_summ_data)
table(all_summ_data$brkPnts)

subset(all_summ_data, brkPnts == 'Red', select=c(1,7,14))

xtabs(Count ~ brkPnts + TEST_NAME + TEST_CASE, all_summ_data)






##-------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------
##-------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------
## 				P  L  O  T  S
##-------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------
##-------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------



##----------------------------------------------------------
## PLOT - Transactions
##----------------------------------------------------------

pp_ggplot2 <- ggplot2like()
pp_ggplot2$strip.border      <- list(col="grey60")
pp_ggplot2$axis.line         <- list(col="grey60")
pp_ggplot2$axis.text         <- list(col="black")

byVar <- tapply(all_LR_data$trans_resp_time$value, ss$variable, my.func)  
byVar <- do.call('rbind', byVar)
byVar


load("ALL_LRdata.Rdata")

all_summ_data <- subset(all_summ_data, label == 'FULL_DATA' 
                        & TransNames != "IJ_01_Login"
                        & TransNames != "IJ_10_Logout")
output <- 'png'

topD <- d_ply(all_summ_data, .(TransNames), function(df) {
  
  TN <- df$TransName[1]
  openGdevice(type=output, file=paste(graphDir, "TRANS", "_", TN, sep=""), w=10, h=5, r=108, ps=14)
  
  plot <- segplot(reorder(factor(TEST_NAME), Median) ~ Min + p90, 
                  data = subset(all_summ_data, label == "TRIM_DATA" & TransNames == TN & TEST_CASE == 'TEST2'), 
                  draw.bands = FALSE, centers = Median,
                  segments.fun = panel.arrows, ends = "both", 
                  angle = 90, length = 1, unit = "mm", cex=1, pch=15, pch.cex=2,
                  xlab='Response Time/sec',
                  main=paste(TN, "\nMin -- Median -- p95", sep=" "))
  plot$y.scales$cex <- c(1.1,1.1)
  plot$x.scales$cex <- c(1.2,1.2)
  
  print(plot)
  dev.off()
})
dev.off()


str(plot)

##----------------------------------------------------------
## PLOT - SUBSET Transactions
##----------------------------------------------------------

pp_ggplot2 <- ggplot2like()
pp_ggplot2$strip.border      <- list(col="grey60")
pp_ggplot2$axis.line         <- list(col="grey60")
pp_ggplot2$axis.text         <- list(col="black")

byVar <- tapply(all_LR_data$trans_resp_time$value, ss$variable, my.func)  
byVar <- do.call('rbind', byVar)
byVar

#plot <- segplot(reorder(factor(TEST_NAME), Median) ~ Min + p95, 
#		data = subset(all_summ_data, label == "FULL_DATA" & TransNames == "LW_03_select_Submit_job_doc_search" & TEST_CASE == 'TEST1'), 
#		draw.bands = FALSE, centers = Median,
#		segments.fun = panel.arrows, ends = "both",
#		xlab="MORE STUFF", xlab.cex=3,
#		angle = 90, length = 1, unit = "mm", cex=1.2, pch=15, pch.cex=2,
#		main=paste("SHIT", "\nMin -- Median -- p95", sep=" ") , main.cex=5 )
#plot$y.scales$cex <- c(1.1,1.1)
#plot$x.scales$cex <- c(1.2,1.2)
#print(plot)
#str(plot)

#load("ALL_LRdata.Rdata")

###----------------------------------------
###    MODEL DATA
###----------------------------------------

load("subset_LRraw_data.Rdata")


rpt <- subset(all_summ_data, grepl("^DEL", TEST_NAME) | grepl("3_2", TEST_NAME) | grepl("NEW", TEST_NAME) | grepl("5_2", TEST_NAME))

rpt <- subset(all_summ_data, TransNames == "Clump_03_select_Submit_button" & TEST_CASE == "TEST1" & grepl("TEST", TEST_NAME))[, "Median"]

rpt <- data.frame(seqnum=seq(1:5), val=rpt)

model.glm <- glm(rpt$val ~ rpt$seqnum, inverse.gaussian, weights=as.numeric(seq(1:5)))
model.lm <- lm(rpt ~ seq(1:5))

dev.off()

plot(rpt, pch=15, col='black', ylim=c(0,ifelse(max(model.lm$fitted.values) > max(model.glm$fitted.values), max(model.lm$fitted.values), max(model.glm$fitted.values))))
points(model.glm$fitted.values, col='red', pch=20)
lines(model.glm$fitted.values, type='l', col='red')
points(model.lm$fitted.values, col='orange')
lines(model.lm$fitted.values, type='l', col='orange')


shit <- predict.glm(model.glm, data.frame(seqnum=seq(from=6, to=10, by=1), val=rep(0,5)))
str(shit)
str(fitted)
predict.glm(fitted)

fitted.values

str(model.lm)

?glm

plot(rpt)

model.lm <- lm(rpt ~ seq(1:5))
model.nls <- nls(rpt ~ seq(1:5))
model.poly <- polym(rpt, degree=3)
plot(model.poly)

fitted <- glm(seq(1:5) ~ rpt, inverse.gaussian, weights=as.numeric(rpt))

plot(rpt, pch=15, col='black', ylim=c(0,7))
points(fitted$fitted.values, col='red', pch=20)
lines(fitted$fitted.values, type='l', col='red')

plot(predict(model.nls), type='l', ylim=c(0,6))
points(rpt, col='red')

predict(m, seq(1:10), se.fit = TRUE)

str(m)
rpt

?predict
?lm


indx <- grepl("job", tolower(levels(all_summ_data$TransNames)))
tolower(levels(all_summ_data$TransNames))[indx]



####_---------------------------------------------------------------------------------------------
####_---------------------------------------------------------------------------------------------
###     SEGMENT PLOTS
####_---------------------------------------------------------------------------------------------
####_---------------------------------------------------------------------------------------------
ss_summ_data <- subset(ss_summ_data, label == 'FULL_DATA' 
                       & TransNames != "IJ_01_Login"
                       & TransNames != "IJ_10_Logout")

output <- 'png'
str(ss_summ_data)
topD <- d_ply(ss_summ_data, .(TransNames), function(df) {
  
  TN <- df$TransName[1]
  openGdevice(type=output, file=paste(graphDir, "SS_TRANS", "_TEST1_", TN, sep=""), w=10, h=5, r=108, ps=14)
  
  plot <- segplot(reorder(factor(TEST_NAME), Median) ~ Min + p95, 
                  data = subset(ss_summ_data, label == "FULL_DATA" & TransNames == TN & TEST_CASE == 'TEST1'), 
                  draw.bands = FALSE, centers = Median,
                  segments.fun = panel.arrows, ends = "both", 
                  angle = 90, length = 1, unit = "mm", cex=1, pch=15, pch.cex=2,
                  xlab='Response Time/sec',
                  main=paste(TN, "\nMin -- Median -- p95", sep=" "))
  plot$y.scales$cex <- c(1.1,1.1)
  plot$x.scales$cex <- c(1.2,1.2)
  
  print(plot)
  dev.off()
})
dev.off()

topD <- d_ply(ss_summ_data, .(TransNames), function(df) {
  
  TN <- df$TransName[1]
  openGdevice(type=output, file=paste(graphDir, "Mean_SS_TRANS", "_TEST1_", TN, sep=""), w=10, h=5, r=108, ps=14)
  
  plot <- segplot(reorder(factor(TEST_NAME), Mean) ~ Min + p95, 
                  data = subset(ss_summ_data, label == "FULL_DATA" & TransNames == TN & TEST_CASE == 'TEST1'), 
                  draw.bands = FALSE, centers = Mean,
                  segments.fun = panel.arrows, ends = "both", 
                  angle = 90, length = 1, unit = "mm", cex=1, pch=15, pch.cex=2,
                  xlab='Response Time/sec',
                  main=paste(TN, "\nMin -- Median -- p95", sep=" "))
  plot$y.scales$cex <- c(1.1,1.1)
  plot$x.scales$cex <- c(1.2,1.2)
  
  print(plot)
  dev.off()
})
dev.off()


topD <- d_ply(ss_summ_data, .(TransNames), function(df) {
  
  TN <- df$TransName[1]
  openGdevice(type=output, file=paste(graphDir, "SS_TRANS", "_TEST2_", TN, sep=""), w=10, h=5, r=108, ps=14)
  
  plot <- segplot(reorder(factor(TEST_NAME), Median) ~ Min + p95, 
                  data = subset(ss_summ_data, label == "FULL_DATA" & TransNames == TN & TEST_CASE == 'TEST2'), 
                  draw.bands = FALSE, centers = Median,
                  segments.fun = panel.arrows, ends = "both", 
                  angle = 90, length = 1, unit = "mm", cex=1, pch=15, pch.cex=2,
                  xlab='Response Time/sec',
                  main=paste(TN, "\nMin -- Median -- p95", sep=" "))
  plot$y.scales$cex <- c(1.1,1.1)
  plot$x.scales$cex <- c(1.2,1.2)
  
  print(plot)
  dev.off()
})
dev.off()


##----------------------------------------------------------
## PLOT - Resources
##----------------------------------------------------------

pp_ggplot2 <- ggplot2like()
pp_ggplot2$strip.border      <- list(col="grey60")
pp_ggplot2$axis.line         <- list(col="grey60")
pp_ggplot2$axis.text         <- list(col="black")

output <- "png"
ss <- subset(all_perfmon_data, server == "MAOPFS02_sql" & TEST_CASE == 'TEST1')
ss <- subset(ss, variable %in% c("cpuUtil",))
ss$TEST_NAME <- as.factor(ss$TEST_NAME)
ss$TEST_CASE <- as.factor(ss$TEST_CASE)

str(ss)

byVar <- tapply(ss$value, ss$variable, my.func)  
byVar <- do.call('rbind', byVar)
byVar

topD <- d_ply(ss, .(server, TEST_NAME), function(df) {
  
  srv <- df$server[1]
  openGdevice(type=output, file=paste(graphDir, TEST_NAME, "_", TEST_CASE, '_SERVER_', server, sep=""), w=14.25, h=7.15, r=108, ps=14)
  
  print(xyplot(value ~ POSIX | variable, data=df, 
               type='l', 
               layout=c(1,6),
               as.table=TRUE,
               strip=FALSE,
               strip.left = TRUE,
               par.strip.text = list(cex = .8),
               par.settings=pp_ggplot2,
               main=paste(srv, " - Server Metrics"),
               sub=paste(startTime, " to ", endTime),
               xlab="Time",
               ylab="Value\n**Observe scale limits**",
               axis = axis.grid,
               scales=list(y=list(relation="free", rot=0, cex=.5, alternating=3),
                           x = list(rot = 45, tick.number=24, cex=.85, format="%m/%d-%H:%M"))
  )
  )
  
  dev.off()
  
})
dev.off()


##----------------------------------------------------------
## PLOT - boxplot
##----------------------------------------------------------

pp_ggplot2 <- ggplot2like()
pp_ggplot2$strip.border      <- list(col="grey60")
pp_ggplot2$axis.line         <- list(col="grey60")
pp_ggplot2$axis.text         <- list(col="black")

output <- "png"
ss <- subset(all_perfmon_data, server == "MAOPFS02_sql" & TEST_CASE == 'TEST1')
metric_list <- c("cpuUtil",
                 "pdTotAvgDiskRd",
                 "pdTotAvgDiskWr",
                 "pdTotPctDiskTime",
                 "pdTotAvgQLen",
                 "ProcQ",
                 "AvailMB",
                 "PctPageFileUtil",
                 "PctPageTotUsage",
                 "AM_extAllSec",
                 "AM_FullScanSec",
                 "AM_IndxSrchSec",
                 "AM_TblLockEscalSec",
                 "AM_WrkFilCrtSec",
                 "AM_WrkTblCrtSec",
                 "BM_CacheHitRatio",
                 "userConnect",
                 "Ltch_AvgWaitTimeMs",
                 "Ltch_WaitsSec",
                 "Ltch_TotWaitTimeMs" ,
                 "Lock_AvgWaitTimeMs",
                 "Lock_TimeoutSec",
                 "Lock_WaitTimeMs",
                 "Lock_NumDeadLockSec",
                 "batchReqSec",
                 "sqlCompileSec")
ss <- subset(all_perfmon_data, variable %in% metric_list)

ss$TEST_NAME <- as.factor(ss$TEST_NAME)
ss$TEST_CASE <- as.factor(ss$TEST_CASE)

str(ss)

byVar <- tapply(ss$value, ss$variable, my.func)  
byVar <- do.call('rbind', byVar)
byVar

listOfVars <- unique(as.character(ss$variable))

for (n in listOfVars) {
  ss_v <- subset(ss, variable == n)
  remove_indx <- trimOutliers(ss_v$value)
  
  if (exists("trimmed_ss")) {
    trimmed_ss <- rbind(trimmed_ss, ss_v[-remove_indx,])
  } else {
    trimmed_ss <- ss_v[-remove_indx,]
  }
  
}

warnings()
str(ss)
str(trimmed_ss)
rm(trimmed_ss)

ss <- trimmed_ss
topD <- d_ply(ss, .(variable), function(df) {
  
  VAR <- df$variable[1]
  openGdevice(type=output, file=paste(graphDir, TEST_NAME, "_", TEST_CASE, '_SERVER_', VAR, sep=""), w=10, h=5, r=108, ps=14)
  
  print(bwplot(value ~ TEST_NAME, data=df, 
               type='l', 
               #		layout=c(1,6),
               #as.table=TRUE,
               #strip=FALSE,
               #strip.left = TRUE,
               #par.strip.text = list(cex = .8),
               #par.settings=pp_ggplot2,
               main=paste(VAR, " - Server Metrics"),
               #sub=paste(startTime, " to ", endTime),
               #xlab="Time",
               #ylab="Value\n**Observe scale limits**",
               axis = axis.grid,
               #scales=list(y=list(relation="free", rot=0, cex=.5, alternating=3),
               #		x = list(rot = 45, tick.number=24, cex=.85, format="%m/%d-%H:%M"))
  )
  )
  
  dev.off()
  
})
dev.off()


#### CPU CHART

cast_ss <- cast(subset(all_perfmon_data, variable == 'cpuUtil' & server == 'MAOPFS02_sql'), id=c("server","POSIX"))  

cast_ss$brks1min <- cut(cast_ss$POSIX, seq(trunc(min(cast_ss$POSIX), units='hours'), trunc(max(cast_ss$POSIX)+3600, units='hours'), by='1 min'))
cast_ss$brks5min <- cut(cast_ss$POSIX, seq(trunc(min(cast_ss$POSIX), units='hours'), trunc(max(cast_ss$POSIX)+300, units='hours'), by='5 min'))

describe(cast_ss)

str(cast_ss)
listOfMetrics <- c("cpuUtil","")
require(ggplot2)
cpu <- tapply(cast_ss$cpuUtil, cast_ss$brks1min, sao.func)
cpuSumm <- data.frame(do.call("rbind", cpu))
all_dates <- as.POSIXct(levels(as.factor(as.character(cast_ss$brks1min))))
str(all_dates)
summary(all_dates)
cpuSumm <- cbind(POSIX=all_dates, cpuSumm)
str(cpuSumm)

for (d in levels(as.factor(cast_ss$TEST_NAME))) {
  sugar <- subset()
}

plot(cpuSumm$POSIX, cpuSumm$Max, type='l', col=alpha("red", .7))
lines(cpuSumm$POSIX, cpuSumm$p95, type='l', col=alpha("blue", .7))
lines(cpuSumm$POSIX, cpuSumm$Min, type='l', col=alpha("green", .7))
lines(cpuSumm$POSIX, cpuSumm$Mean, type='l', col=alpha("black", .7))



ss <- subset(all_perfmon_data, server == "MAOPFS02_sql" & variable == 'cpuUtil' & TEST_CASE == 'TEST1')
ss$server <- as.factor(as.character(ss$server))
ss$variable <- as.factor(as.character(ss$variable))
ss$TEST_NAME <- as.factor(as.character(ss$TEST_NAME))
ss$TEST_CASE <- as.factor(as.character(ss$TEST_CASE))

shit <- ddply(ss, .(TEST_NAME), function(df) {
  seqNum <- seq(1:nrow)
}) 

ss <- subset(all_perfmon_data, server == "MAOPFS02_sql" & variable == 'cpuUtil' & TEST_CASE == 'TEST1' & TEST_NAME == 'BASELINE')

plot(ss$POSIX, ss$value, type='l')


print(xyplot(scvt ~ et | operation + test, data=in_data, 
             #				type='s', 
             col="#F781BF50",
             pch=19,
             #				ylim=c(0,50000),
             xlim=c(0,20),				
             as.table=TRUE,
             #				layout=c(5,3),
             par.strip.text = list(cex = .8),
             par.settings=pp_ggplot2,
             axis = axis.grid,
             xlab="Elapsed Time",
             ylab="Service Time",
             #				sub=paste("SOMTHING NOTE WORTHY"),
             main=paste("IO Testing\nElapsed Time vs. Service Time")
             
)
)


str(ss)


##----------------------------------------------------------
## PLOT - SUBSET Transactions DEPENDENT ON THE X
##----------------------------------------------------------

pp_ggplot2 <- ggplot2like()
pp_ggplot2$strip.border      <- list(col="grey60")
pp_ggplot2$axis.line         <- list(col="grey60")
pp_ggplot2$axis.text         <- list(col="black")

byVar <- tapply(all_LR_data$trans_resp_time$value, ss$variable, my.func)  
byVar <- do.call('rbind', byVar)
byVar

#plot <- segplot(reorder(factor(TEST_NAME), Median) ~ Min + p95, 
#		data = subset(all_summ_data, label == "FULL_DATA" & TransNames == "LW_03_select_Submit_job_doc_search" & TEST_CASE == 'TEST1'), 
#		draw.bands = FALSE, centers = Median,
#		segments.fun = panel.arrows, ends = "both",
#		xlab="MORE STUFF", xlab.cex=3,
#		angle = 90, length = 1, unit = "mm", cex=1.2, pch=15, pch.cex=2,
#		main=paste("SHIT", "\nMin -- Median -- p95", sep=" ") , main.cex=5 )
#plot$y.scales$cex <- c(1.1,1.1)
#plot$x.scales$cex <- c(1.2,1.2)
#print(plot)
#str(plot)

#load("ALL_LRdata.Rdata")
load("subset_LRraw_data.Rdata")
ss_summ_data <- subset(ss_summ_data, label == 'FULL_DATA' 
                       & TransNames != "IJ_01_Login"
                       & TransNames != "IJ_10_Logout")

output <- 'png'

ss_summ_data$TEST_NAME <- factor(ss_summ_data$TEST_NAME, labels=c("BASELINE","TEST_3_2MM","TEST_3_7MM","TEST_4_2MM","TEST_4_7MM","TEST_5_2MM"))
ss_summ_data$TN <- as.numeric(ss_summ_data$TEST_NAME)

my_red   <- "#E41A1CAF" 
my_blue  <- "#377EB8AF" 
my_green <- "#4DAF4AAF" 
my_violet <- "#984EA3AF" 
my_orange <- "#FF7F00AF"



test_data <- subset(ss_summ_data, TransNames == "LW_11_select_Home_link" & TEST_CASE == 'TEST1')
cat("TN = ",Tranname, "\n")

openGdevice(type=output, file=paste(graphDir, "PLOT_TRANS", "_TEST1_", "LW_11_select_Home_link", sep=""), w=10, h=5, r=108, ps=14)

plot(test_data$TN, test_data$p95, type='p', pch=15, col=my_red, 
     axes=FALSE, 
     ylim=c(0,max(test_data$p95)),
     ylab="Response Time",
     xlab="Database Size",
     main="LW_11_select_Home_link")

for (s in 1:6) {
  segments(s, test_data$Min[s], s, test_data$p95[s],  col=my_green)
  text(s, round(test_data$Median[s],1), labels=round(test_data$Median[s],1), col='black', pos=3, cex=.8)
}

points(test_data$TN, test_data$p95, pch=15, col=my_red)
points(test_data$TN, test_data$Median, pch=18, col=my_blue)
lines(test_data$TN, test_data$Median, col=my_blue)
points(test_data$TN, test_data$Min, pch=15, col=my_green)


axis(1, at = seq(1:6), labels =c("BASELINE","3_2MM","3_7MM","4_2MM","4_7MM","5_2MM"))
axis(2, at = seq(from=0, to=max(test_data$p95), by=.5))

legend("topleft", c("p95","Median", "Min"), pch=c(15,18,15), 
       col=c(my_red,my_blue,my_green),
       title="Legend", inset=.05 )

dev.off()


###------------------------------------------------------------

test_data <- subset(ss_summ_data, TransNames == "Clump_04_select_Document" & TEST_CASE == 'TEST1')
cat("TN = ",Tranname, "\n")

openGdevice(type=output, file=paste(graphDir, "PLOT_TRANS", "_TEST1_", "Clump_04_select_Document", sep=""), w=10, h=5, r=108, ps=14)

plot(test_data$TN, test_data$p95, type='p', pch=15, col=my_red, 
     axes=FALSE, 
     ylim=c(0,max(test_data$p95)),
     ylab="Response Time",
     xlab="Database Size",
     main="Clump_04_select_Document")

for (s in 1:6) {
  segments(s, test_data$Min[s], s, test_data$p95[s],  col=my_green)
  text(s, round(test_data$Median[s],1), labels=round(test_data$Median[s],1), col='black', pos=3, cex=.8)
}

points(test_data$TN, test_data$p95, pch=15, col=my_red)
points(test_data$TN, test_data$Median, pch=18, col=my_blue)
lines(test_data$TN, test_data$Median, col=my_blue)
points(test_data$TN, test_data$Min, pch=15, col=my_green)


axis(1, at = seq(1:6), labels =c("BASELINE","3_2MM","3_7MM","4_2MM","4_7MM","5_2MM"))
axis(2, at = seq(from=0, to=max(test_data$p95), by=.2))

legend("topleft", c("p95","Median", "Min"), pch=c(15,18,15), 
       col=c(my_red,my_blue,my_green),
       title="Legend", inset=.05 )

dev.off()






str(ss_summ_data)

trans_DATA <- c("BB_06_select_Doc_Statistics link",
                "BB_05_select Primery Documents link",
                "Clump_03_select Submit button",
                "CPJR_09_select Submit job doc search button",
                "CPJR_06_select Parent ID link",
                "PW_13_select_Submit_button",
                "CPJR_06_select Parent ID link",
                "LW_03_select Submit job doc search",
                "PW_06_select Submit button")

## TRANS_NAMES THAT HEAVY DATA, RESPONSE TIME TRANS
trans_DATA <- c("BB_06_select Doc Statistics link",
                "Clump_03_select Submit button",
                "SIS_03_select_Submit_button",
                "PW_13_select_Submit_button",
                "IJ_07_select_Associated_Documentation",
                "Clump_04_select_Document",
                "CPJR_09_select Submit job doc search button",
                "LW_03_select Submit job doc search")

## TAKE THE SPACES
trans_DATA <- gsub("\\ ", "\\_", trans_DATA)


## chnage 3.859 to 6.01
ss_summ_data$Median[ss_summ_data$TransNames == "IJ_07_select_Associated_Documentation" & ss_summ_data$TEST_CASE == 'TEST1' & ss_summ_data$TEST_NAME == 'TEST_4_7MM'] <- 6.01

###--------------------------------------------------------------------------
###  THIS IS THE CHARTS FOR ECM TESTING - TRANSACTION DATA, COMPARISONS 
###       BETWEEN TESTS:
###                  BASE and LARGEDATA CHARTS
###--------------------------------------------------------------------------
###   BASE CHART
###
logDir <- (paste("C:/Data/performance_data/20120312_ECM_Documentum/performance_tests/20120802_SCALABILITY/reports/", sep=""))
graphDir <- (paste("C:/Data/performance_data/20120312_ECM_Documentum/performance_tests/20120802_SCALABILITY/graphics/V2/", sep=""))
setwd(logDir)
load("ALL_LRdata.Rdata")
###
###--------------------------------------------------------------------------


my_red   <- "#E41A1CAF" 
my_blue  <- "#377EB8AF" 
my_green <- "#4DAF4AAF" 
my_violet <- "#984EA3AF" 
my_orange <- "#FF7F00AF"

output <- 'png'

str(all_summ_data)
plot_summ_data <- all_summ_data

plot_summ_data$lvl <- 99
plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L25vuser"] <- 1
plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L50vuser"] <- 2
plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L75vuser"] <- 3
plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L100vuser"] <- 4
plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L125vuser"] <- 5
plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L150vuser"] <- 6

plot_summ_data <- subset(plot_summ_data, lvl <= 6)

plot_summ_data$lvl <- factor(plot_summ_data$lvl)

#plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L25vuser"] <- 1
#plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L25vuser"] <- 1
#plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L25vuser"] <- 1
#plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L25vuser"] <- 1
#plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L25vuser"] <- 1
#plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L25vuser"] <- 1
#,"L50vuser","L50_LD","L75vuser","L100vuser","L100_LD","L125vuser","L150vuser","L150_FT","L150_LD"
#plot_summ_data$TEST_NAME <- factor(plot_summ_data$TEST_NAME,  ordered=FALSE,
#										levels=c("L25vuser","L50vuser","L50_LD","L75vuser","L100vuser","L100_LD","L125vuser","L150vuser","L150_FT","L150_LD"))
#options(width=160)
#str(plot_summ_data)




subset(plot_summ_data, TransNames == '02_Select Search button JobNumber' & TEST_CASE == "TEST1" & label == 'TRIM_DATA')

levels(plot_summ_data$TEST_NAME)

plot_summ_data$TN <- as.numeric(as.character(plot_summ_data$lvl))
plot_summ_data$TransNames <- as.character(plot_summ_data$TransNames)

#TN =  p90 data  1 2 4 5 7 8 9 10 3 6
#TN =  Med data  1 2 4 5 7 8 9 10 3 6
#TN =  Min data  1 2 4 5 7 8 9 10 3 6

#0.724 0.744 0.734 0.797 0.875 1.172 1.203 6.016 2.07  1.625  
#0.535 0.578 0.547 0.563 0.582 0.688 0.672 2.906 1.016 1.016 
#0.419 0.486 0.47  0.458 0.47  0.477 0.465 1.016 0.769 0.801 


topD <- d_ply(plot_summ_data, .(TransNames), function(df) {
  
  Tranname <- df$TransNames[1]
  plot_data <- subset(df, TransNames == Tranname & TEST_CASE == 'TEST1' & label == 'TRIM_DATA')
  cat("TN = ", "p90 data ", plot_data$TN, plot_data$p90, "\n")
  cat("TN = ", "Med data ", plot_data$TN, plot_data$Median, "\n")
  cat("TN = ", "SD data ", plot_data$TN, plot_data$SD, "\n")
  cat("TN = ", "Min data ", plot_data$TN, plot_data$Min, "\n")
  openGdevice(type=output, file=paste(graphDir, "BASE_TRANS", "_TEST1_", Tranname, sep=""), w=8, h=5, r=108, ps=12)
  
  plot(plot_data$TN, plot_data$p90, type='p', pch=15, col=my_red, 
       axes=FALSE, 
       ylim=c(0,max(plot_data$p90)),
       ylab="Response Time (seconds)",
       xlab="Number of Users",
       main=paste("ECM Foundation Scalability Testing\nTransaction: ",Tranname))
  points(plot_data$TN, plot_data$Median, pch=18, col=my_blue)
  lines(plot_data$TN, plot_data$Median, col=my_blue)
  points(plot_data$TN, plot_data$Min, pch=15, col=my_green)
  
  for (s in 1:6) {
    segments(s, plot_data$Min[s], s, plot_data$p90[s],  col=my_green)
    text(s, round(plot_data$Median[s],1), labels=round(plot_data$Median[s],3), col='black', pos=3, cex=.7)
  }
  
  axis(1, at = seq(1:6), cex.axis=.7, 
       labels =c("L25vuser","L50vuser","L75vuser","L100vuser","L125vuser","L150vuser"))
  axis(2, at = seq(from=0, to=max(plot_data$p90)*1.2, by=.2))
  
  legend("topleft", c("p90","Median", "Min"), pch=c(15,18,15), 
         col=c(my_red,my_blue,my_green),
         title="Legend", inset=.05 )
  
  dev.off()
})
dev.off()


###--------------------------------------------------------------------------
###    LARGE DATA
###--------------------------------------------------------------------------
str(all_summ_data)
plot_summ_data <- all_summ_data

###---------
###  FIX TEST2, COPY TEST FOR L100_LD

hold_summ_data <- subset(plot_summ_data, TEST_NAME == 'L100_LD' & TEST_CASE == 'TEST1' & label == 'TRIM_DATA')
hold_summ_data$TEST_CASE <- 'TEST2'
hold_summ_data$test_num <- 'TEST2'


plot_summ_data <- rbind(plot_summ_data, hold_summ_data)



plot_summ_data$lvl <- 99

plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L50vuser"] <- 1
plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L50_LD"] <- 2
plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L100vuser"] <- 3
plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L100_LD"] <- 4
plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L150vuser"] <- 5
plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L150_LD"] <- 6

plot_summ_data <- subset(plot_summ_data, lvl <= 6)

plot_summ_data <- sort.data.frame(plot_summ_data,~lvl)

#plot_summ_data$lvl <- factor(plot_summ_data$lvl)

#plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L25vuser"] <- 1
#plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L25vuser"] <- 1
#plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L25vuser"] <- 1
#plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L25vuser"] <- 1
#plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L25vuser"] <- 1
#plot_summ_data$lvl[plot_summ_data$TEST_NAME == "L25vuser"] <- 1
#,"L50vuser","L50_LD","L75vuser","L100vuser","L100_LD","L125vuser","L150vuser","L150_FT","L150_LD"
#plot_summ_data$TEST_NAME <- factor(plot_summ_data$TEST_NAME,  ordered=FALSE,
#										levels=c("L25vuser","L50vuser","L50_LD","L75vuser","L100vuser","L100_LD","L125vuser","L150vuser","L150_FT","L150_LD"))
#options(width=160)
#str(plot_summ_data)




subset(plot_summ_data, TransNames == '02_Select Search button JobNumber' & TEST_CASE == "TEST1" & label == 'TRIM_DATA')
levels(plot_summ_data$TEST_NAME)

#plot_summ_data$TN <- as.numeric(as.character(plot_summ_data$lvl))
plot_summ_data$TN <- plot_summ_data$lvl
plot_summ_data$TransNames <- as.character(plot_summ_data$TransNames)

#TN =  p90 data  1 2 4 5 7 8 9 10 3 6
#TN =  Med data  1 2 4 5 7 8 9 10 3 6
#TN =  Min data  1 2 4 5 7 8 9 10 3 6

#0.724 0.744 0.734 0.797 0.875 1.172 1.203 6.016 2.07  1.625  
#0.535 0.578 0.547 0.563 0.582 0.688 0.672 2.906 1.016 1.016 
#0.419 0.486 0.47  0.458 0.47  0.477 0.465 1.016 0.769 0.801 


output <- 'png'

my_red   <- "#E41A1CAF" 
my_blue  <- "#377EB8AF" 
my_green <- "#4DAF4AAF" 
my_violet <- "#984EA3AF" 
my_orange <- "#FF7F00AF"

rpt_TEST_CASE <- 'TEST1'
graphDir <- paste(graphDir, "V2/", sep="")
topD <- d_ply(plot_summ_data, .(TransNames), function(df) {
  
  Tranname <- df$TransNames[1]
  plot_data <- subset(df, TransNames == Tranname & TEST_CASE == rpt_TEST_CASE & label == 'TRIM_DATA')
  cat("TN = ", "p90 data ", plot_data$TN, plot_data$p90, "\n")
  cat("TN = ", "Med data ", plot_data$TN, plot_data$Median, "\n")
  cat("TN = ", "SD data ", plot_data$TN, plot_data$SD, "\n")
  cat("TN = ", "Min data ", plot_data$TN, plot_data$Min, "\n")
  openGdevice(type=output, file=paste(graphDir, "LD_TRANS", "_", rpt_TEST_CASE, "_", Tranname, sep=""), w=8, h=5, r=108, ps=12)
  
  plot(plot_data$TN, plot_data$p90, type='p', pch=15, col=my_red, 
       #plot(plot_data$TN, plot_data$p90, type='l', pch=15, cex=3, col='green',					
       axes=FALSE, 
       ylim=c(0,max(plot_data$p90)),
       ylab="Response Time (seconds)",
       xlab="Number of Users",
       main=paste("ECM Foundation Scalability Testing", "\nBase vs. Large Dataset\nTransaction: ",Tranname))
  #main=paste("ECM Foundation Scalability Testing - ", rpt_TEST_CASE, "\nBase vs. Large Dataset\nTransaction: ",Tranname))
  
  
  points(plot_data$TN, plot_data$Min, pch=15, col=my_green)
  
  ## BASE
  points(plot_data$TN[c(1,3,5)], plot_data$Median[c(1,3,5)], pch=18, col=my_blue)
  lines(plot_data$TN[c(1,3,5)], plot_data$Median[c(1,3,5)], col=my_blue, lwd=2)
  for (s in c(1,3,5)) {
    segments(s, plot_data$Min[s], s, plot_data$p90[s],  col=my_green)
    text(s, round(plot_data$Median[s],1), labels=round(plot_data$Median[s],3), col='black', pos=3, cex=.7)
  }
  
  ## LD
  points(plot_data$TN[c(2,4,6)], plot_data$Median[c(2,4,6)], pch=18, col=my_red)
  lines(plot_data$TN[c(2,4,6)], plot_data$Median[c(2,4,6)], col=my_red, lwd=2)
  for (s in c(2,4,6)) {
    segments(s, plot_data$Min[s], s, plot_data$p90[s],  col=my_green)
    text(s, round(plot_data$Median[s],1), labels=round(plot_data$Median[s],3), col=my_red, pos=3, cex=.7)
  }
  
  if (rpt_TEST_CASE == 'TEST1') {
    axis(1, at = seq(1:6), cex.axis=.7,
         labels =c("L50vuser","L50_LD","100vuser","L100_LD","L50vuser","L150_LD"))
  } else {
    ### LABEL FOR TEST2
    axis(1, at = seq(1:6), cex.axis=.7,
         labels =c("L50vuser","L50_LD","100vuser","L100_LD\n(TEST1)","L50vuser","L150_LD"))
    
  }
  
  axis(2, at = seq(from=0, to=max(plot_data$p90)*1.2, by=.2))
  
  legend("topleft", c("p90","Base Median", "LD Median", "Min"), pch=c(15,18,18,15), 
         col=c(my_red,my_blue,my_red,my_green),
         text.col=c('black',my_blue,my_red,'black'),
         title="Legend", inset=.05,
         cex=.9)
  
  dev.off()
})
dev.off()


##----------------------------------------------------------
## PLOT - TRANSACTION SUMMARY DATA
##----------------------------------------------------------
str(all_summ_data)
levels(all_summ_data$TransNames)
ts <- subset(all_summ_data, label == "FULL_DATA" & TransNames == "LW_03_select_Submit_job_doc_search" & TEST_CASE == 'TEST1')
str(ts)
boxplot(Median ~ TEST_NAME, data=ts)

xyplot(ts$TEST_NAME ~ ts$Median)



pp_ggplot2 <- ggplot2like()
pp_ggplot2$strip.border      <- list(col="grey60")
pp_ggplot2$axis.line         <- list(col="grey60")
pp_ggplot2$axis.text         <- list(col="black")

output <- "png"

openGdevice(type=output, file="p:/temp/ALL_et_vs_svct", w=10, h=14, r=72, ps=12)

print(xyplot(scvt ~ et | operation + test, data=in_data, 
             #				type='s', 
             col="#F781BF50",
             pch=19,
             #				ylim=c(0,50000),
             xlim=c(0,20),				
             as.table=TRUE,
             #				layout=c(5,3),
             par.strip.text = list(cex = .8),
             par.settings=pp_ggplot2,
             axis = axis.grid,
             xlab="Elapsed Time",
             ylab="Service Time",
             #				sub=paste("SOMTHING NOTE WORTHY"),
             main=paste("IO Testing\nElapsed Time vs. Service Time")
             
)
)
dev.off()


y <- c(.5,.6,.75,1,1.1,1.3)
x <- c(2,3,4,5,6,7)

plot(x,y, ylim=c(0,2), type='l', lwd=2, col='red',
     main="ECTS Short Query Response Time",
     ylab="Seconds",
     xlab="Number of Images (millions)")
points(x,y, col='red', pch=19)


##----------------------------------------------------------
##----------------------------------------------------------
## PLOT - SUMMARY OF THE CPU UTILIZATION, WITH SELECT PERFORMANCE DATA
##----------------------------------------------------------
##----------------------------------------------------------

options(width=80)

select_TN <- c("BASELINE", 
               "TEST_3_2MM",
               "TEST_3_7MM",
               "TEST_4_2MM",
               "TEST_4_7MM",
               "TEST_5_2MM")
select_TC <- c("TEST1", 
               "TEST2",
               "TEST2",
               "TEST1",
               "TEST2",
               "TEST2")

rm(ss_cpu_data)
for (tn in 1:6) {
  
  cat("TEST_NAME = ", select_TN[tn], " TEST_CASE = ", select_TC[tn], "\n")
  Dir <- (paste("C:/Data/performance_data/20110920_MAOP/PERFORMANCE_TESTING/", select_TN[tn], "/", select_TC[tn], "/", sep=""))
  ###------------------------------------------------------
  ###   LOAD PARAMETER FILE
  ###------------------------------------------------------
  parms <- read.csv(paste(Dir, "test_parameters.txt", sep=""), stringsAsFactors=FALSE)
  start_time <- as.POSIXct(parms$start_time[parms$task == "loadrunner"])
  end_time <- as.POSIXct(parms$end_time[parms$task == "loadrunner"])
  
  test_start <- start_time + 60*15
  test_end   <- start_time + 60*105
  
  temp_cpu <- subset(all_perfmon_data, POSIX >= test_start & 
                       POSIX <= test_end & 
                       server == "MAOPFS02_sql" & 
                       TEST_CASE == select_TC[tn] & 
                       variable == "cpuUtil")
  
  if (nrow(temp_cpu) > 0) {
    
    seq_num <- seq(from=1, to=nrow(temp_cpu), by=1)
    
    temp_cpu <- cbind(temp_cpu, seq_num)
    
    if(exists("ss_cpu_data")) {
      ss_cpu_data <- rbind(ss_cpu_data, temp_cpu)
    } else {
      ss_cpu_data <- temp_cpu
    }
  }
  
  
  
}

ss_cpu_data$server <- as.factor(as.character(ss_cpu_data$server))
ss_cpu_data$variable <- as.factor(as.character(ss_cpu_data$variable))
ss_cpu_data$TEST_CASE <- as.factor(as.character(ss_cpu_data$TEST_CASE))
str(ss_cpu_data)

levels(ss_cpu_data$variable)


rm(CPUSUMM)
for (tn in TEST_NAME) {
  
  cast_ss <- cast(subset(ss_cpu_data, variable == 'cpuUtil' & server == 'MAOPFS02_sql' & TEST_NAME == tn), id=c("server","POSIX"))  
  
  cast_ss$brks1min <- cut(cast_ss$POSIX, seq(trunc(min(cast_ss$POSIX), units='hours'), trunc(max(cast_ss$POSIX)+3600, units='hours'), by='1 min'))
  cast_ss$brks5min <- cut(cast_ss$POSIX, seq(trunc(min(cast_ss$POSIX), units='hours'), trunc(max(cast_ss$POSIX)+3600, units='hours'), by='2 min'))
  
  #describe(cast_ss)
  
  cpu <- tapply(cast_ss$cpuUtil, cast_ss$brks5min, sao.func)
  cpuSumm <- data.frame(do.call("rbind", cpu))
  all_dates <- as.POSIXct(levels(as.factor(as.character(cast_ss$brks5min))))
  str(all_dates)
  summary(all_dates)
  cpuSumm <- cbind(POSIX=all_dates, cpuSumm, tn, seq_num=seq(1,nrow(cpuSumm), by=1))
  
  if(exists("CPUSUMM")) {
    CPUSUMM <- rbind(CPUSUMM, cpuSumm)
  } else {
    CPUSUMM <- cpuSumm
  }
  
}



TName <- c(
  "TEST_3_2MM",
  "TEST_3_7MM",
  "TEST_4_2MM",
  "TEST_4_7MM",
  "TEST_5_2MM")

my_col <- return_Set1("AA")
Set1   <- return_Set1(alpha="")
#my_col <- return_hex_colors('YlOrRd', 'AA')[rev(3:9)]
#Set1 <-  return_hex_colors('YlOrRd', '')[rev(3:9)]

output <- 'png'

openGdevice(type=output, file=paste(graphDir, "Summary_CPU_Utilization", sep=""), w=10, h=6.5, r=108, ps=14)

plot_data <- subset(CPUSUMM, tn == 'BASELINE')
plot_data
plot(plot_data$seq_num, plot_data$Mean, 
     type='l', 
     ylim=c(0,100),  
     col=my_col[6], 
     xaxt='n',
     ylab="CPU Utilization",
     xlab='Minutes of testing',
     main='Database Server\nAverage CPU Utilization\nby Database Size')

Avg <- mean(plot_data$Mean)
segment_length <- nrow(plot_data)
segments(0, Avg, segment_length, Avg, lwd=3, col=Set1[6])
c <- 5
for (tname in TName) {
  
  plot_data <- subset(CPUSUMM, tn == tname)
  
  cat("tn = ", tname, "max seq_num", max(plot_data$seq_num), "\n")
  
  lines(plot_data$seq_num, plot_data$Mean, col=my_col[c])
  Avg <- mean(plot_data$Mean)
  segments(0, Avg, segment_length, Avg, lwd=3, col=Set1[c])
  
  
  c <- c + -1
  
}
min(plot_data$POSIX) - max(plot_data$POSIX)
axis(1, at = seq(1:53), labels = seq(0, 105, by=2))

legend("bottomright", c("BL", "3.2MM", "3.7MM", "4.2MM", "4.7MM", "5.2MM"), lty=c(1,1,1,1,1,1), 
       col=Set1[rev(1:6)],
       title="Legend", inset=.05, cex=.7, lwd=rep(3,6) )
str(plot_data)
dev.off()
plot()


##----------------------------------------------------------
##----------------------------------------------------------
## PLOT - ALL PERFMON PERFORMANCE METRICS ACROSS THE SELECT PERFORM DATA
##----------------------------------------------------------
##----------------------------------------------------------

setwd(paste("C:/Data/performance_data/20120312_ECM_Documentum/performance_tests/20120802_SCALABILITY/reports/", sep=''))
load("ALL_VM_data_1.Rdata")

options(width=80)

select_TN <- c("BASELINE", 
               "TEST_3_2MM",
               "TEST_3_7MM",
               "TEST_4_2MM",
               "TEST_4_7MM",
               "TEST_5_2MM")
select_TC <- c("TEST1", 
               "TEST2",
               "TEST2",
               "TEST1",
               "TEST2",
               "TEST2")





##-------------------- DATA --------------------------
## NORMALIZE ALL DATA WITH A SEQ_NUM
##-------------------- DATA --------------------------


select_perfmon_data$server <- as.factor(as.character(select_perfmon_data$server))
select_perfmon_data$variable <- as.factor(as.character(select_perfmon_data$variable))
select_perfmon_data$TEST_CASE <- as.factor(as.character(select_perfmon_data$TEST_CASE))

var_names <- levels(select_perfmon_data$variable)
rm(select_metric_data)

SERVERS <- c("MAOPFS02","MAOPFS02_sql","wwwasc231","wwwasc232","wwwasc233","wwwasc234","wwwasc235")

for (srvr in SERVERS) {
  for (tn in 1:6) {
    
    cat("TEST_NAME = ", select_TN[tn], " TEST_CASE = ", select_TC[tn], "\n")
    Dir <- (paste("C:/Data/performance_data/20110920_MAOP/PERFORMANCE_TESTING/", select_TN[tn], "/", select_TC[tn], "/", sep=""))
    ###------------------------------------------------------
    ###   LOAD PARAMETER FILE
    ###------------------------------------------------------
    parms <- read.csv(paste(Dir, "test_parameters.txt", sep=""), stringsAsFactors=FALSE)
    start_time <- as.POSIXct(parms$start_time[parms$task == "loadrunner"])
    end_time <- as.POSIXct(parms$end_time[parms$task == "loadrunner"])
    
    test_start <- start_time + 60*15
    test_end   <- start_time + 60*105
    
    for (v in var_names) {
      
      cat("  PROCESSING SERVER: ", srvr, " TEST_NAME = ", select_TN[tn], " TEST_CASE = ", select_TC[tn], " VARIBALE = ", v, "\n")
      temp_metric <- subset(select_perfmon_data, POSIX >= test_start & 
                              POSIX <= test_end &
                              variable == v &
                              server == srvr &
                              #							server == "MAOPFS02_sql" |
                              #							server == "MAOPFS02") &						
                              TEST_CASE == select_TC[tn] &
                              TEST_NAME == select_TN[tn])
      
      if (nrow(temp_metric) > 0) {
        
        seq_num <- seq(from=1, to=nrow(temp_metric), by=1)
        
        temp_metric <- cbind(temp_metric, seq_num)
        
        if(exists("select_metric_data")) {
          select_metric_data <- rbind(select_metric_data, temp_metric)
        } else {
          select_metric_data <- temp_metric
        } # END if
        
      } # END if
      
    } # END for
    
  } # END for
} # END for

select_metric_data$server <- as.factor(as.character(select_metric_data$server))
select_metric_data$variable <- as.factor(as.character(select_metric_data$variable))
select_metric_data$TEST_CASE <- as.factor(as.character(select_metric_data$TEST_CASE))
str(select_metric_data)

levels(select_metric_data$variable)




##-------------------- DATA --------------------------
## SUMMARIZE ALL PERFOM DATA WITH sao.func
##-------------------- DATA --------------------------

rm(METRICS_SUMM)

levels()
var_names <- levels(select_perfmon_data$variable)
#SERVERS <- c("MAOPFS02", "MAOPFS02_sql")
#SERVERS <- levels(select_perfmon_data$server)
#SERVERS <- c("MAOPFS02","MAOPFS02_sql","wwwasc231","wwwasc232","wwwasc233","wwwasc234","wwwasc235")

for (srvr in SERVERS) {
  for (tn in 1:6) {
    
    cat("SERVER = ", srvr, " TEST_NAME = ", select_TN[tn], " TEST_CASE = ", select_TC[tn], "\n")
    
    for (v in var_names) {
      
      cat("  PROCESSING TEST_NAME = ", select_TN[tn], " TEST_CASE = ", select_TC[tn], " VARIABLE = ", v, "\n")
      
      cast_ss <- cast(subset(select_metric_data, variable == v & server == srvr & TEST_NAME == select_TN[tn]), id=c("server","POSIX"))  
      
      if (nrow(cast_ss) > 1) {
        cast_ss$brks1min <- cut(cast_ss$POSIX, seq(trunc(min(cast_ss$POSIX), units='hours'), trunc(max(cast_ss$POSIX)+3600, units='hours'), by='1 min'))
        
        #describe(cast_ss)
        
        metric     <- tapply(cast_ss[,7], cast_ss$brks1min, sao.func)
        metricSumm <- data.frame(do.call("rbind", metric))
        all_dates  <- as.POSIXct(levels(as.factor(as.character(cast_ss$brks1min))))
        
        metricSumm    <- cbind(POSIX=all_dates, metricSumm, srvr, select_TN[tn], select_TC[tn], v, seq_num=seq(1,nrow(metricSumm), by=1))
        
        if(exists("METRICS_SUMM")) {
          METRICS_SUMM <- rbind(METRICS_SUMM, metricSumm)
        } else {
          METRICS_SUMM <- metricSumm
        }
      }
    }
  }
}
names(METRICS_SUMM)
header <- c("POSIX","Count","Mean","SD","Min","Median","p95","Max","server","TEST_NAME","TEST_CASE","variable","seq_num")

names(METRICS_SUMM) <- header
head(METRICS_SUMM)
str(METRICS_SUMM)
test_plot <- subset(METRICS_SUMM, TEST_NAME == 'TEST_5_2MM' & variable == 'cpuUtil' & server == 'wwwasc232')
plot(test_plot$seq_num, test_plot$Max, type='l', col='red')
lines(test_plot$seq_num, test_plot$p95, type='l', col='black')
lines(test_plot$seq_num, test_plot$Median, type='l', col='orange')
lines(test_plot$seq_num, test_plot$Mean, type='l', col='green')
lines(test_plot$seq_num, test_plot$Min, type='l', col='magenta')

##----------------------------------------------------------
## PLOT NORMALIZED DATA - boxplot
##----------------------------------------------------------

pp_ggplot2 <- ggplot2like()
pp_ggplot2$strip.border      <- list(col="grey60")
pp_ggplot2$axis.line         <- list(col="grey60")
pp_ggplot2$axis.text         <- list(col="black")

output <- "png"
ss <- subset(select_metric_data, server == "MAOPFS02")
metric_list <- levels(ss$variable) 

#		c("cpuUtil",
#		"pdTotAvgDiskRd",
#		"pdTotAvgDiskWr",
#		"pdTotPctDiskTime",
#		"pdTotAvgQLen",
#		"ProcQ",
#		"AvailMB",
#		"PctPageFileUtil",
#		"PctPageTotUsage",
#		"AM_extAllSec",
#		"AM_FullScanSec",
#		"AM_IndxSrchSec",
#		"AM_TblLockEscalSec",
#		"AM_WrkFilCrtSec",
#		"AM_WrkTblCrtSec",
#		"BM_CacheHitRatio",
#		"userConnect",
#		"Ltch_AvgWaitTimeMs",
#		"Ltch_WaitsSec",
#		"Ltch_TotWaitTimeMs" ,
#		"Lock_AvgWaitTimeMs",
#		"Lock_TimeoutSec",
#		"Lock_WaitTimeMs",
#		"Lock_NumDeadLockSec",
#		"batchReqSec",
#		"sqlCompileSec")

ss <- subset(select_metric_data, variable %in% metric_list)

ss$TEST_NAME <- as.factor(ss$TEST_NAME)
ss$TEST_CASE <- as.factor(ss$TEST_CASE)

str(ss)

listOfVars <- unique(as.character(ss$variable))


## TRIM THE OUTLIERS
#for (n in listOfVars) {
#	ss_v <- subset(ss, variable == n)
#	remove_indx <- trimOutliers(ss_v$value)
#	
#	if (exists("trimmed_ss")) {
#		trimmed_ss <- rbind(trimmed_ss, ss_v[-remove_indx,])
#	} else {
#		trimmed_ss <- ss_v[-remove_indx,]
#	}
#	
#}

###--------
###  PLOT xyplot & bwplot ALL NORMAILZED DATA
###--------

graphDir <- (paste("C:/Data/performance_data/20110920_MAOP/PERFORMANCE_TESTING/graphics_final", "/", sep=""))
topD <- d_ply(ss, .(variable), function(df) {
  
  VAR <- df$variable[1]
  openGdevice(type=output, file=paste(graphDir, "MAOPFS02_SELECT_SERVER_", VAR, sep=""), w=10, h=5, r=108, ps=14)
  
  print(xyplot(value ~ seq_num | TEST_NAME, data=df, 
               type='l', 
               layout=c(1,6),
               as.table=TRUE,
               strip=FALSE,
               strip.left = TRUE,
               par.strip.text = list(cex = .5),
               par.settings=pp_ggplot2,
               main=paste(VAR),
               #sub=paste(startTime, " to ", endTime),
               xlab="Relative Time/Minutes of Testing",
               ylab="Value\n**Observe scale limits**",
               axis = axis.grid,
               scales=list(y=list(relation="free", rot=0, cex=.5, alternating=3),
                           x = list(rot = 45, tick.number=24, cex=.85))
  )
  )
  
  dev.off()
  
  openGdevice(type=output, file=paste(graphDir, "MAOPFS02_SELECT_BW_SERVER_", VAR, sep=""), w=10, h=5, r=108, ps=14)
  
  print(bwplot(value ~ TEST_NAME, data=df, 
               type='l', 
               #		layout=c(1,6),
               #as.table=TRUE,
               #strip=FALSE,
               #strip.left = TRUE,
               #par.strip.text = list(cex = .8),
               #par.settings=pp_ggplot2,
               main=paste(VAR, " - Server Metrics"),
               #sub=paste(startTime, " to ", endTime),
               #xlab="Time",
               #ylab="Value\n**Observe scale limits**",
               axis = axis.grid,
               #scales=list(y=list(relation="free", rot=0, cex=.5, alternating=3),
               #		x = list(rot = 45, tick.number=24, cex=.85, format="%m/%d-%H:%M"))
  )
  )
  
  dev.off()
  
  
})
dev.off()


###--------
###  PLOT xyplot & bwplot FOR TRIMMED NORMALIZED DATA
###--------

## TRIM THE OUTLIERS
for (n in listOfVars) {
  ss_v <- subset(ss, variable == n)
  remove_indx <- trimOutliers(ss_v$value)
  
  if (exists("trimmed_ss")) {
    trimmed_ss <- rbind(trimmed_ss, ss_v[-remove_indx,])
  } else {
    trimmed_ss <- ss_v[-remove_indx,]
  }
  
}

ss <- trimmed_ss
str(ss)
graphDir <- (paste("C:/Data/performance_data/20110920_MAOP/PERFORMANCE_TESTING/graphics", "/", sep=""))
topD <- d_ply(ss, .(variable), function(df) {
  
  VAR <- df$variable[1]
  openGdevice(type=output, file=paste(graphDir, "TRIMMED_SELECT_SERVER_", VAR, sep=""), w=10, h=5, r=108, ps=14)
  
  print(xyplot(value ~ seq_num | TEST_NAME, data=df, 
               type='l', 
               layout=c(1,6),
               as.table=TRUE,
               strip=FALSE,
               strip.left = TRUE,
               par.strip.text = list(cex = .5),
               par.settings=pp_ggplot2,
               main=paste(VAR),
               #sub=paste(startTime, " to ", endTime),
               xlab="Relative Time/Minutes of Testing",
               ylab="Value\n**Observe scale limits**",
               axis = axis.grid,
               scales=list(y=list(relation="free", rot=0, cex=.5, alternating=3),
                           x = list(rot = 45, tick.number=24, cex=.85))
  )
  )
  
  dev.off()
  
  openGdevice(type=output, file=paste(graphDir, "TRIMMED_SELECT_BW_SERVER_", VAR, sep=""), w=10, h=5, r=108, ps=14)
  
  print(bwplot(value ~ TEST_NAME, data=df, 
               type='l', 
               #		layout=c(1,6),
               #as.table=TRUE,
               #strip=FALSE,
               #strip.left = TRUE,
               #par.strip.text = list(cex = .8),
               #par.settings=pp_ggplot2,
               main=paste(VAR, " - Server Metrics"),
               #sub=paste(startTime, " to ", endTime),
               #xlab="Time",
               #ylab="Value\n**Observe scale limits**",
               axis = axis.grid,
               #scales=list(y=list(relation="free", rot=0, cex=.5, alternating=3),
               #		x = list(rot = 45, tick.number=24, cex=.85, format="%m/%d-%H:%M"))
  )
  )
  
  dev.off()
  
  
})
dev.off()


###--------
###  PLOT custom plot  FOR SUMMARIZED DATA
###--------
head(METRICS_SUMM)
graphDir <- (paste("C:/Data/performance_data/20110920_MAOP/PERFORMANCE_TESTING/graphics_final", "/", sep=""))


mSummSumm <- ddply(METRICS_SUMM, .(Entity, variable), function(DF) {
  
  DF$TEST_NAME <- as.character(DF$TEST_NAME)
  
  M     <- tapply(DF$Median, DF$TEST_NAME, sao.func)
  mSumm <- data.frame(do.call("rbind", M))
  mSumm$TEST_NAME <- unique(as.character(DF$TEST_NAME)) 
  
  DF$server <- as.character(DF$server)
  DF$variable <- as.character(DF$variable)
  DF$TEST_NAME <- as.character(DF$TEST_NAME)
  DF$TEST_CASE <- as.character(DF$TEST_CASE)
  
  SRVR <- DF$server[1]
  VAR <- DF$variable[1]
  TN <- DF$TEST_NAME[1]
  TC <- DF$TEST_CASE[1]
  
  #rm(SRVR, VAR, TN, TC)
  
  cat("SERVER = ", SRVR, " TEST_NAME = ", TN, " TEST_CASE = ", TC, " Variable = ", VAR, "\n")
  
  return(mSumm)
})

str(mSummSumm)

varList <- levels(mSummSumm$variable)
for (v in varList) {
  VAR <- paste("\"", v, "\",", sep="")
  cat(VAR, "\n")
}

includeVariables <- c(
  "cpuUser", 
  "cpuUtil",  
  "diskAvgQLen", 
  "diskAvgSecRd", 
  "diskAvgSecWr", 
  "diskAvgWrQLen",  
  "memAvailMB",  
  "n1rcvKB", 
  "n1sntKB", 
  "n1totKB", 
  "n2rcvKB", 
  "n2sntKB", 
  "n2totKB", 
  "net1QueLen",  
  "net2QueLen",  
  "pageFltSec", 
  "pagesSec", 
  "TotRcvKB", 
  "TotSntKB", 
  "writeKB", 
  "AM_extAllSec", 
  "AM_extDeAllSec", 
  "AM_FullScanSec", 
  "AM_IndxSrchSec", 
  "AM_PageSpltSec", 
  "AM_ProbScanSec", 
  "AM_RngScanSec", 
  "AM_TblLockEscalSec", 
  "AM_WrkFilCrtSec", 
  "AM_WrkTblCrtSec", 
  "AvailMB", 
  "batchReqSec", 
  "BM_CacheHitRatio", 
  "BM_ChkPntPageSec", 
  "BM_DbPages", 
  "BM_LazyWrSec", 
  "BM_PageLifeExpct", 
  "BM_PageRdSec", 
  "BM_PageWrSec",  
  "BM_TotPages", 
  "Lock_AvgWaitTimeMs", 
  "Lock_NumDeadLockSec", 
  "Lock_TimeoutSec", 
  "Lock_WaitTimeMs", 
  "Ltch_AvgWaitTimeMs", 
  "Ltch_TotWaitTimeMs", 
  "Ltch_WaitsSec", 
  "MM_GrantPending", 
  "MM_TotSrvMemMB", 
  "MM_TrgtSrvMemMB", 
  "net46OutQlen", 
  "net46TotByteSec",  
  "net47OutQlen", 
  "net47TotByteSec", 
  "pd0AvgQLen", 
  "pd1AvgQLen", 
  "pd2AvgQLen", 
  "pd3AvgQLen", 
  "pd4AvgDiskRd", 
  "pd4AvgDiskWr", 
  "pd4AvgQLen", 
  "pd5AvgDiskRd", 
  "pd5AvgDiskWr", 
  "pd5AvgQLen", 
  "pd6AvgQLen", 
  "pdTotAvgDiskRd", 
  "pdTotAvgDiskWr", 
  "pdTotAvgQLen",  
  "ProcQ", 
  "sqlCompileSec", 
  "TotLogFlsuhSec", 
  "diskWrSec")

ylabsIncVars <- list(
  "cpuUser"="% Utilization", 
  "cpuUtil"="% Utilization",  
  "diskAvgQLen"="Queue Length", 
  "diskAvgSecRd"="Seconds", 
  "diskAvgSecWr"="Seconds", 
  "diskAvgWrQLen"="Queue Length",  
  "memAvailMB"="MB",  
  "n1rcvKB"="KB/sec", 
  "n1sntKB"="KB/sec", 
  "n1totKB"="KB/sec", 
  "n2rcvKB"="KB/sec", 
  "n2sntKB"="KB/sec", 
  "n2totKB"="KB/sec", 
  "net1QueLen"="Queue Length",  
  "net2QueLen"="Queue Length",  
  "pageFltSec"="Flts/sec", 
  "pagesSec"="pages/sec", 
  "TotRcvKB"="KB/sec", 
  "TotSntKB"="KB/sec", 
  "writeKB"="KB/sec", 
  "AM_extAllSec"="Alloc. Ext/sec", 
  "AM_extDeAllSec"="De-Alloc Ext/sec", 
  "AM_FullScanSec"="Full Scan/sec", 
  "AM_IndxSrchSec"="Index Search/sec", 
  "AM_PageSpltSec"="Page Split/sec", 
  "AM_ProbScanSec"="Prob Scan/sec", 
  "AM_RngScanSec"="Range Scan/sec", 
  "AM_TblLockEscalSec"="Escalation/sec", 
  "AM_WrkFilCrtSec"="Wrk File Create/sec", 
  "AM_WrkTblCrtSec"="Wrk Tbl Create/sec", 
  "AvailMB"="MB", 
  "batchReqSec"="Batch Req/sec", 
  "BM_CacheHitRatio"="Hit Ratio", 
  "BM_ChkPntPageSec"="Check Point Pg/sec", 
  "BM_DbPages"="DB Pages", 
  "BM_LazyWrSec"="Lazy Wr/sec", 
  "BM_PageLifeExpct"="Seconds", 
  "BM_PageRdSec"="Page Read/sec", 
  "BM_PageWrSec"="Page Write/sec",  
  "BM_TotPages"="Total Pages", 
  "Lock_AvgWaitTimeMs"="Msec", 
  "Lock_NumDeadLockSec"="Seconds", 
  "Lock_TimeoutSec"="Seconds", 
  "Lock_WaitTimeMs"="Msec", 
  "Ltch_AvgWaitTimeMs"="Msec", 
  "Ltch_TotWaitTimeMs"="Msec", 
  "Ltch_WaitsSec"="Seconds", 
  "MM_GrantPending"="Grants", 
  "MM_TotSrvMemMB"="KB", 
  "MM_TrgtSrvMemMB"="KB", 
  "net46OutQlen"="Queue Length", 
  "net46TotByteSec"="Bytes/sec",  
  "net47OutQlen"="Queue Length", 
  "net47TotByteSec"="Bytes/sec", 
  "pd0AvgQLen"="Queue Length", 
  "pd1AvgQLen"="Queue Length", 
  "pd2AvgQLen"="Queue Length", 
  "pd3AvgQLen"="Queue Length", 
  "pd4AvgDiskRd"="Seconds", 
  "pd4AvgDiskWr"="Seconds", 
  "pd4AvgQLen"="Queue Length", 
  "pd5AvgDiskRd"="Seconds", 
  "pd5AvgDiskWr"="Seconds", 
  "pd5AvgQLen"="Queue Length", 
  "pd6AvgQLen"="Queue Length", 
  "pdTotAvgDiskRd"="Seconds", 
  "pdTotAvgDiskWr"="Seconds", 
  "pdTotAvgQLen"="Queue Length",  
  "ProcQ"="Queue Length", 
  "sqlCompileSec"="Compile/Sec", 
  "TotLogFlsuhSec"="Log Flush/sec", 
  "diskWrSec"="Seconds")

VAR <- "diskWrSec"
YLAB <- unlist(ylabsIncVars[VAR][1])[1]
unlist(ylabsIncVars[VAR][1])[1]

mSummSumm$TEST_NAME <- as.factor(mSummSumm$TEST_NAME)

my_red   <- "#E41A1CAF" 
my_blue  <- "#377EB8AF" 
my_green <- "#4DAF4AAF" 
my_violet <- "#984EA3AF" 
my_orange <- "#FF7F00AF"

levels(as.factor(as.character(mSummSumm$server)))

mSummSumm <- subset(mSummSumm, variable %in% includeVariables)

str(test_ss)
output <- 'png'

topD1 <- d_ply(mSummSumm, .(server), function(df) {
  
  topD2 <- d_ply(df, .(variable), function(DF) {
    
    #			DF$server <- as.character(DF$server)
    #			DF$variable <- as.character(DF$variable)
    #			DF$TEST_NAME <- as.character(DF$TEST_NAME)
    #			DF$TEST_CASE <- as.character(DF$TEST_CASE)
    
    
    SRVR <- as.character(DF$server)[1]
    VAR <- as.character(DF$variable)[1]
    TN <- as.character(DF$TEST_NAME)[1]
    YLAB <- unlist(ylabsIncVars[VAR][1])[1] 
    
    
    #rm(plot_data)
    plot_data <- DF
    
    plot_data$TN <- as.numeric(DF$TEST_NAME)
    
    cat("SERVER = ", SRVR, " TEST_NAME = ", TN, " TEST_CASE = ",  " Variable = ", VAR, " LABLES = ", YLAB, "\n")
    
    openGdevice(type=output, file=paste(graphDir, "FINAL_PLOT_SUMMARY", SRVR, TN, VAR, sep="_"), w=10, h=5, r=108, ps=14)
    
    ymax    <- max(plot_data$p95, na.rm=TRUE)*1.05
    ymin    <- min(plot_data$Min, na.rm=TRUE)*.95
    
    cat("variable = ", VAR, " ymax = ", ymax, " ymin = ", ymin, "\n")
    
    plot(plot_data$TN, plot_data$p95, type='p', pch=15, col=my_red, 
         #axes=FALSE,
         xaxt='n',
         ylim=c(min(plot_data$Min)*.95,max(plot_data$p95)*1.05),
         ylab=YLAB,
         xlab="Database Size",
         main=paste(SRVR, "  ", VAR, sep=''))
    points(plot_data$TN, plot_data$Median, pch=18, col=my_blue)
    lines(plot_data$TN, plot_data$Median, col=my_blue)
    points(plot_data$TN, plot_data$Min, pch=15, col=my_green)
    
    for (s in 1:6) {
      segments(s, plot_data$Min[s], s, plot_data$p95[s],  col=my_green)
      text(s, round(plot_data$Median[s],1), labels=round(plot_data$Median[s],1), col='black', pos=3, cex=.8)
    }
    
    axis(1, at = seq(1:6), labels =c("BASELINE","3_2MM","3_7MM","4_2MM","4_7MM","5_2MM"))
    
    #axis(2, at = seq(from=0, to=max(plot_data$p95), by=5))
    #axis(2)
    
    legend("topleft", c("p95","Median", "Min"), pch=c(15,18,15), 
           col=c(my_red,my_blue,my_green),
           title="Legend", inset=.05,
           cex=.85)
    
    dev.off()
  })
})
dev.off()



##----------------------------------------------------------
## PLOT - Resources
##----------------------------------------------------------

pp_ggplot2 <- ggplot2like()
pp_ggplot2$strip.border      <- list(col="grey60")
pp_ggplot2$axis.line         <- list(col="grey60")
pp_ggplot2$axis.text         <- list(col="black")

output <- "png"
webservers <- c("wwwasc231","wwwasc232","wwwasc233","wwwasc234","wwwasc235")
ss <- subset(mSummSumm, server %in% webservers)
ss <- subset(ss, variable %in% c("cpuUtil", "diskWrSec", "TotSntKB", "TotRcvKB", "diskAvgQLen", "memAvailMB"))
ss$TEST_NAME <- as.factor(ss$TEST_NAME)


str(ss)

byVar <- tapply(ss$value, ss$variable, my.func)  
byVar <- do.call('rbind', byVar)
byVar

topD <- d_ply(ss, .(variable), function(df) {
  
  VAR <- df$variable[1]
  openGdevice(type=output, file=paste(graphDir, 'WEBSERVERS_SERVER_', VAR, sep=""), w=10, h=5, r=108, ps=14)
  
  my_key <- simpleKey(text=c("p95","Median","Min"),
                      space = "top", 
                      columns=3,
                      col=c('red','blue','green'),
                      pch=15,
                      points = FALSE,
                      rectangles = FALSE,
                      lines = FALSE,
                      border = TRUE,
                      transparent = FALSE,
                      #background = 'grey90',
                      #title = "CPU Utilization",
                      cex.title = 1,
                      lty = 4,
                      cex=1)
  
  print(xyplot(p95 + Median + Min ~ TEST_NAME | server, data=df, 
               type=c('l','l','l'), 
               col=c('red','blue','green'),
               layout=c(1,5),
               as.table=TRUE,
               strip=FALSE,
               strip.left = TRUE,
               par.strip.text = list(cex = .6),
               #par.settings=pp_ggplot2,
               main=paste("WEB SERVERS: ", VAR),
               cex.main=3,
               #							sub=paste(startTime, " to ", endTime),
               xlab="Database Size",
               ylab="Value\n**Observe scale limits**",
               axis = axis.grid,
               key = my_key,
               scales=list(y=list(rot=0, cex=.55, alternating=3),
                           x = list(rot = 0, tick.number=24, cex=.65))
  )
  )
  
  dev.off()
  
})
dev.off()












