library(Rainmaker)
library(lubridate)
library(dplyr)
source('scripts/2_process/fxn_RMevents_EOF.R')

# read in WQ data to define storm start/end times
wq.dat <- read.csv('data_cached/prepped_WQbystorm.csv', header = TRUE)

date.vars <- c('sample_start', 'sample_end', 'storm_start', 'storm_end')

# read in dates as CST/CDT, but then convert to GMT
for (i in 1:length(date.vars)) {
  temp <- as.POSIXct(as.character(wq.dat[,date.vars[i]]), tz = 'America/Chicago')
  temp <- with_tz(temp, tzone = "Etc/GMT+6")
  wq.dat[,date.vars[i]] <- temp
}

# read in precipitation data
files <- list.files('data_raw')
sw1.id <- 5601
sw3.id <- 5001
precip.files <- grep('precip', files, value = TRUE, ignore.case = TRUE)
precip.sw1 <- grep(sw1.id, precip.files, value = TRUE)
precip.sw3 <- grep(sw3.id, precip.files, value = TRUE)


run.rain <- function(files, ieHr = 2, ) {

# set some universal variables 
ieHr = 2

#read in raw precip data
precip_raw_sw1 <- read.csv(file = paste('data_raw/', precip.sw1, sep = ""), header = TRUE, skip = 14)
precip_raw_sw3 <- read.csv(file = paste('data_raw/', precip.sw3, sep = ""), header = TRUE, skip = 14)

# prep data for other RM functions
precip_prep_sw1 <- RMprep(precip_raw_sw1, prep.type = 3, date.type = 2, dates.in = 'Timestamp..UTC.06.00.', 
                      dates.out = 'pdate', tz = 'Etc/GMT+6', cnames.in = names(precip_raw_sw1), cnames.new = c('timestamp_utc', 'timestamp_utc-6', 'rain', 'approval', 'grade', 'qualifiers'))
precip_prep_sw3 <- RMprep(precip_raw_sw3, prep.type = 3, date.type = 2, dates.in = 'Timestamp..UTC.06.00.', 
                          dates.out = 'pdate', tz = 'Etc/GMT+6', cnames.in = names(precip_raw_sw3), cnames.new = c('timestamp_utc', 'timestamp_utc-6', 'rain', 'approval', 'grade', 'qualifiers'))


# get rain events

events_sw1 <- RMevents_eof(df=precip_prep_sw1, storms = wq.dat, site = 'SW1', ieHr=2, rainthresh=0.008, rain='rain', time='pdate')
events_sw3 <- RMevents_eof(df=precip_prep_sw3, storms = wq.dat, site = 'SW3', ieHr=2, rainthresh=0.008, rain='rain', time='pdate')

# using storms instead of storms 2 incase some get filtered out
events_list_sw1 <- events_sw1$storms
events_list_sw3 <- events_sw3$storms
tipsbystorm <- events_sw1$tipsbystorm
# calculate storm intensity at different time intervals

StormSummary <- RMIntense(df=precip_prep_sw1, date="pdate", rain="rain", df.events=events_list_sw1,
                          sdate="StartDate", edate="EndDate", depth="rain", xmin=c(5,10,15,30,60))

# calculate erosivity 
timeInterval <- 5
StormSummary.1 <- RMerosivity(df = tipsbystorm, ieHr=ieHr, rain='rain', StormSummary=StormSummary, method=1)
erosivity.col <- grep('erosivity', names(StormSummary.1))
names(StormSummary.1)[erosivity.col] <- 'erosivity_m1'

# calculate erosivity using method 2
StormSummary.2 <- RMerosivity(df= tipsbystorm, ieHr=ieHr, rain="rain", StormSummary=StormSummary, method=2)
erosivity.col <- grep('erosivity', names(StormSummary.2))
names(StormSummary.2)[erosivity.col] <- 'erosivity_m2'

# calculate antecedent rain
antecedentDays <- c(1,2,3,7,14)
ARF <- RMarf(df = tipsbystorm, date = 'pdate', rain = 'rain', df.events = StormSummary, 
             sdate = "StartDate", days = antecedentDays, varnameout = "ARFdays")

# merge rain summary data 
arf.cols <- grep('arf', names(ARF), ignore.case = TRUE, value = TRUE)
dat.combine <- list(StormSummary.1, StormSummary.2[,c('stormnum', 'erosivity_m2')], ARF[,c('stormnum', arf.cols)])
merged.rain <- Reduce(function(...) merge(..., all = T), dat.combine)

write.csv(merged.rain, 'data_cached/rain_variables.csv', row.names = FALSE)

wq.rain <- merge(wq.dat, )
  