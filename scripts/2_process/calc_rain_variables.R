library(Rainmaker)
library("dataRetrieval")
library(lubridate)

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


run.rain <- function(files, ) {

#read in raw precip data
precip_raw <- read.csv(file = paste('data_raw/', precip.sw1,sep = ""), header = TRUE, skip = 14)

# prep data for other RM functions
precip_prep <- RMprep(precip_raw, prep.type = 3, date.type = 2, dates.in = 'Timestamp..UTC.06.00.', 
                      dates.out = 'pdate', tz = 'Etc/GMT+6', cnames.in = names(precip_raw), cnames.new = c('timestamp_utc', 'timestamp_utc-6', 'rain', 'approval', 'grade', 'qualifiers'))
rain <- 'rain'
precip_prep <- precip_prep[precip_prep[rain] > 0.00001,]
ieHr <- 2 
rainthresh <- 0.008
rain <- "rain"
time <- "pdate"

# get rain events
precip_events <- RMevents_sko(df=precip_prep, ieHr=ieHr, rainthresh=rainthresh, rain=rain, time=time)
precip_event_list <- precip_events$storms2

,skip = 3,col.names = c("site.dd", "YEAR", "MONTH", "DAY", "MINUTE", "rain"))
  

}

  