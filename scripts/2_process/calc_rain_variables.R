library(Rainmaker)
library(lubridate)
library(dplyr)
library(dataRetrieval)
source('scripts/2_process/fxn_RMevents_EOF.R')
source('scripts/2_process/fxn_runrainmaker.R')

# read in WQ data to define storm start/end times
wq.dat <- read.csv('data_cached/prepped_WQbystorm.csv', header = TRUE)

# read in dates as CST/CDT, but then convert to GMT
date.vars <- c('sample_start', 'sample_end', 'storm_start', 'storm_end')
for (i in 1:length(date.vars)) {
  temp <- as.POSIXct(as.character(wq.dat[,date.vars[i]]), tz = 'America/Chicago')
  temp <- with_tz(temp, tzone = "Etc/GMT+6")
  wq.dat[,date.vars[i]] <- temp
}

# list precipitation data files
#files <- list.files('data_raw')
#precip.files <- grep('precip', files, value = TRUE, ignore.case = TRUE)

# get precipitation data from NWIS using dataretrieval
siteNumber <- c("441624088045601", "441520088045001")
parameterCd <- "00045"  # Precipitation
#startDate <- "2010-06-30"
#endDate <- "2017-09-18"
precip_raw <- readNWISuv(siteNumber, parameterCd, tz = "Etc/GMT+6")
precip_raw <- renameNWISColumns(precip_raw)

# rename columns
names(precip_raw)[grep('precip_inst$', names(precip_raw), ignore.case = TRUE)] <- 'rain'
names(precip_raw)[grep('dateTime', names(precip_raw), ignore.case = TRUE)] <- 'pdate'

# summarize precip data for each site
precip.dat <- run.rainmaker(precip_raw, siteid = c('441624088045601', '441520088045001'),
                            sitename = c("SW1", "SW3"), ieHr = 2, rainthresh = 0.008, wq.dat = wq.dat,
                            xmin = c(5,10,15,30,60), antecedentDays = c(1,2,7,14))

write.csv(precip.dat, 'data_cached/rain_variables.csv', row.names = FALSE)

  