library(Rainmaker)
library(lubridate)
library(dplyr)
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
files <- list.files('data_raw')
precip.files <- grep('precip', files, value = TRUE, ignore.case = TRUE)

# set working directory to location of precip data
precip.wd <- 'H:/Projects/GLRIeof/data_raw'

# summarize precip data for each site
precip.dat <- run.rainmaker(precip.dir = precip.wd, precip.files = precip.files, siteid = c(5601, 5001),
                            sitename = c("SW1", "SW3"), ieHr = 2, rainthresh = 0.008, wq.dat = wq.dat,
                            xmin = c(5,10,15,30,60), antecedentDays = c(1,2,7,14))

write.csv(precip.dat, 'data_cached/rain_variables.csv', row.names = FALSE)

  