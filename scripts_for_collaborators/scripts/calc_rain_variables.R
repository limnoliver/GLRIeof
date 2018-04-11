library(Rainmaker)
library(lubridate)
library(dplyr)
library(dataRetrieval)

# read in WQ data to define storm start/end times
wq.dat <- read.csv(wq_filename, header = TRUE)

############## Get rain data ###############
# get data from NWIS if file is not provided
if (is.na(rain_file)) {
  
  # set readNWISuv parameters
  parameterCd <- "00045"  # Precipitation
  startDate <- as.Date(start_date) - 7 # put a week buffer on the study start date in case storm started prior to first sample date
  endDate <- as.Date(end_date)
  
  # get NWIS data
  precip_raw <- readNWISuv(temp_sites[i], parameterCd, tz = site_tz)
  
  # rename columns
  precip_raw <- renameNWISColumns(precip_raw)
  
  # rename columns
  names(precip_raw)[grep('precip_inst$', names(precip_raw), ignore.case = TRUE)] <- 'rain'
  names(precip_raw)[grep('dateTime', names(precip_raw), ignore.case = TRUE)] <- 'pdate'
  
  } else {
    
  # read in precip file
  precip_raw <- read.csv(file.path('data_raw', rain_file), stringsAsFactors = FALSE, strip.white = TRUE)
  precip_raw[,date_column] <- as.POSIXct(precip_raw[,date_column], tz = site_tz)
  
  names(precip_raw)[which(names(precip_raw) %in% rain_column)] <- 'rain'
  names(precip_raw)[which(names(precip_raw) %in% date_column)] <- 'pdate'
  
}

############## Process rain data ###########
# summarize precip data using Rainmaker
# run.rainmaker is a wrapper function for multiple
# Rainmaker steps/functions
precip.dat <- run.rainmaker(precip_raw = precip_raw, siteid = site_no,
                            sitename = site, ieHr = 2, rainthresh = 0.008, wq.dat = wq.dat,
                            xmin = c(5,10,15,30,60), antecedentDays = c(1,2,7,14))

precip.dat <- rename(precip.dat, 'rain_startdate' = 'StartDate', 'rain_enddate' = 'EndDate')

precip_filename <- file.path('data_cached', paste0(site, '_rain_variables.csv'))
write.csv(precip.dat, precip_filename, row.names = FALSE)

  