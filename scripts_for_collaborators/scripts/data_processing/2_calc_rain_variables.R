
# read in WQ data to define storm start/end times
wq.dat <- read.csv(file.path('data_cached', paste0(site, '_prepped_WQbystorm.csv')), 
                   header = TRUE, stringsAsFactors = F, colClasses = c(storm_start = 'POSIXct', storm_end = 'POSIXct'))

############## Get rain data ###############
# get data from NWIS if file is not provided
if (is.na(rain_file)) {
  
  # set readNWISuv parameters
  parameterCd <- "00045"  # Precipitation
  startDate <- as.Date(start_date) - 7 # put a week buffer on the study start date in case storm started prior to first sample date
  endDate <- as.Date(end_date)
  
  # get NWIS data
  message('Pulling precip data from NWIS.')
  start_time <- Sys.time()
  precip_raw <- readNWISuv(rain_site, parameterCd, tz = site_tz)
  end_time <- Sys.time()
  if (nrow(precip_raw) > 0){
    message(paste0(nrow(precip_raw)), ' rows of data pulled from NWIS in ', round(difftime(end_time , start_time, units = 'secs'), 0), ' seconds.')
  } else {
    stop('No precip data pulled from NWIS. Please check inputs to verify correct site number and start and end dates. To debug, see code in "data_processing/2_calc_rain_variables.R"')
  }
  
  # rename columns
  precip_raw <- renameNWISColumns(precip_raw)
  
  # rename columns
  names(precip_raw)[grep('precip_inst$', names(precip_raw), ignore.case = TRUE)] <- 'rain'
  names(precip_raw)[grep('dateTime', names(precip_raw), ignore.case = TRUE)] <- 'pdate'
  
  # print warning if dates of rain do not span dates of study
  if (min(as.Date(precip_raw$pdate)) > start_date) {
    warning('Data pulled from NWIS does not span the entire study period.')
  }
  
  if (max(as.Date(precip_raw$pdate)) < end_date) {
    warning('Data pulled from NWIS does not span the entire study period.')
  }
  
  } else {
    
  # read in precip file
  precip_raw <- read.csv(file.path('data_raw', rain_file), stringsAsFactors = FALSE, strip.white = TRUE)
  precip_raw[,'datetime'] <- as.POSIXct(precip_raw[,'datetime'], tz = site_tz)
  
  names(precip_raw) <- c('pdate', 'rain')
}

############## Process rain data ###########
# summarize precip data using Rainmaker
# run.rainmaker is a wrapper function for multiple
# Rainmaker steps/functions
precip.dat <- run.rainmaker(precip_raw = precip_raw, ieHr = 2, rainthresh = 0.008, wq.dat = wq.dat,
                            xmin = c(5,10,15,30,60), antecedentDays = c(1,2,7,14))

precip.dat <- rename(precip.dat, 'rain_startdate' = 'StartDate', 'rain_enddate' = 'EndDate')

precip_filename <- file.path('data_cached', paste0(site, '_rain_variables.csv'))
write.csv(precip.dat, precip_filename, row.names = FALSE)

# check if the precip.dat data frame has values
test <- precip.dat[!is.na(precip.dat$rain), ]

if (nrow(test)>0) {
  message(paste("The precipitation data has been processed. Please check", precip_filename, "to ensure correct processing."))
} else {
  stop("Something went wrong with processing the precipitation data. To debug, see code in 'scripts/data_processing/2_calc_rain_variables.R'.")
}
  