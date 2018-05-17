storms <- read.csv(file.path('data_cached', paste0(site, '_prepped_WQbystorm.csv')), 
                   stringsAsFactors = FALSE,
                   colClasses = c(storm_start = 'POSIXct', storm_end = 'POSIXct'))

if (nchar(Sys.getenv("noaakey")) == 0) {
  stop('NOAA key not found. Please see data processing SOP for instructions to set up key.')
}
options("noaakey" = Sys.getenv("noaakey"))

if (!is.na(noaa_site)) {
  temp_start <- as.character(as.Date(start_date)-7)
  temp_end = as.character(as.Date(end_date))
  message('Pulling weather data from NOAA.')
  s_time <- Sys.time()
  weather_noaa <- rnoaa::meteo_pull_monitors(noaa_site, date_min = temp_start, 
                                        date_max = temp_end, var = 'all')
  e_time <- Sys.time()
  d_time <- round(difftime(e_time, s_time, units = 'secs'), 0)
  if (nrow(weather_noww) > 0){
    message(paste0(nrow(weather_noaa), ' rows of data pulled from NOAA in ', d_time, ' seconds.'))
  } else {
    stop("Weather data pull from NOAA failed. To debug, see file 'data_processing/2_calc_weather_variables.R'.")
  }
} 

if (!is.na(weather_file)) {
  weather_dat <- read.csv(file.path('data_raw', weather_file))
  weather_dat$date <- as.Date(weather_dat$date, format = date_format)
}

# calculate change in snow depth
# check if snow var is in weather_dat
if (!is.na(weather_file) & 'snwd' %in% names(weather_file)) {
  snow_in_file <- TRUE
} else {
  snow_in_file <- FALSE
}

if (snow_in_file == FALSE) {
  snowpack_diff <- diff(weather_noaa$snwd)
  weather_noaa$snwd_diff[2:nrow(weather_noaa)] <- snowpack_diff
} else {
  snowpack_diff <- diff(weather_dat$snwd)
  weather_dat$snwd_diff[2:nrow(weather_dat)] <- snowpack_diff
}

if (!is.na(weather_file) & !is.na(noaa_site)) {
  weather <- left_join(weather_dat, weather_noaa, by = 'date')
} else if (is.na(weather_file)) {
  weather <- weather_noaa
} else {
  weather <- weather_dat
}

# calculate time vars for storms df
storm_dates <- as.Date(storms$storm_start)
total_days <- as.numeric(difftime(max(storm_dates), min(storm_dates)))
storm_dates_since <- as.numeric(difftime(storm_dates, as.Date("2012-01-01"), unit = 'days'))
b <- (2*pi)/365 # gets correct period for sin cos waves

storms$sin_sdate <- sin(b*storm_dates_since)
storms$cos_sdate <- cos(b*storm_dates_since)

for (i in 1:nrow(storms)) {
  dates <- as.Date(c(format(storms$storm_start[i], format = "%Y-%m-%d"), format(storms$storm_end[i], format = "%Y-%m-%d")))
  dates <- seq(dates[1], dates[2], by = 'days')
  dates <- unique(dates)
  weather_temp <- filter(weather, date %in% dates)
  storms$tmax[i] <- max(weather_temp$tmax)/10 #convert from tenths of degrees C to degrees C
  storms$tmin[i] <- min(weather_temp$tmin)/10 #convert from tenths of degrees C to degrees C
  #storms$prcp[i] <- sum(weather_temp$prcp)*(0.0393701/10) # convert from tenths of mm to inches
  #storms$snow[i] <- sum(weather_temp$snow)*0.0393701 # convert from mm to inches
  #storms$snwd[i] <- mean(weather_temp$snwd)*0.0393701 # convert from mm to inches
  storms$snwd_diff[i] <- sum(weather_temp$snwd_diff)*0.0393701 # convert from mm to inches
  
  if (!is.na(other_weather_vars)) {
    for (j in 1:length(other_weather_vars)) {
      # default behavior is to take the mean of the "other_weather_vars if the storm spans multiple days
      # but could change to min, max, sum depending on what type of variable it is.
      storms[i, other_weather_vars[j]] <- mean(weather_temp[, other_weather_vars[j]])
    }
  }
  
  
  
}

weather.dat <- select(storms, unique_storm_number, sin_sdate:snwd_diff)

weather_tempname <- file.path('data_cached', paste0(site, '_weather_by_storm.csv'))
write.csv(weather.dat, weather_tempname, row.names = F)

test <- weather.dat[!is.na(weather.dat$tmax), ]

if (nrow(test) > 0) {
  message(paste("Weather data has been processed. Please see", weather_tempname, "to ensure correct processing."))
} else {
  stop('Something went wrong while processing weather data. To debug, see code in "scripts/data_processing/2_calc_weather_variables.R"')
}
