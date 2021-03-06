library(lubridate)
library(dplyr)

# read in site-specific weather data
site <- 'sw3'
weather_filename <- file.path('data_raw', paste0(site, '_GRB_weather_dat.csv'))
storm_filename <- file.path('data_cached', paste0(site, '_prepped_WQbystorm.csv'))

gb <- read.csv(weather_filename, stringsAsFactors = FALSE, 
               colClasses = c(date = 'Date'))
storms <- read.csv(storm_filename, stringsAsFactors = FALSE,
                   colClasses = c(storm_start = 'POSIXct', storm_end = 'POSIXct'))
names(gb)
# calculate change in snow depth
snowpack_diff <- diff(gb$snwd)
gb$snwd_diff[2:nrow(gb)] <- snowpack_diff

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
  weather <- filter(gb, date %in% dates)
  storms$tmax[i] <- max(weather$tmax)/10 #convert from tenths of degrees C to degrees C
  storms$tmin[i] <- min(weather$tmin)/10 #convert from tenths of degrees C to degrees C
  storms$prcp[i] <- sum(weather$prcp)*(0.0393701/10) # convert from tenths of mm to inches
  storms$snow[i] <- sum(weather$snow)*0.0393701 # convert from mm to inches
  storms$snwd[i] <- mean(weather$snwd)*0.0393701 # convert from mm to inches
  storms$snwd_diff[i] <- sum(weather$snwd_diff)*0.0393701 # convert from mm to inches
  
}

weather.dat <- select(storms, unique_storm_number, sin_sdate:snwd_diff)

weather_tempname <- file.path('data_cached', paste0(site, '_weather_by_storm.csv'))
write.csv(weather.dat, weather_tempname, row.names = F)
