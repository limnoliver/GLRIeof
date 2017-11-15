library(lubridate)
library(dplyr)

gb <- read.csv('data_raw/GRB_weather_dat.csv', stringsAsFactors = FALSE, 
               colClasses = c(date = 'Date'))
storms <- read.csv('data_cached/prepped_WQbystorm.csv', stringsAsFactors = FALSE,
                   colClasses = c(storm_start = 'POSIXct', storm_end = 'POSIXct'))
names(gb)
# calculate change in snow depth
snowpack_diff <- diff(gb$snwd)
gb$snwd_diff[2:nrow(gb)] <- snowpack_diff

# calculate time vars for storms df
storms$doy <- yday(storms$sample_start)

for (i in 1:nrow(storms)) {
  dates <- as.Date(unique(c(format(storms$storm_start[i], format = "%Y-%m-%d"), format(storms$storm_end[i], format = "%Y-%m-%d"))))
  weather <- filter(gb, date %in% dates)
  storms$tmax[i] <- max(weather$tmax)/10 #convert from tenths of degrees C to degrees C
  storms$tmin[i] <- min(weather$tmin)/10 #convert from tenths of degrees C to degrees C
  storms$prcp[i] <- sum(weather$prcp)*(0.0393701/10) # convert from tenths of mm to inches
  storms$snow[i] <- sum(weather$snow)*0.0393701 # convert from mm to inches
  storms$snwd[i] <- mean(weather$snwd)*0.0393701 # convert from mm to inches
  storms$snwd_diff[i] <- sum(weather$snwd_diff)*0.0393701 # convert from mm to inches
  
}

weather.dat <- select(storms, unique_storm_id, doy:snwd_diff)

write.csv(weather.dat, 'data_cached/weather_by_storm.csv')
