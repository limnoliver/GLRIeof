# script to merge wq, rain, and discharge data

wq <- read.csv('data_cached/prepped_WQbystorm.csv')
rain <- read.csv('data_cached/rain_variables.csv')
discharge <- read.csv('data_cached/discharge_variables.csv')
weather <- read.csv('data_cached/weather_by_storm.csv')
field <- read.csv('data_cached/field_predictors.csv')
  
# subset and rename columns to reduce duplicate cols

names(rain)[1] <- 'unique_storm_id'
rain <- rain[,c(1, c(4:18))]

names(discharge)[2] <- 'unique_storm_id'
discharge <- discharge[,c(2, 4:12)]

weather <- weather[,c(2:9)]

field <- field[,c(1,4:7)]
# merge dat

all.eof <- merge(wq, rain, by = 'unique_storm_id', all.x = TRUE)
all.eof <- merge(all.eof, discharge, by = 'unique_storm_id', all.x = TRUE)
all.eof <- merge(all.eof, weather, by = 'unique_storm_id', all.x = TRUE)
all.eof <- merge(all.eof, field, by = "unique_storm_id", all.x = TRUE)
# write dat

write.csv(all.eof, 'data_cached/merged_dat.csv', row.names = FALSE)
