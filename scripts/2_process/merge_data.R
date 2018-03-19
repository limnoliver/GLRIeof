# script to merge wq, rain, and discharge data
library(dplyr)

wq <- read.csv('data_cached/prepped_WQbystorm.csv')
rain <- read.csv('data_cached/rain_variables.csv')
discharge <- read.csv('data_cached/discharge_variables.csv')
weather <- read.csv('data_cached/weather_by_storm.csv')
field <- read.csv('data_cached/field_predictors.csv')
  
# subset and rename columns to reduce duplicate cols
rain <- rename(rain, 'rain_startdate' = 'StartDate', 'rain_enddate' = 'EndDate')
rain <- select(rain, -stormnum, -site)

discharge <- select(discharge, unique_storm_number, ant_dis_1day_max:ant_dis_14day_max)

field <- field[,c(1,4:7)]
# merge dat

all.eof <- merge(wq, rain, by = 'unique_storm_id', all.x = TRUE)
all.eof <- merge(all.eof, discharge, by = 'unique_storm_id', all.x = TRUE)
all.eof <- merge(all.eof, weather, by = 'unique_storm_id', all.x = TRUE)
all.eof <- merge(all.eof, field, by = "unique_storm_id", all.x = TRUE)
# write dat

write.csv(all.eof, 'data_cached/merged_dat.csv', row.names = FALSE)
