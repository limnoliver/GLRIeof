# script to merge wq, rain, and discharge data
library(dplyr)
site <- 'sw3'
temp_file <- paste0('data_cached/', site)

wq <- read.csv(paste0(temp_file, '_prepped_WQbystorm.csv'))
rain <- read.csv(paste0(temp_file, '_rain_variables.csv'))
discharge <- read.csv(paste0(temp_file, '_discharge_variables.csv'))
weather <- read.csv(paste0(temp_file, '_weather_by_storm.csv'))
field <- read.csv(paste0(temp_file, '_field_predictors.csv'))
  
# subset and rename columns to reduce duplicate cols
rain <- rename(rain, 'rain_startdate' = 'StartDate', 'rain_enddate' = 'EndDate')
rain <- select(rain, -stormnum, -site)

discharge <- select(discharge, unique_storm_number, ant_dis_1day_max:ant_dis_14day_max)

field <- field[,c(1,4:7)]
# merge dat

all.eof <- merge(wq, rain, by = 'unique_storm_number', all.x = TRUE)
all.eof <- merge(all.eof, discharge, by = 'unique_storm_number', all.x = TRUE)
all.eof <- merge(all.eof, weather, by = 'unique_storm_number', all.x = TRUE)
all.eof <- merge(all.eof, field, by = "unique_storm_number", all.x = TRUE)

tempfile_name <- file.path('data_cached', paste0(site, '_merged_dat.csv'))
write.csv(all.eof, tempfile_name, row.names = FALSE)
