# script to merge wq, rain, and discharge data
temp_file <- paste0('data_cached/', site)

wq <- read.csv(paste0(temp_file, '_prepped_WQbystorm.csv'))
rain <- read.csv(paste0(temp_file, '_rain_variables.csv'))
discharge <- read.csv(paste0(temp_file, '_discharge_variables.csv'))
weather <- read.csv(paste0(temp_file, '_weather_by_storm.csv'))
field <- read.csv(paste0(temp_file, '_field_predictors.csv'))

# merge dat

all.eof <- merge(wq, rain, by = 'unique_storm_number', all.x = TRUE)
all.eof <- merge(all.eof, discharge, by = 'unique_storm_number', all.x = TRUE)
all.eof <- merge(all.eof, weather, by = 'unique_storm_number', all.x = TRUE)
all.eof <- merge(all.eof, field, by = "unique_storm_number", all.x = TRUE)

tempfile_name <- file.path('data_cached', paste0(site, '_merged_dat.csv'))
write.csv(all.eof, tempfile_name, row.names = FALSE)

if(nrow(all.eof) == nrow(wq)) {
  message(paste("All storm and predictor data have been merged. See", tempfile_name, "to ensure data were merged properly."))
} else {
  stop("Something went wrong during the data merge process. To debug, see code in 'scripts/data_processing/3_merge_data.R'")
}
