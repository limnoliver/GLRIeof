# calculate days since field action variables
# e.g., days since planing corn, alfalfa
# days since last fertilizer/manure application
# use the field activity sheet and storm start dates to calculate
storm_filename <- file.path('data_cached', paste0(site, '_prepped_WQbystorm.csv'))
storms <- read.csv(storm_filename, stringsAsFactors = FALSE,
                   colClasses = c(storm_start = 'POSIXct', storm_end = 'POSIXct'))

timeline_filename <- file.path('data_raw', activity_file)
timeline <- read.csv(timeline_filename,
                     stringsAsFactors = FALSE, strip.white = TRUE)
timeline$date <- as.Date(timeline$date, format = date_format)

# days since planting
# for corn, after cutting, value assumes zero
# for alfalfa, resets after cutting but goes up until next cutting

field_events <- arrange(storms, storm_start) %>%
  select(unique_storm_number, storm_start)

# calculate days since manure/fertilizer
# calculate days since planting, set back to zero after
# cutting
field_events <- field_events %>%
  mutate(days_since_planting = NA) %>%
  mutate(days_since_fertilizer = NA) %>%
  mutate(days_since_cultivation = NA)

for (i in 1:nrow(field_events)) {
  
  temp_date <- as.Date(format(field_events$storm_start[i], "%Y-%m-%d"))
  
  #fert/manure
  man_fert_filter <- grep(paste0(nut_additions_keywords, collapse = '|'), timeline$activity_group, ignore.case = T)
  temp_timeline <- timeline[man_fert_filter, ]
  temp_timeline <- filter(temp_timeline, date <= temp_date)
  
  fert_diff <- temp_date - temp_timeline$date
  field_events$days_since_fertilizer[i] <- as.numeric(min(fert_diff))
  
  # cultivation
  cultivation_filter <- grep(paste0(cultivation_keywords, collapse = '|'), timeline$activity_group, ignore.case = T)
  temp_timeline <- timeline[cultivation_filter, ]
  temp_cultivation <- filter(temp_timeline, date <= temp_date)
  
  cultivation_diff <- temp_date - temp_cultivation$date
  field_events$days_since_cultivation[i] <- as.numeric(min(cultivation_diff))
  
  #plantings & harvest
  temp_timeline <- filter(timeline, date <= temp_date)
  planting_filter <- grep(paste0(planting_keywords, collapse = '|'), temp_timeline$activity_group, ignore.case = T)
  harvest_filter <- grep(paste0(harvest_keywords, collapse = '|'), temp_timeline$activity_group, ignore.case = T)
  temp_plantings <- temp_timeline[planting_filter, ]
  temp_harvest <- temp_timeline[harvest_filter, ]
  
  # now decide which planting/harvest date to use. 
  # e.g., if a cutting happened between now and planting, use days since cutting
  temp_diff <- c((temp_date - temp_plantings$date), (temp_date - temp_harvest$date))
  
  field_events$days_since_planting[i] <- as.numeric(min(temp_diff))
}

field_events <- select(field_events, -storm_start)

temp_filename <- file.path('data_cached', paste0(site, '_field_predictors.csv'))
write.csv(field_events, temp_filename, row.names = FALSE)

if(nrow(field_events) == nrow(storms) & nrow(field_events) > 0) {
  message(paste("Field events processing is complete. Please see", temp_filename, "to ensure correct processing."))
} else {
  stop("Something went wrong with processing the field events data. To debug, see code in 'scripts/data_processing/2_calc_field_predictors.R'")
}
