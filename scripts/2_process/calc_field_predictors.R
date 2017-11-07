# calculate days since field action variables
# e.g., days since planing corn, alfalfa
# days since last fertilizer/manure application
# use the field activity sheet and storm start dates to calculate
storms <- read.csv('data_cached/prepped_WQbystorm.csv', stringsAsFactors = FALSE,
                   colClasses = c(storm_start = 'POSIXct', storm_end = 'POSIXct'))
timeline <- read.csv('data_raw/SW1_field_activity.csv', 
                     stringsAsFactors = FALSE, strip.white = TRUE)
timeline$date <- as.Date(timeline$date, format = '%m/%d/%Y')

# days since planting
# for corn, after cutting, value assumes zero
# for alfalfa, resets after cutting but goes up until next cutting

field_events <- storms %>%
  mutate(period = ifelse(storm_start >= as.POSIXct('2015-06-01 00:00:01'), 'after', 'before'))
  

field_events$period[field_events$storm_start > as.POSIXct('2015-05-10 00:00:01')& field_events$storm_start < as.POSIXct('2015-06-01 00:00:01')] <- 'transition'

# create another period that includes distinguising between
# after - corn and after - alfalfa
field_events$period_crop <- field_events$period
field_events$period_crop[field_events$period_crop == 'after'] <- "after (corn)"
field_events$period_crop[field_events$storm_start >= as.POSIXct('2016-05-04 00:00:01')] <- "after (alfalfa)"

field_events <- arrange(field_events, storm_start) %>%
  select(unique_storm_id, storm_start, period, period_crop)

# calculate days since manure/fertilizer
# calculate days since planting, set back to zero after
# cutting
field_events <- field_events %>%
  mutate(days_since_planting = NA) %>%
  mutate(days_since_fertilizer = NA)

for (i in 2:nrow(field_events)) {
  
  temp.date <- as.Date(format(field_events$storm_start[i], "%Y-%m-%d"))
  
  if (field_events$period_crop[i] == "before"|field_events$period_crop[i] == "after (corn)") {
    temp_timeline <- filter(timeline, current_crop == "corn") %>%
      filter(date <= temp.date)
    
    temp_plantings <- temp_timeline[grep('planting', temp_timeline$activity_group), ]
    temp_harvest <- temp_timeline[grep('harvest', temp_timeline$activity_group), ]
    
    if (nrow(temp_plantings) > 0) {
      planting_diff <- temp.date - temp_plantings$date
      field_events$days_since_planting[i] <- as.numeric(min(planting_diff))
      
      harvest_diff <- 
    }
    
  } 
  plantings <- timeline[grep('planting')]
  days_since_planting <- diffs[diffs <= 0 & timeline]
  
}