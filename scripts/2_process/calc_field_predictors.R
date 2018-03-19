library(dplyr)
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
  select(unique_storm_number, site, storm_start, period, period_crop) %>%
  filter(site == "SW1")

# calculate days since manure/fertilizer
# calculate days since planting, set back to zero after
# cutting
field_events <- field_events %>%
  mutate(days_since_planting = NA) %>%
  mutate(days_since_fertilizer = NA)

for (i in 1:nrow(field_events)) {
  
  temp.date <- as.Date(format(field_events$storm_start[i], "%Y-%m-%d"))
  
  temp_timeline <- filter(timeline, activity_group == "fertilizer application"|activity_group == "planting + fertilizer"|activity_group == "manure application") %>%
    filter(date <= temp.date)
  
  fert_diff <- temp.date - temp_timeline$date
  field_events$days_since_fertilizer[i] <- as.numeric(min(fert_diff))
  
  if (i == 1) {
    field_events$days_since_planting <- 0 
    next
  }
  
  
  
  if (field_events$period_crop[i] == "before"|field_events$period_crop[i] == "after (corn)") {
    temp_timeline <- filter(timeline, current_crop == "corn") %>%
      filter(date <= temp.date)
    
    temp_plantings <- temp_timeline[grep('planting', temp_timeline$activity_group), ]
    temp_harvest <- temp_timeline[grep('harvest', temp_timeline$activity_group), ]
    
    if (nrow(temp_plantings) > 0) {
      planting_diff <- temp.date - temp_plantings$date
      field_events$days_since_planting[i] <- as.numeric(min(planting_diff))
      
      harvest_diff <- temp.date - temp_harvest$date
      harvest_diff <- ifelse(length(harvest_diff) > 0, as.numeric(min(harvest_diff)), NA)
      
      if (!is.na(harvest_diff) & harvest_diff < as.numeric(min(planting_diff))) {
          field_events$days_since_planting[i] <- 0
      }
      
    } else {
      field_events$days_since_planting[i] <- 0 
    }
  } else {
    
    temp_timeline <- filter(timeline, date <= temp.date)
    
    temp_plantings <- temp_timeline[grep('planting', temp_timeline$activity_group), ]
    temp_harvest <- temp_timeline[grep('cutting|harvest', temp_timeline$activity_group), ]
    
    temp_diff <- c((temp.date - temp_plantings$date), (temp.date - temp_harvest$date))
    
    field_events$days_since_planting[i] <- as.numeric(min(temp_diff))
    
  }
}

write.csv(field_events, 'data_cached/field_predictors.csv', row.names = FALSE)
