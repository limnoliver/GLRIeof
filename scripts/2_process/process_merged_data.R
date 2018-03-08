# source functions
# source('scripts/3_analyze/fxn_vif.R')

# set some site-specific parameters
# these have not been implemented below but should give an idea of how to 
# optomize the script for other sites
site_keep <- "SW1"

# set the dates for your site that will define your 
# before and after period. This allows for a "transition period" to also 
# be define. This should be able to be dropped or included as a seperate designation
before_date <- asPOSIXct('')
after_date <- asPOSIXct('')

# if before and after data aren't equal - a "transition date" will be calculated

# are there any predictors that should not be in this site's model?
# e.g., in the case of SW1, some field activities should not be in the model 
# such as day since manure application - this was changed by the BMP so should 
# not go into the model.
predictors.drop <- c('days_since_planting', 'days_since_fertilizer')

# read in merged data
eof <- read.csv('data_cached/merged_dat.csv', header = TRUE, stringsAsFactors = FALSE,
                colClasses = c(sample_start = 'POSIXct', storm_start = 'POSIXct'))

# create a water equivalent var that sums rain and 
# water equivalent of snow, estimated as 1:10
eof$weq <- ifelse(eof$snwd_diff > 0, eof$rain, eof$rain + (abs(eof$snwd_diff)/10))

# set responses and predictors

# start with all predictors - 30 in total
predictors <- names(select(eof, rain:ARFdays14, ant_dis_1day_max:tmin, days_since_planting,
                     days_since_fertilizer,weq))
predictors <- predictors[-which(predictors %in% predictors.drop)]
                    
# set responses and set cleaner name to plot for responses
responses <- names(select(eof, Suspended_Sediment_mg_L:Organic_Nitrogen_Load_pounds))

# make cleaner response names for plotting reasons
responses_clean <- c('SS (mg/L)', 'Cl (mg/L)', 'NO2 + NO3 (mg/L)','NH4 (mg/L)','TKN (mg/L)',
                     'DRP (mg/L)','TP (mg/L)','TN (mg/L)','Org N (mg/L)',
                     'SS (pounds)', 'Cl (pounds)', 'NO2 + NO3 (pounds)','NH4 (pounds)','TKN (pounds)',
                     'DRP (pounds)','TP (pounds)','TN (pounds)','Org N (pounds)')

########################
# site-specific decisions
# this is SW1-specific

# some of this will get simpler with frozen column, comments, etc

# reduce data to the site of interest, non-frozen periods
# also add before/transition/after BMP

sw1 <- eof %>%
  mutate(frozen = as.logical(substr(eof$frozen, 1, 1))) %>%
  filter(site == 'SW1') %>%
  #filter(frozen == FALSE) %>%
  mutate(period = ifelse(storm_start >= as.POSIXct('2015-06-01 00:00:01'), 'after', 'before'))

sw1$period_crop <- factor(sw1$period_crop)
sw1$period_crop <- ordered(sw1$period_crop, levels = c('before', 'after (corn)', 'after (alfalfa)'))


sw1$crop <- sw1$period_crop
sw1$crop <- ifelse(sw1$crop == "after (alfalfa)", "alfalfa", "corn")
sw1$crop <- as.factor(sw1$crop)


## create a column that marks suspect splits
sw1$suspect_split <- grepl('split', sw1$comment)

## get rid of events with suspect splits
sw1 <- filter(sw1, suspect_split == FALSE)

# find transition period, and drop
# change level order for before and after
sw1$period[sw1$storm_start > as.POSIXct('2015-05-10 00:00:01')& sw1$storm_start < as.POSIXct('2015-06-01 00:00:01')] <- 'transition'
sw1 <- filter(sw1, period != 'transition')
sw1$period <- factor(sw1$period)
sw1$period <- ordered(sw1$period, levels = c('before', 'after'))


