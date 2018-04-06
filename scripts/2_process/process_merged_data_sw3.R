# set the dates for your site that will define your 
# before and after period. This allows for a "transition period" to also 
# be define. This should be able to be dropped or included as a seperate designation
#before_date <- asPOSIXct('')
#after_date <- asPOSIXct('')

# if before and after data aren't equal - a "transition date" will be calculated

# are there any predictors that should not be in this site's model?
# e.g., in the case of SW1, some field activities should not be in the model 
# such as day since manure application - this was changed by the BMP so should 
# not go into the model.
predictors.drop <- c('days_since_cultivation', 'days_since_planting')

# read in merged data
eof <- read.csv('data_cached/sw3_merged_dat.csv', header = TRUE, stringsAsFactors = FALSE,
                colClasses = c(storm_start = 'POSIXct'))

# create a water equivalent var that sums rain and 
# water equivalent of snow, estimated as 1:10
eof$weq <- ifelse(eof$snwd_diff >= 0, eof$rain, eof$rain + (abs(eof$snwd_diff)/10))

# set responses and predictors

# start with all predictors - 32 in total
predictors <- names(select(eof, duration:tmin, days_since_planting:weq))
predictors <- c(predictors, 'frozen')

# drop site-specific predictors that shouldn't be in mod
predictors <- predictors[-which(predictors %in% predictors.drop)]
                    
# set responses and set cleaner name to plot for responses
responses <- names(select(eof, Suspended_Sediment_mg_L:Organic_Nitrogen_Load_pounds, peak_discharge))

# make cleaner response names for plotting reasons
responses_clean <- c('SS (mg/L)', 'Cl (mg/L)', 'NO2 + NO3 (mg/L)','NH4 (mg/L)','TKN (mg/L)',
                     'DRP (mg/L)','TP (mg/L)','TN (mg/L)','Org N (mg/L)',
                     'SS (pounds)', 'Cl (pounds)', 'NO2 + NO3 (pounds)','NH4 (pounds)','TKN (pounds)',
                     'DRP (pounds)','TP (pounds)','TN (pounds)','Org N (pounds)', "Peak Discharge")

########################
# site-specific decisions
# this is SW1-specific

# some of this will get simpler with frozen column, comments, etc

# reduce data to the site of interest, non-frozen periods
# also add before/transition/after BMP

sw3 <- eof %>%
  mutate(frozen = as.logical(substr(eof$frozen, 1, 1)))


# rain gauge was not online for first two storms, so there is na values for
# those rain metrics - drop these events for now

sw3 <- filter(sw3, !is.na(rain))
dat <- sw3

#write.csv(sw3, 'data_cached/sw3_mod_dat.csv', row.names = F)

