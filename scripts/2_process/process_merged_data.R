# source functions
# source('scripts/3_analyze/fxn_vif.R')

# site of interest
site <- "SW1"
# read in merged data
eof <- read.csv('data_cached/merged_dat.csv', header = TRUE, stringsAsFactors = FALSE,
                colClasses = c(sample_start = 'POSIXct'))

# create a water equivalent var that sums rain and 
# water equivalent of snow, estimated as 1:10
eof$weq <- ifelse(eof$snwd_diff > 0, eof$rain, eof$rain + (abs(eof$snwd_diff)/10))

# set responses and predictors

# start with all predictors - 30 in total
predictors <- names(select(eof, rain:ARFdays14, ant_dis_1day_mean:tmin, days_since_planting,
                     days_since_fertilizer,weq))
                    
# set responses and set cleaner name to plot for responses
responses <- names(select(eof, Suspended_Sediment_mg_L:Organic_Nitrogen_Load_pounds))

# make cleaner response names for plotting reasons
responses_clean <- c('SS (mg/L)', 'Cl (mg/L)', 'NO2 + NO3 (mg/L)','NH4 (mg/L)','TKN (mg/L)',
                     'DRP (mg/L)','TP (mg/L)','TN (mg/L)','Org N (mg/L)',
                     'SS (pounds)', 'Cl (pounds)', 'NO2 + NO3 (pounds)','NH4 (pounds)','TKN (pounds)',
                     'DRP (pounds)','TP (pounds)','TN (pounds)','Org N (pounds)')


# reduce data to the site of interest, non-frozen periods
# also add before/transition/after BMP

sw1 <- eof %>%
  mutate(frozen = as.logical(substr(eof$frozen, 1, 1))) %>%
  filter(site == 'SW1') %>%
  filter(frozen == FALSE) %>%
  mutate(period = ifelse(storm_start >= as.POSIXct('2015-06-01 00:00:01'), 'after', 'before'))

sw1_all <- eof %>% 
  mutate(frozen = as.logical(substr(eof$frozen, 1, 1))) %>%
  filter(site == 'SW1') %>%
  mutate(period = ifelse(storm_start >= as.POSIXct('2015-06-01 00:00:01'), 'after', 'before'))

# remove highly correlated predictors
predictors.cor <- cor(eof[,predictors[-31]], use = 'complete.obs') # drop var "crop" from correlation since it's a categorical var
names.cor <- row.names(predictors.cor)
drop.predictors <- findCorrelation(predictors.cor, cutoff = 0.95, verbose = FALSE, exact = TRUE)


sw1$period[sw1$storm_start > as.POSIXct('2015-05-10 00:00:01')& sw1$storm_start < as.POSIXct('2015-06-01 00:00:01')] <- 'transition'

sw1 <- filter(sw1, period != 'transition')

# transform variables that need it - was determined with simple histograms
# look at histograms
pred.keep <- names.cor[-drop.predictors]
pred.keep.df <- reshape2::melt(sw1[,pred.keep])
ggplot(pred.keep.df, aes(x = value)) +
  geom_histogram() +
  facet_wrap(~variable, scales = 'free')
# vars to transform
# removed ARFdays from transformation even though they were lognormally distributed
# they have true zeros, so will just keep them unstransformed
# also do not transform ant_dis_2day_max and erosivity_m1
# vars.transform <- c('peak_discharge', 'Chloride_mg_L', 'sum_runoff', 'Ievent', 'I5', 'erosivity_m1', 'ARFdays1', 'ARFdays2',
#                     'ARFdays7', 'ARFdays14', 'ant_dis_2day_max', 'ant_dis_7day_mean', 'ant_dis_7day_max', 'ant_dis_14day_mean',
#                     'ant_dis_14day_max')
vars.transform <- c('peak_discharge', 'Chloride_mg_L', 'Ievent', 'I5', 'ant_dis_7day_mean', 'ant_dis_7day_max', 'ant_dis_14day_mean',
                    'ant_dis_14day_max')

# add 0.001 to antecedent rainfall 0s so that I can log transform
hist(log10(sw1$ARFdays14 + 0.01), breaks = 30)
length(which(sw1$ant_dis_2day_max == 0))



# get rid of zeros from database - this should be fixed at some point
# should not be zeros for erosivity, etc, but there is an issue with getting sub events to count as a single event
#################Fix this after subevents are fixed, just get rid of next line

#sw1 <- filter(sw1, erosivity_m1 > 0)
# need to fix storms 60 and 79 from SW1

# transform vars
sw1[,vars.transform] <- log10(sw1[,vars.transform])

# for vars that just have a couple of variables nad could benefit from 
# log transformation, will add 0.5 of minimum value tp log transformation
# this includes 7 and 4 day antecedent rainfall and erosivity

hist(log10(sw1$ARFdays14 + (0.5*min(sw1$ARFdays14[sw1$ARFdays14 > 0]))))
sw1$erosivity_m1 <- log10(sw1$erosivity_m1 + (0.5*min(sw1$erosivity_m1[sw1$erosivity_m1 > 0])))
sw1$ARFdays7 <- log10(sw1$ARFdays7 + (0.5*min(sw1$ARFdays7[sw1$ARFdays7 > 0])))
sw1$ARFdays14 <- log10(sw1$ARFdays14 + (0.5*min(sw1$ARFdays14[sw1$ARFdays14 > 0])))



# log transform responses
sw1[,responses] <- log10(sw1[,responses])

########################
# site-specific decisions
# this is SW1-specific
sw1$period_crop <- factor(sw1$period_crop)
sw1$period_crop <- ordered(sw1$period_crop, levels = c('before', 'after (corn)', 'after (alfalfa)'))


sw1$crop <- sw1$period_crop
sw1$crop <- ifelse(sw1$crop == "after (alfalfa)", "alfalfa", "corn")
sw1$crop <- as.factor(sw1$crop)


sw1$period <- factor(sw1$period)
sw1$period <- ordered(sw1$period, levels = c('before', 'after'))
## create a column that marks suspect splits
sw1$suspect_split <- grepl('split', sw1$comment)

## get rid of events with suspect splits
sw1 <- filter(sw1, suspect_split == FALSE)

