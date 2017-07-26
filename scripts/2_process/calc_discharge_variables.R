# This script calculates antecedent discharge relative to storm starts
# for EOF work

source('scripts/2_process/fxn_calc_antdischarge.R')

# read in storm start/end times
storms <- read.csv('data_cached/rain_variables.csv', colClasses = c(StartDate = 'POSIXct', EndDate = 'POSIXct'))
storms <- storms[,c('site', 'stormnum', 'StartDate')]

# set discharge directory and files
discharge.dir <- 'H:/Projects/GLRIeof/data_raw'
files <- list.files(discharge.dir)
discharge.files <- grep('discharge', files, value = TRUE, ignore.case = TRUE)

# run antecedent discharge function
ant.discharge <- ant.discharge.bysite(discharge.dir = discharge.dir, discharge.files = discharge.files, 
                                      siteid = c(5601, 5001), sitename = c('SW1', 'SW3'), antecedentDays = c(1,2,7,14), 
                                      storms = storms, start.col = 'StartDate', stats = c('mean', 'max'))

# rename columns
names(ant.discharge) <- c(names(ant.discharge)[1:2], c('ant_discharge_date', 'ant_dis_1day_mean', 'ant_dis_1day_max',
                                                       'ant_dis_2day_mean', 'ant_dis_2day_max',
                                                       'ant_dis_7day_mean', 'ant_dis_7day_max',
                                                       'ant_dis_14day_mean', 'ant_dis_14day_max'))

# write antecedent discharge data
write.csv(ant.discharge, 'data_cached/discharge_variables.csv', row.names = FALSE)

 


