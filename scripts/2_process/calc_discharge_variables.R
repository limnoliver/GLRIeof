# This script calculates antecedent discharge relative to storm starts
# for EOF work
library(dplyr)
# read in storm start/end times
site <- 'sw3'

wq_files <- file.path('data_cached', paste0(site, '_prepped_WQbystorm.csv'))

wq.dat <- read.csv(wq_files, header = TRUE, colClasses = c(storm_start = 'POSIXct'))
storms <- wq.dat[,c('site', 'unique_storm_number', 'storm_start')]

# get discharge
library(dataRetrieval)
discharge.dat <- readNWISdv(siteNumbers = '04085108', parameterCd = '00060')
discharge.dat <- renameNWISColumns(discharge.dat)

antecedentDays = c(1,2,3,7,14)
stats = c('mean', 'max')
# run antecedent discharge function
library(USGSHydroTools)

discharge_vars <- TSstats(discharge.dat, date = 'Date', varnames = 'Flow', dates = storms, starttime = "storm_start",
                          times = antecedentDays, units = 'days', stats.return = stats)

# rename columns
names(discharge_vars) <- c(names(discharge_vars)[1:3], c('ant_discharge_date', 'ant_dis_1day_mean', 'ant_dis_1day_max',
                                                       'ant_dis_2day_mean', 'ant_dis_2day_max',
                                                       'ant_dis_3day_mean', 'ant_dis_3day_max',
                                                       'ant_dis_7day_mean', 'ant_dis_7day_max',
                                                       'ant_dis_14day_mean', 'ant_dis_14day_max'))
discharge_vars <- select(discharge_vars, -ant_dis_1day_mean)
# write antecedent discharge data
write.csv(discharge_vars, 'data_cached/discharge_variables.csv', row.names = FALSE)

 


