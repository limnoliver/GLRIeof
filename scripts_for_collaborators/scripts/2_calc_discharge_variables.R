# This script calculates antecedent discharge relative to storm starts
# for EOF work
# read in storm start/end times

wq_files <- file.path('data_cached', paste0(site, '_prepped_WQbystorm.csv'))
wq.dat <- read.csv(wq_files, header = TRUE, colClasses = c(storm_start = 'POSIXct'))

storms <- wq.dat[,c('unique_storm_number', 'storm_start')]

# get discharge
if (is.na(discharge_file)) {
  discharge.dat <- readNWISdv(siteNumbers = discharge_site_no, parameterCd = '00060')
  discharge.dat <- renameNWISColumns(discharge.dat)
} else {
  temp_filepath <- file.path('data_raw', discharge_file)
  discharge.dat <- read.csv(temp_filepath)
  names(discharge.dat)[which(names(discharge.dat) %in% dis_date_column)] <- 'Date'
  names(discharge.dat)[which(names(discharge.dat) %in% discharge_column)] <- 'Flow'
}

stats = c('mean', 'max')

discharge_vars <- TSstats(discharge.dat, date = 'Date', varnames = 'Flow', dates = storms, starttime = "storm_start",
                          times = antecedent_days, units = 'days', stats.return = stats)

# rename columns
names(discharge_vars) <- c(names(discharge_vars)[1:2], c('ant_discharge_date', 'ant_dis_1day_mean', 'ant_dis_1day_max',
                                                       'ant_dis_2day_mean', 'ant_dis_2day_max',
                                                       'ant_dis_3day_mean', 'ant_dis_3day_max',
                                                       'ant_dis_7day_mean', 'ant_dis_7day_max',
                                                       'ant_dis_14day_mean', 'ant_dis_14day_max'))
discharge_vars <- select(discharge_vars, -ant_dis_1day_mean, -storm_start)

# write antecedent discharge data
temp_filename <- file.path('data_cached', paste0(site, '_discharge_variables.csv'))
write.csv(discharge_vars, temp_filename, row.names = FALSE)

 


