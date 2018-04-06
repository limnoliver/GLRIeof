# script gets weather (snow, temp, etc) from nearby Green Bay NOAA station
library(rnoaa)
# set noaa key, see instructions here about
# setting key for NOAA: https://cran.r-project.org/web/packages/countyweather/README.html
options("noaakey" = Sys.getenv("noaakey"))

site <- 'sw3'
site_start <- '2014-03-01'
site_end <- '2017-08-31'

gb <- meteo_pull_monitors('USW00014898', date_min = site_start, date_max = site_end, var = 'all')

temp_filename <- file.path('data_raw', paste0(site, '_GRB_weather_dat.csv'))
write.csv(gb, temp_filename)
