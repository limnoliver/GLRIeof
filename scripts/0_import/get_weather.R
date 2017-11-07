# script gets weather (snow, temp, etc) from nearby Green Bay NOAA station
library(rnoaa)
# set noaa key, see instructions here about
# setting key for NOAA: https://cran.r-project.org/web/packages/countyweather/README.html
options("noaakey" = Sys.getenv("noaakey"))

gb <- meteo_pull_monitors('USW00014898', date_min = '2011-10-01', date_max = '2017-10-01', var = 'all')

write.csv(gb, 'data_raw/GRB_weather_dat.csv')
