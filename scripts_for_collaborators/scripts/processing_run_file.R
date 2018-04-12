# Run this file to run all data processing/analysis steps

# load libraries
library(dplyr)
library(Rainmaker)
library(dataRetrieval)
library(USGSHydroTools)
library(lubridate)
library(rnoaa)

# source the master file with all site-specific vars
source('scripts/0_master_file.R', echo = F)

# source the water quality file which is the basis for all other processing.
wq_env <- new.env()
source('scripts/1_calc_storm_wq.R', echo = F, local = wq_env)

# source functions that are needed
source('scripts/fxns_data_processing.R')

# source the data merge step, which sources all of the data processing steps
rain_env <- new.env()
source('scripts/2_calc_rain_variables.R', echo = F, local = rain_env)

weather_env <- new.env()
source('scripts/2_calc_weather_variables.R', echo = F, local = weather_env)
