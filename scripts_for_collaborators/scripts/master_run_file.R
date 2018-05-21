# this script will source all scripts required for data processing and analysis

#################################
## Get the R packages you need ##

# you likely need the latest version of Rainmaker, which is only on github. 
# Uncomment the line below and run (only need to do this once)
# devtools::install_github('USGS-R/Rainmaker')
# you may need to install package devtools in order for this command to run

# load libraries
library(dplyr)
library(Rainmaker)
library(dataRetrieval)
library(USGSHydroTools)
library(lubridate)
library(rnoaa)
library(randomForest)
library(ggplot2)
library(pdp)
library(jtools)

# if you do not have certain libraries installed (e.g., the code above failes for one or 
# more packages) you need to install them (one time only). To do so, uncomment the lines below
# and fill in the package names.
# install.packages(c('lubridate', 'rnoaa'))

###########################
## Process your raw data ##

source('scripts/run_files/processing_run_file.R', echo = F)

## Analyze your data ##
source('scripts/run_files/analysis_run_file.R', echo = F)
