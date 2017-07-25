# This script calculates antecedent discharge relative to storms
# for EOF 

# can use RMarf to calculate antecedent discharge (instead of rain)
# or can use TSstats in package USGSHydroTools

install.packages('USGSHydroTools')
library(USGSHydroTools)
library(Rainmaker)
storms <- read.csv('data_cached/rain_variables.csv', colClasses = c(StartDate = 'POSIXct', EndDate = 'POSIXct'))



# read in storm start/end times
discharge.dir <- 'H:/Projects/GLRIeof/data_raw'
files <- list.files(discharge.wd)
discharge.files <- grep('discharge', files, value = TRUE, ignore.case = TRUE)

ant.discharge.bysite <- function(discharge.dir, discharge.files, siteid = c(5601, 5001), sitename = c('SW1', 'SW3'), 
                                 antecedentDays = c(1,2,3,7,14)){
  for (i in 1:length(discharge.files)){
    discharge.raw.file <- grep(siteid[i], discharge.files, value = TRUE)
    discharge_raw <- read.csv(file = paste(discharge.dir, discharge.raw.file, sep = "/"), header = TRUE, skip = 14)
    discharge_raw$pdate <- as.POSIXct(as.character(discharge_raw$Timestamp..UTC.06.00.), tz = 'Etc/GMT+6')
    
    discharge_vars <- TSstats(discharge_raw, date = 'pdate', varnames = 'Value', dates = storms, starttime = 'StartDate',
            times = antecedentDays, units = 'days', stats.return = c('mean', 'max'))
    
    ARF <- RMarf(df = tipsbystorm, date = 'pdate', rain = 'rain', df.events = StormSummary, times = antecedentDays,
                 sdate = "StartDate", days = antecedentDays, varnameout = "ARFdays")
  }
}
 


