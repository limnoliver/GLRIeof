# function to run discharge calculations on multiple sites
# uses TSstats to calculate antecedent discharge using storm start dates

library(USGSHydroTools)

# discharge.dir path of raw discharge data
# list of discharge files
# siteid unqiue site identifiers, atleast last four digits
# sitename site name codes, listed in same order as siteid
# antecedentDays days over which to calculate antecedent discharge
# storms = dataframe with storm start time in a column, along with stormid
# start.col character string of column name with storm start date/time
# stats = what type of summary stats you want for antecedent discharge

ant.discharge.bysite <- function(discharge.dir, discharge.files, siteid = c(5601, 5001), sitename = c('SW1', 'SW3'), 
                                 antecedentDays = c(1,2,3,7,14), storms = storms, start.col = 'StartDate', stats = c('mean', 'max')){
  if (length(discharge.files) == 1) {
    discharge_raw <- read.csv(file = paste(discharge.dir, discharge.files, sep = "/"), header = TRUE, skip = 14)
    discharge_raw$pdate <- as.POSIXct(as.character(discharge_raw$Timestamp..UTC.06.00.), tz = 'Etc/GMT+6')
    temp.storms <- subset(storms, site == sitename)
    discharge_vars <- TSstats(discharge_raw, date = 'pdate', varnames = 'Value', dates = temp.storms, starttime = start.col,
                              times = antecedentDays, units = 'days', stats.return = stats)
    return(discharge_vars)
  } else {
    calc.discharge <- list()
    for (i in 1:length(discharge.files)){
      discharge.raw.file <- grep(siteid[i], discharge.files, value = TRUE)
      discharge_raw <- read.csv(file = paste(discharge.dir, discharge.raw.file, sep = "/"), header = TRUE, skip = 14)
      discharge_raw$pdate <- as.POSIXct(as.character(discharge_raw$Timestamp..UTC.06.00.), tz = 'Etc/GMT+6')
      
      temp.storms <- subset(storms, site == sitename[i])
      discharge_vars <- TSstats(discharge_raw, date = 'pdate', varnames = 'Value', dates = temp.storms, starttime = start.col,
                                times = antecedentDays, units = 'days', stats.return = stats)
      calc.discharge[[i]] <- discharge_vars
      
    }
    return(do.call("rbind", calc.discharge))
  }
}