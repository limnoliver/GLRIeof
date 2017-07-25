library(Rainmaker)
library(lubridate)
library(dplyr)
source('scripts/2_process/fxn_RMevents_EOF.R')

# read in WQ data to define storm start/end times
wq.dat <- read.csv('data_cached/prepped_WQbystorm.csv', header = TRUE)

date.vars <- c('sample_start', 'sample_end', 'storm_start', 'storm_end')

# read in dates as CST/CDT, but then convert to GMT
for (i in 1:length(date.vars)) {
  temp <- as.POSIXct(as.character(wq.dat[,date.vars[i]]), tz = 'America/Chicago')
  temp <- with_tz(temp, tzone = "Etc/GMT+6")
  wq.dat[,date.vars[i]] <- temp
}

# read in precipitation data
files <- list.files('data_raw')
sw1.id <- 5601
sw3.id <- 5001
precip.files <- grep('precip', files, value = TRUE, ignore.case = TRUE)
precip.sw1 <- grep(sw1.id, precip.files, value = TRUE)
precip.sw3 <- grep(sw3.id, precip.files, value = TRUE)

# set working directory to location of precip data
precip.wd <- 'H:/Projects/GLRIeof'

# function to run through all Rainmaker functions and extract rain variables
# precip.dir location of rain gauge files
# precip.files vector of file names with rain gauge data
# siteid unique identifier for site, atleast last four digits, should be in names of precip.files
# sitename unique site names, should be in same order of siteids
# ieHr time between storms to ID new events
# rainthresh minimum rain amount that constitutes an event
# wq.dat dataframe that identifies storm start/end from EOF WQ data
# xmin minutes over which to calculate storm intensity
# antecedentDays days over which to calculate antecedent rainfall

run.rainmaker <- function(precip.dir = 'H:/Projects/GLRIeof/data_raw', precip.files, 
                     siteid = c(5601, 5001), sitename = c('SW1', 'SW3'), ieHr = 2, rainthresh = 0.008, wq.dat = wq.dat,
                     xmin = c(5,10,15,30,60), antecedentDays = c(1,2,3,7,14)) {
  
  for (i in 1:length(precip.files)) {
    
    # read in raw rain gauge data for site[i]
    precip.raw.file <- grep(siteid[i], precip.files, value = TRUE)
    precip_raw <- read.csv(file = paste(precip.dir, precip.raw.file, sep = "/"), header = TRUE, skip = 14)
    
    # prep data
    precip_prep <- RMprep(precip_raw, prep.type = 3, date.type = 2, dates.in = 'Timestamp..UTC.06.00.', 
                              dates.out = 'pdate', tz = 'Etc/GMT+6', cnames.in = names(precip_raw), cnames.new = c('timestamp_utc', 'timestamp_utc-6', 'rain', 'approval', 'grade', 'qualifiers'))
    # calculate events
    events <- RMevents_eof(df=precip_prep, storms = wq.dat, site = sitename[i], ieHr=ieHr, 
                           rainthresh=rainthresh, rain='rain', time='pdate')
    # extract data from events output
    events_list <- events$storms
    tipsbystorm <- events$tipsbystorm
   
    # calculate storm intensity at different time intervals
    StormSummary <- RMIntense(df=tipsbystorm, date="pdate", rain="rain", df.events=events_list,
                              sdate="StartDate", edate="EndDate", depth="rain", xmin=xmin)
    
    # calculate erosivity 
    StormSummary.1 <- RMerosivity(df = tipsbystorm, ieHr=ieHr, rain='rain', StormSummary=StormSummary, method=1)
    erosivity.col <- grep('erosivity', names(StormSummary.1))
    names(StormSummary.1)[erosivity.col] <- 'erosivity_m1'
    
    # calculate erosivity using method 2
    StormSummary.2 <- RMerosivity(df= tipsbystorm, ieHr=ieHr, rain="rain", StormSummary=StormSummary, method=2)
    erosivity.col <- grep('erosivity', names(StormSummary.2))
    names(StormSummary.2)[erosivity.col] <- 'erosivity_m2'
    
    # calculate antecedent rain
    ARF <- RMarf(df = tipsbystorm, date = 'pdate', rain = 'rain', df.events = StormSummary, 
                 sdate = "StartDate", days = antecedentDays, varnameout = "ARFdays")
    
    
    
    
    }





# merge rain summary data 
arf.cols <- grep('arf', names(ARF), ignore.case = TRUE, value = TRUE)
dat.combine <- list(StormSummary.1, StormSummary.2[,c('stormnum', 'erosivity_m2')], ARF[,c('stormnum', arf.cols)])
merged.rain <- Reduce(function(...) merge(..., all = T), dat.combine)

write.csv(merged.rain, 'data_cached/rain_variables.csv', row.names = FALSE)

wq.rain <- merge(wq.dat, )
  