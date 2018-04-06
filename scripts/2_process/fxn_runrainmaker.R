# Run Rainmaker

# Function to run all functions within rainmaker for multiple rain gauge sites

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
library(dplyr)
library(Rainmaker)

run.rainmaker <- function(precip_raw = precip_raw,
                          siteid = c('441624088045601', '441520088045001'), sitename = c('SW1', 'SW3'), ieHr = 2, rainthresh = 0.008, 
                          wq.dat = wq.dat, xmin = c(5,10,15,30,60), antecedentDays = c(1,3,7,14)) {
  

  
    
    #sites <- siteid
    # read in raw rain gauge data for site[i]
    
    #precip_temp <- filter(precip_raw, site_no == sites[i])
    #wq.dat.temp <- filter(wq.dat, site == sitename[i])
    
    # prep data
    #precip_prep <- RMprep(precip_temp, prep.type = 3, date.type = 2, dates.in = 'Timestamp..UTC.06.00.', 
    #                      dates.out = 'pdate', tz = 'Etc/GMT+6', cnames.in = names(precip_raw), cnames.new = c('timestamp_utc', 'timestamp_utc-6', 'rain', 'approval', 'grade', 'qualifiers'))
    # calculate events
    events <- RMevents_sample(df = precip_raw, ieHr = ieHr, rain = 'rain', time = 'pdate', 
                              dfsamples = wq.dat, bdate = 'storm_start', edate = 'storm_end')
    
    #events <- RMevents_eof(df=precip_temp, storms = wq.dat.temp, site = sites[i], ieHr=ieHr, 
    #                       rainthresh=rainthresh)
    # extract data from events output
    events_list <- events$storms
    tipsbystorm <- events$tipsbystorm
    
    # calculate storm intensity at different time intervals
    StormSummary <- RMintensity(df=tipsbystorm, date="pdate", rain="rain", df.events=events_list,
                              sdate="StartDate", edate="EndDate", depth="rain", xmin=xmin)
    
    # calculate erosivity 
    # method 1
    StormSummary <- RMerosivity(df = tipsbystorm, ieHr=ieHr, rain='rain', StormSummary=StormSummary, method=1)
    StormSummary <- rename(StormSummary, 'erosivity_m1' = "erosivity", 'energy_m1' = 'energy')

    # method 2
    StormSummary <- RMerosivity(df= tipsbystorm, ieHr=ieHr, rain="rain", StormSummary=StormSummary, method=2)
    StormSummary <- rename(StormSummary, 'erosivity_m2' = "erosivity", 'energy_m2' = 'energy')
    
    
    # calculate antecedent rain
    StormSummary <- RMarf(df = tipsbystorm, date = 'pdate', rain = 'rain', df.events = StormSummary, 
                 sdate = "StartDate", days = antecedentDays, varnameout = "ARFdays")
    
    
    # merge all rain data
    StormSummary$site <- sitename[i]
    StormSummary$unique_storm_number <- wq.dat$unique_storm_number
  
  return(StormSummary)
}
