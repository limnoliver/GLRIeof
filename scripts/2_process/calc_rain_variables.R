library(Rainmaker)
library(lubridate)
library(dplyr)
library(dataRetrieval)
source('scripts/2_process/fxn_RMevents_EOF.R')
source('scripts/2_process/fxn_runrainmaker.R')

site <- 'sw3'
site_no <- ifelse(site == 'sw1', "441624088045601", "441520088045001")

wq_filename <- file.path('data_cached', paste0(site, '_prepped_WQbystorm.csv'))
# read in WQ data to define storm start/end times
wq.dat <- read.csv(wq_filename, header = TRUE)



# read in dates as CST/CDT, but then convert to GMT+6 to get rid of CDT
date.vars <- c('sample_start', 'sample_end', 'storm_start', 'storm_end')
for (i in 1:length(date.vars)) {
  temp <- as.POSIXct(as.character(wq.dat[,date.vars[i]]), tz = 'America/Chicago')
  temp <- with_tz(temp, tzone = "Etc/GMT+6")
  wq.dat[,date.vars[i]] <- temp
}



# list precipitation data files
#files <- list.files('data_raw')
#precip.files <- grep('precip', files, value = TRUE, ignore.case = TRUE)

# get precipitation data from NWIS using dataretrieval
# siteNumber <- c("441624088045601", "441520088045001")
# parameterCd <- "00045"  # Precipitation
# #startDate <- "2010-06-30"
# #endDate <- "2017-09-18"
# precip_raw <- readNWISuv(site_no, parameterCd, tz = "Etc/GMT+6")
# precip_raw <- renameNWISColumns(precip_raw)
# 
# # rename columns
# names(precip_raw)[grep('precip_inst$', names(precip_raw), ignore.case = TRUE)] <- 'rain'
# names(precip_raw)[grep('dateTime', names(precip_raw), ignore.case = TRUE)] <- 'pdate'

# data retrieval currently not working for precip data, need to pull from local file

precip.location <- "L:/Oliver"
precip_files <- list.files(precip.location)[grep('precipitation', list.files(precip.location), ignore.case = T)]
precip_file <- precip_files[grep(site, precip_files, ignore.case = TRUE)]

 
#colClasses = c(rep("POSIXct", 2), 'numeric', 'character', NA, 'character')
precip_raw <- read.csv(file.path(precip.location, precip_file), stringsAsFactors = FALSE, skip = 14, 
                       strip.white = TRUE)

precip_raw$ISO.8601.UTC <- gsub('T', ' ', precip_raw$ISO.8601.UTC)
precip_raw$ISO.8601.UTC <- gsub('Z', '', precip_raw$ISO.8601.UTC)

precip_raw$`Timestamp..UTC.06.00.`<- as.POSIXct(precip_raw$`Timestamp..UTC.06.00.`, tz = "Etc/GMT+6")
precip_raw$ISO.8601.UTC <- as.POSIXct(precip_raw$ISO.8601.UTC, tz = "UTC")

if (site == 'sw1') {
# clean up precipitation data that we know is wrong/bad
# this includes a large event on 2016-09-14 that was a calibration run
# 2015-07-16 through 2015-07-21 (use SW3 data)
# 2016-06-04 (use SW3 data)
# 2016-06-10 through 2016-06-13 (use SW3 data)
# this is all info from Todd

# find the value > 1.2 on 2016-09-14 to exclude
exclude <- which.max(precip_raw$Value)
precip_raw$Value[exclude] <- 0

# read in SW1 data to replace bad data
precip.location <- "L:/Oliver"
precip.file <- "SW3 UV Precipitation 2012-2017.csv"
precip_raw_sw3 <- read.csv(file.path(precip.location, precip.file), stringsAsFactors = FALSE, skip = 14, 
                       strip.white = TRUE)

precip_raw_sw3$ISO.8601.UTC <- gsub('T', ' ', precip_raw_sw3$ISO.8601.UTC)
precip_raw_sw3$ISO.8601.UTC <- gsub('Z', '', precip_raw_sw3$ISO.8601.UTC)


precip_raw_sw3$`Timestamp..UTC.06.00.`<- as.POSIXct(precip_raw_sw3$`Timestamp..UTC.06.00.`, tz = "Etc/GMT+6")
precip_raw_sw3$ISO.8601.UTC <- as.POSIXct(precip_raw_sw3$ISO.8601.UTC, tz = "UTC")

# filter out data from precip_raw and replace with same dates from precip_raw_sw3
precip_raw <- filter(precip_raw, (`Timestamp..UTC.06.00.` < as.POSIXct('2015-07-16 00:00:00', tz = "Etc/GMT+6")) |  
                 (`Timestamp..UTC.06.00.` > as.POSIXct('2015-07-21 23:59:59', tz = "Etc/GMT+6"))) %>%
  filter(`Timestamp..UTC.06.00.` < as.POSIXct('2016-06-04 00:00:00', tz = "Etc/GMT+6") |
           `Timestamp..UTC.06.00.` > as.POSIXct('2016-06-04 23:59:59', tz = "Etc/GMT+6")) %>%
  filter(`Timestamp..UTC.06.00.` < as.POSIXct('2016-06-10 00:00:00', tz = "Etc/GMT+6") |
           `Timestamp..UTC.06.00.` > as.POSIXct('2016-06-13 23:59:59', tz = "Etc/GMT+6")) 

replacement_dates1 <-  filter(precip_raw_sw3, (`Timestamp..UTC.06.00.` >= as.POSIXct('2015-07-16 00:00:00', tz = "Etc/GMT+6")) &  
                               (`Timestamp..UTC.06.00.` <= as.POSIXct('2015-07-21 23:59:59', tz = "Etc/GMT+6")))
replacement_dates2 <- filter(precip_raw_sw3, `Timestamp..UTC.06.00.` >= as.POSIXct('2016-06-04 00:00:00', tz = "Etc/GMT+6") &
           `Timestamp..UTC.06.00.` <= as.POSIXct('2016-06-04 23:59:59', tz = "Etc/GMT+6"))
replacement_dates3 <- filter(precip_raw_sw3, `Timestamp..UTC.06.00.` >= as.POSIXct('2016-06-10 00:00:00', tz = "Etc/GMT+6") &
           `Timestamp..UTC.06.00.` <= as.POSIXct('2016-06-13 23:59:59', tz = "Etc/GMT+6")) 
replacement_dates <- bind_rows(replacement_dates1, replacement_dates2, replacement_dates3)

precip_raw_fixed <- bind_rows(precip_raw, replacement_dates) %>%
  rename(pdate = `Timestamp..UTC.06.00.`, rain = Value) %>%
  arrange(pdate) %>%
  mutate(site_no = '441624088045601')

}

precip_raw <- rename(precip_raw, pdate = `Timestamp..UTC.06.00.`, rain = Value) %>%
  arrange(pdate)
# summarize precip data site(s) of interest
precip.dat <- run.rainmaker(precip_raw = precip_raw, siteid = site_no,
                            sitename = site, ieHr = 2, rainthresh = 0.008, wq.dat = wq.dat,
                            xmin = c(5,10,15,30,60), antecedentDays = c(1,2,7,14))

test <- filter(precip_raw_fixed, pdate >= as.POSIXct('2017-03-05 13:59:59', tz = "Etc/GMT+6") & 
                 pdate <= as.POSIXct('2017-03-07 12:45:00', tz = "Etc/GMT+6"))
# precip.dat <- run.rainmaker(precip_raw, siteid = c('441624088045601', '441520088045001'),
#                             sitename = c("SW1", "SW3"), ieHr = 2, rainthresh = 0.008, wq.dat = wq.dat,
#                             xmin = c(5,10,15,30,60), antecedentDays = c(1,2,7,14))

precip_filename <- file.path('data_cached', paste0(site, '_rain_variables.csv'))
write.csv(precip.dat, precip_filename, row.names = FALSE)

  