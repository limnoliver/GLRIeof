# setup master variables for site of interest

# all raw input files should be contained in the same folder 'raw_data'
# all processed data files will be exported to the same folder 'cached_data'
# all figures will be exported to the same folder 'figures'

# set site-specific information
site <- '' # site abbreviation that may be used for file naming conventions
site_no <- '' # USGS site number

study_type <- '' # either "before_after" or "paired"

site_paired <- '' # site abbreviation for paired site - used for file naming conventions
site_no_paired <- '' # USGS paired (control) site number, 'NA' if no paired site

# R likes dates in a very specific format (YYYY-MM-DD, coded in R as "%Y-%m-%d"). The scripts need to know 
# what time zone your input files are in (these should all be the same), and what format your dates are in.
# To figure out the R format code for your own dates, see this brief tutorial: https://www.r-bloggers.com/date-formats-in-r/ 
# you can also see all date codes by looking executing ?strptime in your R Console, 
# and scrolling down to "Details"

site_tz <- '' # all data should be converted (or called into R) to the same time zone. 
              # Indicate here which timezone that is - e.g. central "America/Chicago" or central without DST "Etc/GMT+6"
datetime_format <- '' # format of all datetime columns. For example, 
date_format <- '' # format of date variables in provided data files. For example, the default in Excel is
# mm/dd/yyyy, and the coded format of that date in R is "%m/%d/%Y". 
# dates from Excel are read in the format "" -- but R wants them in the format "YYYY"
# setting this variable read in NWIS data in the same time zone, but will not verify that all of your data
# are in a consistent timezone. Please verify that all data are in appropriate time zones.

start_date <- as.POSIXct('') # YYYY-MM-DD HH:MM:SS date/time of study start
bmp_date <- as.POSIXct('') # YYYY-MM-DD HH:MM:SS date/time of BMP implementation. All events before
                           # this date will be considered 'before', all events after this 
                           # date will be considered 'after'
end_date <- as.POSIXct('') # YYYY-MM-DD HH:MM:SS date/time of study end

######################################
## set wq data parameters
wq_file <- '' # file name (with .csv extention) where event-level water quality data are stored

concentrations <- c('') # this can either be a vector of verbatim constituent concentrations
# columns (e.g, c('SS_mg_L', 'TP_mg_L')) or a character string that all concentration
# columns share in common (e.g., "mg_L")

loads <- c('') # this can either be a vector of verbatim constituent load
# columns (e.g, c('SS_load_pounds', 'TP_load_pounds')) or a character string that all load
# columns share in common (e.g., "pounds")

flags <- c('') # this can either be a vector of verbatim flag
# columns (e.g, c('SS_flag', 'TP_flag')) or a character string that all flag
# columns share in common (e.g., "flag")



###########################################################################################
# If you're using a before-after study design, the variables below should be filled out.
# If you have a paired site, these data aren't necessary, but can be used to run 
# site diagnostics (yearly precipitation, etc)
###########################################################################################
# rain data
rain_site <- '' # site number for rain gage if pulling from NWIS, 'NA' if providing data file
rain_file <- '' # filename of rain file, 'NA' if pulling from NWIS. Should be a csv and include ".csv" at end of file name.

rain_column <- '' # if using own data, the name of the rain value column
date_column <- '' # if using own data, the name of the date/time column, should be in standard POSIXct (YYY-MM-DD HH:MM:SS)
rain_tz <- '' # time zone of date_column. See ?timezones for more information. Central time is

# field activity data
activity_file <- '' # filename of field activity log and should include .csv extension
nut_additions_keywords <- c('manure', 'fertilizer') # words used to describe any manure and/or fertilizer additions 
                                                    # to the field. These should be keywords found in the "activity_group" column
cultivation_keywords <- c('cultivation') # words to describe tillage and other field cultivation activities
                                         # this word will be found in the "activity_group"
planting_keywords <- c('planting') # words to describing planting activities, found in "activity_group" column
harvest_keywords <- c('cutting', 'harvest') # words describing cutting/harvest activities, found in "activity_group" column.

# discharge data
discharge_file <- '' #filename of discharge data of a nearby stream gage to calculate antecedent discharge, NA if pulling from NWIS using site_no
discharge_site_no <- '' # if not providing a discharge file, the USGS stream gage site to pull discharge data from
antecedent_days <- c(1,2,3,7,14) # the days over which to calculate antecedent discharge. These are the values used for the WI sites.

dis_date_column <- 'Date' # name of column with dates if providing own file - defaults to NWIS standards
discharge_column <- 'Flow' # name of column with discharge data if providing own file - defaults to NWIS standards

# 


