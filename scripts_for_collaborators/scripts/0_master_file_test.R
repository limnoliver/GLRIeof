# setup master variables for site of interest

# all raw input files should be contained in the same folder 'raw_data'
# all processed data files will be exported to the same folder 'cached_data'
# all figures will be exported to the same folder 'figures'

# set site-specific information
site <- 'test' # site abbreviation that may be used for file naming conventions
site_no <- "441520088045001" # USGS site number

study_type <- 'before_after' # either "before_after" or "paired"

site_paired <- NA # site abbreviation for paired site - used for file naming conventions. NA if no paired site.
site_no_paired <- NA # USGS paired (control) site number, 'NA' if no paired site

# R likes dates in a very specific format (YYYY-MM-DD, coded in R as "%Y-%m-%d"). The scripts need to know 
# what time zone your input files are in (these should all be the same), and what format your dates are in.
# To figure out the R format code for your own dates, see this brief tutorial: https://www.r-bloggers.com/date-formats-in-r/ 
# you can also see all date codes by looking executing ?strptime in your R Console, 
# and scrolling down to "Details"

site_tz <- 'Etc/GMT+6' # all data should be in (or called into R) the same time zone. 
                       # Indicate here which timezone that is - e.g. central "America/Chicago" or central without DST "Etc/GMT+6"
datetime_format <- "%m/%d/%Y %H:%M" # format of all datetime columns.
date_format <- '%m/%d/%Y' # format of date variables in provided data files. For example, the default in Excel is
# mm/dd/yyyy, and the coded format of that date in R is "%m/%d/%Y". 
# dates from Excel are read in the format "" -- but R wants them in the format "YYYY"
# setting this variable read in NWIS data in the same time zone, but will not verify that all of your data
# are in a consistent timezone. Please verify that all data are in appropriate time zones.

start_date <- as.POSIXct('2014-03-11 00:00:01') # YYYY-MM-DD HH:MM:SS date/time of study start
                                                
bmp_date <- as.POSIXct('2017-05-26 00:00:01') # YYYY-MM-DD HH:MM:SS date/time of BMP implementation. All events before
                           # this date will be considered 'before', all events after this 
                           # date will be considered 'after'
end_date <- as.POSIXct('2017-08-04 00:00:01') # YYYY-MM-DD HH:MM:SS date/time of study end

######################################
## set wq data parameters
wq_file <- 'test_storm_wq.csv' # file name (with .csv extention) where event-level water quality data are stored

concentrations <- c('mg_L') # this can either be a vector of verbatim constituent concentrations
# columns (e.g, c('SS_mg_L', 'TP_mg_L')) or a character string that all concentration
# columns share in common (e.g., "mg_L")

loads <- c('pounds') # this can either be a vector of verbatim constituent load
# columns (e.g, c('SS_load_pounds', 'TP_load_pounds')) or a character string that all load
# columns share in common (e.g., "pounds")

flags <- c('flag') # this can either be a vector of verbatim flag
# columns (e.g, c('SS_flag', 'TP_flag')) or a character string that all flag
# columns share in common (e.g., "flag")

clean_names <- c('SS (mg/L)', 'TP (mg/L)', 'SS load (pounds)', 'TP load (pounds)') # a vector of "clean" response variables names for all variables you want to 
                     # model. This will be used for plotting purposes to create nice looking
                     # axes. For example, if a column was named suspended_sediments_load_pound, a 
                     # better axis name would be "SS load (pounds)"

###########################################################################################
# If you're using a before-after study design, the variables below should be filled out.
# If you have a paired site, these data aren't necessary, but can be used to run 
# site diagnostics (yearly precipitation, etc)
###########################################################################################
# rain data
rain_site <- "441520088045001" # site number for rain gage if pulling from NWIS, 'NA' if providing data file
rain_file <- NA # filename of rain file, 'NA' if pulling from NWIS. Should be a csv and include ".csv" at end of file name.

rain_column <- '' # if using own data, the name of the rain value column
date_column <- '' # if using own data, the name of the date/time column, should be in standard POSIXct (YYY-MM-DD HH:MM:SS)

# field activity data
activity_file <- 'test_field_activity' # filename of field activity log and should include .csv extension
nut_additions_keywords <- c('manure', 'fertilizer') # words used to describe any manure and/or fertilizer additions 
                                                    # to the field. These should be keywords found in the "activity_group" column
cultivation_keywords <- c('cultivation') # words to describe tillage and other field cultivation activities
                                         # this word will be found in the "activity_group"
planting_keywords <- c('planting') # words to describing planting activities, found in "activity_group" column
harvest_keywords <- c('cutting', 'harvest') # words describing cutting/harvest activities, found in "activity_group" column.

# discharge data
discharge_file <- NA # filename of discharge data of a nearby stream gage to calculate antecedent discharge, NA if pulling from NWIS using site_no
discharge_site_no <- '04085108' # if not providing a discharge file, the USGS stream gage site to pull discharge data from
antecedent_days <- c(1,2,3,7,14) # the days over which to calculate antecedent discharge. These are the values used for the WI sites.

dis_date_column <- 'Date' # name of column with dates if providing own file - defaults to NWIS standards
discharge_column <- 'Flow' # name of column with discharge data if providing own file - defaults to NWIS standards

# weather data
weather_file <- NA # filename (ending in .csv) of daily weather data from a nearby met station to calculate storm
                   # weather characteristics, NA if pulling data directly from NOAA (preferred). 
                   # At a minimum, this file should include 'date' and 
                   # daily min (tmin) and max (tmax) temperature in degrees celsius. 
                   # Can also include snow depth  in millimeters (snwd).


noaa_site <- 'USW00014898' # nearest NOAA met station site number. Can use both weather_file and noaa_site if 
                           # weather_file does not contain snow depth. 

# other
predictors_drop <- c('days_since_cultivation', 'days_since_planting')
                      # one or more predictor variables that should be dropped from the analysis based
                      # on site-specific analysis goals. For example, if you expect that the implemented
                      # BMP will affect field activity (e.g., if the farmer moves to no-till, 'days_since_cultivation'
                      # will be different before vs after) it should not be included in the residual model.
                      # to get a full list of variables that that can be included in the model, 
                      # run the 'processing_run_file.R' and look at the generated 'merged_dat.csv' file 
                      # stored in data_cached


