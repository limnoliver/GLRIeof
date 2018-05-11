# setup master variables for site of interest

# all raw input files should be contained in the same folder 'raw_data'
# all processed data files will be exported to the same folder 'cached_data'
# all figures will be exported to the same folder 'figures'

# set site-specific information
site <- '' # site abbreviation that may be used for file naming conventions
site_no <- '' # USGS site number

study_type <- '' # either "before_after" or "paired"

# for paired studies
site_paired <- '' # site abbreviation for paired site - used for file naming conventions and column identification. 
                  # This should precede your column name for site-specific variables (e.g.)
site_no_paired <- '' # USGS paired (control) site number, 'NA' if no paired site

control_site <- '' # If a paired study design, the abbreviation of the control site. This should be the same abbreviation as one of the sites listed above.
test_site <- '' # If a paired study design, the abbreviation of the test, or BMP implementation, site. This should be the same abbreviation as oneof the sites listed above.

discharge_col <- '' # The name of the discharge columns, if included. Include the column name without 
                    # the test or control prefix. E.g., if your columns are 'sw1_runoff' and 'sw2_runoff',
                    # list the discharnge col name as 'runoff'. Use NA if you're not including discharge/runoff values.

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
                             # This can be the first second of the day on the date of the first sampling event
bmp_date <- as.POSIXct('') # YYYY-MM-DD HH:MM:SS date/time of BMP implementation. All events before
                           # this date will be considered 'before', all events after this 
                           # date will be considered 'after'
end_date <- as.POSIXct('') # YYYY-MM-DD HH:MM:SS date/time of study end

######################################
## set wq data parameters
wq_file <- '' # file name (with .csv extention) where event-level water quality data are stored

# for setting conentrations and loads, only include the variables you want to 
# include in the analysis. E.g., if you only want to evaluate loads (not concentrations), 
# simply put NA for concentrations and fill out the loads variable.

concentrations <- c('') # this can either be a vector of verbatim constituent concentrations
# columns (e.g, c('SS_mg_L', 'TP_mg_L')) or a character string that all concentration
# columns share in common (e.g., "mg_L")

loads <- c('') # this can either be a vector of verbatim constituent load
# columns (e.g, c('SS_load_pounds', 'TP_load_pounds')) or a character string that all load
# columns share in common (e.g., "pounds")

other_responses <- c('') # this is a space for a response variable that may not be a constituent concentration or load. 
# e.g., if you expect the BMP to affect runoff itself, you may want to assess runoff or peak discharge as a response variable.

flags <- c('') # this can either be a vector of verbatim flag
               # columns (e.g, c('SS_flag', 'TP_flag')) or a character string that all flag
               # columns share in common (e.g., "flag")  

clean_names <- c('') # a vector of "clean" response variables names for all variables you want to 
                     # model. This will be used for plotting purposes to create nice looking
                     # axes. For example, if a column was named suspended_sediments_load_pound, a 
                     # better axis name would be "SS load (pounds)". These should be in the same order
                     # as variables listed above, or the order they appear in your spreadsheet if you're 
                     # using a unique string that all load or concentration columns share. Order should be
                     # concentration variables, load variables, and then other response variables. 
                     # For a paired study, just include a single clean name
                     # for each variable - e.g., only provide "SS load (pounds)" instead of "Control - SS load (pounds)"
                     # and "Treatment - SS load (pounds)"

event_over_thaw <- # For subevents that span a thaw, you may have to decide whether to split the events or combine them.
                     # If you decide to combine events, such that part of the event was during the "frozen" period
                     # and part was during the "non-frozen" period, then you need to decided how to classify the 
                     # frozen status of the event. Set this variable to 0 to set the event to "non-frozen" and 1 to 
                     # set the event to "frozen".
  
  
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

# field activity data
activity_file <- '' # filename of field activity log and should include .csv extension
nut_additions_keywords <- c('manure', 'fertilizer') # words used to describe any manure and/or fertilizer additions 
                                                    # to the field. These should be keywords found in the "activity_group" column
cultivation_keywords <- c('cultivation') # words to describe tillage and other field cultivation activities
                                         # this word will be found in the "activity_group"
planting_keywords <- c('planting') # words to describing planting activities, found in "activity_group" column
harvest_keywords <- c('cutting', 'harvest') # words describing cutting/harvest activities, found in "activity_group" column.

# discharge data
discharge_file <- '' # filename of discharge data of a nearby stream gage to calculate antecedent discharge, NA if pulling from NWIS using site_no
discharge_site_no <- '' # if not providing a discharge file, the USGS stream gage site to pull discharge data from
antecedent_days <- c(1,2,3,7,14) # the days over which to calculate antecedent discharge. These are the values used for the WI sites.

dis_date_column <- 'Date' # name of column with dates if providing own file - defaults to NWIS standards
discharge_column <- 'Flow' # name of column with discharge data if providing own file - defaults to NWIS standards

# weather data
weather_file <- '' # filename (ending in .csv) of daily weather data from a nearby met station to calculate storm
                   # weather characteristics, NA if pulling data directly from NOAA (preferred). 
                   # At a minimum, this file should include 'date' and 
                   # daily min (tmin) and max (tmax) temperature in degrees celsius. 
                   # Can also include snow depth  in milimeters (snwd).


noaa_site <- '' # nearest NOAA met station site number. Can use both weather_file and noaa_site if 
                # weather_file does not contain snow depth. 

# other
predictors_drop <- '' # one or more predictor variables that should be dropped from the analysis based
                      # on site-specific analysis goals. For example, if you expect that the implemented
                      # BMP will affect field activity (e.g., if the farmer moves to no-till, 'days_since_cultivation'
                      # will be different before vs after) it should not be included in the residual model.
                      # to get a full list of variables that that can be included in the model, 
                      # run the 'processing_run_file.R' and look at the generated 'merged_dat.csv' file 
                      # stored in data_cached


