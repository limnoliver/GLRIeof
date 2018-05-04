# tests to run on master file to make sure user has input
# variables correctly
source('scripts/functions/fxns_tests.R')

stopifnot(study_type %in% c('before_after', 'paired'))

if (study_type == "before_after") {
  # check if user has included ".csv" in file extensions
  # water quality dat
  test.csv(wq_file)
  
  # rain dat
  if (is.na(rain_site)){
    test.csv(rain_file)
  }
  # field activity file
  test.csv(activity_file)
  
  # discharge dat
  if (is.na(discharge_site_no)) {
    test.csv(discharge_file)
  }
  
  # weather dat
  if (is.na(noaa_site)) {
    test.csv(weather_file)
  }
  
  
  
}

