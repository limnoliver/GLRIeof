# Run this file to run all data processing/analysis steps

# source the master file with all site-specific vars
source('scripts/0_master_file_test.R', echo = F)

# source the test file to check inputs in 0_master_file
source('scripts/data_processing/0_check_inputs.R')

if (study_type == 'before_after') {
# source the water quality file which is the basis for all other processing.
message('Importing and processing the storm water quality data for the before & after study.')
wq_env <- new.env()
source('scripts/data_processing/1_calc_storm_wq.R', echo = F, local = wq_env)

# source functions that are needed
source('scripts/functions/fxns_data_processing.R')

# source all data processing steps
message('Importing and processing rain metrics.')
rain_env <- new.env()
source('scripts/data_processing/2_calc_rain_variables.R', echo = F, local = rain_env)

message('Importing and processing weather metrics.')
weather_env <- new.env()
source('scripts/data_processing/2_calc_weather_variables.R', echo = F, local = weather_env)

message('Importing and processing discharge metrics.')
dis_env <- new.env()
source('scripts/data_processing/2_calc_discharge_variables.R', echo = F, local = dis_env)

message('Importing and processing field activity metrics.')
field_env <- new.env()
source('scripts/data_processing/2_calc_field_predictors.R', echo = F, local = field_env)

# source the merge step which uses the CSVs of all processing steps.
message('Merging all processed data.')
merge_env <- new.env()
source('scripts/data_processing/3_merge_data.R', echo = F, local = merge_env)

# source the merge processing step
message('Prepping merged data for analysis.')
mod_dat_env <- new.env()
source('scripts/data_processing/4_process_merged_data.R', echo = F, local = mod_dat_env)

# source the diagnostic plots
message('Creating diagnostic plots for the before/after study')
diag_env <- new.env()
source('scripts/data_processing/5_diagnostics_plots.R', echo = F, local = diag_env)

} else {
  # import water quality
  message('Importing and processing the storm water quality data for the paired study.')
  wq_env <- new.env()
  source('scripts/data_processing/1_calc_storm_wq_paired.R', echo = F, local = wq_env)

  # create diagnostic plots
  message('Creating diagnostic plots/data for paired study.')
  diag_env <- new.env()
  source('scripts/data_processing/5_paired_diagnostics_plots.R', echo = F, local = diag_env)
}
