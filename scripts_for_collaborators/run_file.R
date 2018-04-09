# Run this file to run all data processing/analysis steps

# source master file
source('0_master_file.R')

# source functions that are needed
source('scripts/fxns_data_processing.R')

# source the data merge step, which sources all of the data processing steps
source('scripts/')