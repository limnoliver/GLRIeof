# Run this file to perform all data analysis

if (study_type == 'before_after') {
# source the water quality file which is the basis for all other processing.
message('Calculating minimum detectable change using the pre-BMP data.')
mdc_env <- new.env()
source('scripts/data_analysis/1_mdc_before_after.R', echo = F, local = mdc_env)



} else {

}
