# Run this file to perform all data analysis

if (study_type == 'before_after') {
  mod_env <- new.env()
  
  # source the mod.dat prep step that will feed into both MDC calculations and modeling
  source('scripts/data_analysis/0_before_after_datmod_prep.R', echo = F, local = mod_env)
  
  # source script to calculate MDC.
  message('Calculating minimum detectable change using the pre-BMP data. This calculation assumes the same number of observations in the post-BMP period.')
  source('scripts/data_analysis/1_mdc_before_after.R', echo = F, local = mod_env)
  
  # source script to do the residual test, and if significant, the calculations for percent change
  # script also generates model figures and sends to figures
  message('Testing whether BMP implementation reduced loads/concentrations.')
  source('scripts/data_analysis/2_before_after_mods.R', echo = F, local = mod_env)
  
  
} else {
  message('Calculating minimum detectable change using the pre-BMP data. This calculation assumes the same number of observations in the post-BMP period.')
  mod_env <- new.env()
  source('scripts/data_analysis/1_mdc_paired.R', echo = F, local = mod_env)
  
  message('Testing whether BMP implementation reduced loads/concentrations.')
  source('scripts/data_analysis/2_paired_mods.R', echo = F, local = mod_env)
  
}
