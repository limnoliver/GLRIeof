# GLRIeof
Scripts to import, munge, and model edge of field data that is part of the Great Lakes Restoration Initiative

## What these scripts accomplish
1. Pulls storm - level water quality data for all years from sites of interest using `get_StormWQ.R`.
2. Munges water quality data to clean up various coding within data (esimated events, frozen vs non-frozen, etc) using `clean_WQdata.R`
3. Collapses data into storm-level events using `modelprep_WQdata.R`
4. Uses the storms identified above as inputs to RainMaker to calculate precip characteristics (e.g., instead of rainmaker identifying storm events, you tell rainmaker which storms to summarize). Script `calc_rain_variables.R` does this work by calling the modified functions `fxn_RMEvents_EOF.R` and `fxn_runrainmaker.R`.
5. Calculates antecedent discharge metrics using nearby stream information using `fxn_calc_antdischarge.R`
6. Merges water quality, hydrology, and precipitation data using `merge_data.R`
7. Models water quality as a function of hydrology and precipitation. Does a before-and-after analysis of the field-level implementation.

