# GLRIeof
Scripts to import, munge, and model edge of field data that is part of the Great Lakes Restoration Initiative

## What these scripts accomplish
1. Pulls storm event information (hydrology, water quality) for all years from sites of interest
2. Uses the storms identified above as inputs to RainMaker (e.g., instead of rainmaker identifying storm events, you tell rainmaker which storms to summarize).
3. Merges water quality, hydrology, and precipitation data
4. Models water quality as a function of hydrology and precipitation. Does a before-and-after analysis of the field-level implementation.

