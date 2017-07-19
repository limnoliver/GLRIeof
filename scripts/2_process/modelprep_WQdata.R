# this script preps the WQ data by aggregating by storm
library(dplyr)
# read in cleaned wq data

wq <- read.csv('data_cached/cleaned_WQdata.csv', stringsAsFactors = FALSE)

# Drop estimated samples
# Drop discrete samples
wq <- filter(wq, discrete == FALSE, estimated == FALSE)

# drop unused levels for factors

# make "<" values equal to half of the censored value

# first, find which variables have a "<"
censored <- grep('<', wq)

ss.censored <- grep('<', wq$Suspended_Sediment_mg_L)
ss.cens.val <- as.numeric(gsub('<', '', wq$Suspended_Sediment_mg_L[ss.censored]))
wq[ss.censored, 'Suspended_Sediment_mg_L'] <- 0.5*ss.cens.val
wq$Suspended_Sediment_mg_L <- as.numeric(wq$Suspended_Sediment_mg_L)

nit.censored <- grep('<', wq$NO2_NO3_N_mg_L)
nit.cens.val <- as.numeric(gsub('<', '', wq$NO2_NO3_N_mg_L[nit.censored]))
wq[nit.censored, 'NO2_NO3_N_mg_L'] <- 0.5*nit.cens.val
wq$NO2_NO3_N_mg_L <- as.numeric(wq$NO2_NO3_N_mg_L)

# add a column that will be used for weighting concentrations by total runoff volume
storm.vols <- wq[,c('unique_storm_id', 'runoff_volume')]
storm.vols <- storm.vols %>%
  group_by(unique_storm_id) %>%
  summarise(sum_runoff = sum(runoff_volume))

wq <- merge(wq, storm.vols, by = 'unique_storm_id', all.x = TRUE)
wq <- mutate(wq, vol_weight = runoff_volume/sum_runoff)

# Handle sub storms
# when combining sub events, take:
# max of peak discharge to report as "peak discharge" for event
# min start date
# max end date
# load = sum of subs
# conc weighted by sum(load)/sum(runoff volume)
loadvars <- grep('load', names(wq), ignore.case = TRUE, value = TRUE)
concvars <- grep('mg_L', names(wq), ignore.case = TRUE, value = TRUE)

loadbystorm <- wq %>% 
  group_by(unique_storm_id) %>%
  summarise_at(vars(loadvars), sum, na.rm = TRUE) 

concbystorm <- wq[,c(concvars, 'unique_storm_id', 'vol_weight')]
concbystorm[, concvars] <- concbystorm[,concvars]*concbystorm$vol_weight

concbystorm <- concbystorm %>%
  group_by(unique_storm_id) %>%
  summarise_at(vars(concvars), sum, na.rm = TRUE)

flagvars <- grep('flag|frozen|comment', names(wq), ignore.case = TRUE, value = TRUE)
flagsbystorm <- wq %>%
  group_by(unique_storm_id) %>%
  summarise_at(vars(flagvars), toString) 

wq.bystorm <- merge(concbystorm, loadbystorm)
wq.bystorm <- merge(wq.bystorm, storm.vols)
wq.bystorm <- merge(wq.bystorm, flagsbystorm)
wq.bystorm <- merge(wq.bystorm, unique(wq[,c('site', 'water_year', 'unique_storm_id')]), all.x = TRUE)
