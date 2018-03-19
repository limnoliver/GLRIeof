# this script preps the WQ data by aggregating by storm
library(dplyr)
library(readxl)
# read in cleaned wq data

wq <- read.csv('data_cached/cleaned_WQdata.csv', stringsAsFactors = FALSE)
wq.storm.sw1 <- read_excel('data_raw/sw1_event_parsing.xlsx')

# find which storms to keep from storm parsing file
keep.storms <- filter(wq.storm.sw1, exclude == 0) %>%
  filter(estimated == 0) %>%
  filter(discrete == 0) %>%
  select(unique_storm_id, unique_storm_number)

wq <- filter(wq, storm_id %in% keep.storms$unique_storm_id) %>%
  filter(!is.na(runoff_volume)) %>%
  filter(!(lab_id == '376-13-10' & storm_id == 'ESW1-15')) %>% #some duplicated id's that needed to be removed
  rename(unique_storm_id = storm_id) %>%
  left_join(keep.storms, by = 'unique_storm_id')

# Drop estimated samples
# Drop discrete samples
# wq <- filter(wq, discrete == FALSE, estimated == FALSE)

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

chl.censored <- grep('<', wq$Chloride_mg_L)
chl.cens.val <- as.numeric(gsub('<', '', wq$Chloride_mg_L[chl.censored]))
wq[chl.censored, 'Chloride_mg_L'] <- 0.5*chl.cens.val
wq$Chloride_mg_L <- as.numeric(wq$Chloride_mg_L)


# add a column that will be used for weighting concentrations by total runoff volume
storm.vols <- wq[,c('unique_storm_number', 'runoff_volume', 'unique_storm_id')]
storm.vols <- storm.vols %>%
  group_by(unique_storm_number) %>%
  summarise(sum_runoff = sum(runoff_volume), 
            sub_storms = paste(unique_storm_id, collapse = ","))

wq <- merge(wq, storm.vols, by = 'unique_storm_number', all.x = TRUE)
wq <- mutate(wq, vol_weight = runoff_volume/sum_runoff)
# get rid of any samples that do not have a volume weight, 
# which will give us NA values later on
wq <- filter(wq, !is.na(vol_weight))
wq <- filter(wq, !is.na(Suspended_Sediment_mg_L))

# Handle sub storms
# when combining sub events, take:
# max of peak discharge to report as "peak discharge" for event
# min start date
# max end date
# load = sum of subs
# conc weighted by sum(load)/sum(runoff volume)
loadvars <- grep('load', names(wq), ignore.case = TRUE, value = TRUE)
concvars <- grep('mg_L', names(wq), ignore.case = TRUE, value = TRUE)
storm.desc <- grep('start|_end|peak', names(wq), ignore.case = TRUE, value = TRUE)
flagvars <- grep('flag|frozen|comment', names(wq), ignore.case = TRUE, value = TRUE)


loadbystorm <- wq %>% 
  group_by(unique_storm_number) %>%
  summarise_at(vars(loadvars), sum, na.rm = TRUE) 

concbystorm <- wq[,c(concvars, 'unique_storm_number', 'vol_weight')]
concbystorm[, concvars] <- concbystorm[,concvars]*concbystorm[,'vol_weight']

concbystorm <- concbystorm %>%
  group_by(unique_storm_number) %>%
  summarise_at(vars(concvars), sum, na.rm = TRUE)

stormdesc <- wq %>%
  group_by(unique_storm_number) %>%
  summarise(
    sample_start = min(sample_start),
    sample_end = max(sample_end),
    storm_start = min(storm_start),
    storm_end = max(storm_end),
    peak_discharge = max(peak_discharge)
  )

flagsbystorm <- wq %>%
  group_by(unique_storm_number) %>%
  summarise_at(vars(flagvars), toString) 

wq.bystorm <- merge(concbystorm, loadbystorm)
wq.bystorm <- merge(wq.bystorm, storm.vols)
wq.bystorm <- merge(wq.bystorm, flagsbystorm)
wq.bystorm <- merge(wq.bystorm, unique(wq[,c('site', 'water_year', 'unique_storm_number', 'sub_storms')]), all.x = TRUE)
wq.bystorm <- merge(wq.bystorm, stormdesc)

write.csv(wq.bystorm, 'data_cached/prepped_WQbystorm.csv', row.names = FALSE)
