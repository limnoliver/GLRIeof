# import storm-specific water quality data
wq <- read.csv(file.path('data_raw', wq_file), na.strings = c("", "NA"), stringsAsFactors = F)
  
# check to see if all required columns are in data frame 
# use list in stickies to set this, not quite sure what the complete list is
must.haves <- c('storm_start', 'storm_end', 'sample_start', 'sample_end', 'runoff_volume', 
                'unique_storm_number', 'unique_storm_id', 'peak_discharge', 'frozen')

# set concentration, load, and flag variables
if (!all(must.haves %in% names(wq))) {
  vars.missing <- must.haves[which(!must.haves %in% names(wq))]
  stop(paste0('The water quality data is missing column(s): ', paste0(vars.missing, collapse = ', ')), 
        call. = F)
}

# get conc/load vars
if (length(loads) == 1 & !is.na(loads)){
  loadvars <- grep(loads, names(wq), ignore.case = TRUE, value = TRUE)
} else if (is.na(loads)) {
  loadvars <- NA
} else {
  loadvars <- loads
}

if (length(concentrations) == 1 & !is.na(concentrations)){
  concvars <- grep(concentrations, names(wq), ignore.case = TRUE, value = TRUE)
} else if (is.na(concentrations)) {
  concvars <- NA
} else {
  concvars <- concentrations
}

if (length(flags) == 1 & !is.na(flags)) {
  flagvars <- grep(flags, names(wq), ignore.case = TRUE, value = TRUE)
} else if (is.na(flags)) {
  flagvars <- NA
} else {
  flagvars <- flags
}


# set dates to time zone
.origin <- as.POSIXct(ifelse(Sys.info()[['sysname']] == "Windows", "1899-12-30", "1904-01-01"))
tz(.origin) <- site_tz

date.vars <- c('sample_start', 'sample_end', 'storm_start', 'storm_end')

for (i in 1:length(date.vars)) {
  wq[,date.vars[i]] <- as.POSIXct(wq[,date.vars[i]], origin = .origin, tz = site_tz, format = datetime_format)
}

# clean up the data to exclude estimated values, combine sub storm events, etc.
# exclude storms that have a 1 in exclude, are estimated, or are discrete samples
storms <- filter(wq, exclude == 0) %>%
  filter(estimated == 0) %>%
  filter(discrete == 0)

# make "<" values equal to half of the censored value

# first, find which variables have a "<"
# and replace with 0.5 * value
if (!is.na(flagvars[1])){
for (i in 1:length(flagvars)) {
  flags <- grep('<', storms[, flagvars[i]])
  storms[flags, concvars[i]] <- 0.5*storms[flags, concvars[i]]
  print(paste0(length(flags), ' observations below detection limit for ', concvars[i]))
}
}

# combine sub storms
# add a column that will be used for weighting concentrations by total runoff volume
storm.vols <- wq[,c('unique_storm_number', 'runoff_volume', 'unique_storm_id')]
storm.vols <- storm.vols %>%
  group_by(unique_storm_number) %>%
  summarise(sum_runoff = sum(runoff_volume), 
            sub_storms = paste(unique_storm_id, collapse = ","))

storms <- merge(storms, storm.vols, by = 'unique_storm_number', all.x = TRUE)
storms <- mutate(storms, vol_weight = runoff_volume/sum_runoff)

# get rid of any samples that do not have a volume weight, 
# which will give us NA values later on
storms <- filter(storms, !is.na(vol_weight))

# Handle sub storms
# when combining sub events, take:
# max of peak discharge to report as "peak discharge" for event
# min start date
# max end date
# load = sum of subs
# conc weighted by sum(load)/sum(runoff volume)


loadbystorm <- storms %>% 
  group_by(unique_storm_number) %>%
  summarise_at(vars(loadvars), sum, na.rm = TRUE) 

concbystorm <- storms[,c(concvars, 'unique_storm_number', 'vol_weight')]
concbystorm[, concvars] <- concbystorm[,concvars]*concbystorm[,'vol_weight']

concbystorm <- concbystorm %>%
  group_by(unique_storm_number) %>%
  summarise_at(vars(concvars), sum, na.rm = TRUE)

stormdesc <- storms %>%
  group_by(unique_storm_number) %>%
  summarise(
    sample_start = min(sample_start),
    sample_end = max(sample_end),
    storm_start = min(storm_start),
    storm_end = max(storm_end),
    peak_discharge = max(peak_discharge), 
    runoff_volume = sum(runoff_volume), 
    frozen = paste0(unique(frozen), collapse = ', ')
  )

stormdesc$frozen <- as.numeric(ifelse(nchar(stormdesc$frozen) == 1, stormdesc$frozen, event_over_thaw))

flagsbystorm <- storms %>%
  group_by(unique_storm_number) %>%
  summarise_at(vars(flagvars), toString) 

wq.bystorm <- merge(concbystorm, loadbystorm)
wq.bystorm <- merge(wq.bystorm, storm.vols)
wq.bystorm <- merge(wq.bystorm, flagsbystorm)
wq.bystorm <- merge(wq.bystorm, unique(storms[,c('unique_storm_number', 'sub_storms')]), all.x = TRUE)
wq.bystorm <- merge(wq.bystorm, stormdesc)

# check if wq.bystorm has more than one row, and print message. Otherwise, stop process.

temp_filename <- file.path("data_cached", paste0(site, "_", "prepped_WQbystorm.csv"))
write.csv(wq.bystorm, temp_filename, row.names = FALSE)


if (nrow(wq.bystorm) > 0) {
  message(paste('Water quality data is now processed. See', temp_filename, 'to ensure correct processing.'))
} else {
  stop("Something went wrong with processing of water quality data. To debug, look through code in 'scipts/data_processing/1_calc_storm_wq.R'.")
}

