# import storm-specific water quality data
wq <- read.csv(file.path('data_raw', wq_file), na.strings = c("", "NA"), stringsAsFactors = F)
  
# check to see if all required columns are in data frame 
# use list in stickies to set this, not quite sure what the complete list is

# get conc/load vars
if (length(loads) == 1){
  loadvars <- grep(loads, names(wq), ignore.case = TRUE, value = TRUE)
} else {
  loadvars <- loads
}

if (length(concentrations) == 1){
  concvars <- grep(concentrations, names(wq), ignore.case = TRUE, value = TRUE)
} else {
  loadvars <- concentrations
}

if (length(flags) == 1) {
  flagvars <- grep(flags, names(wq), ignore.case = TRUE, value = TRUE)
} else {
  flagvars <- flags
}

# set dates to time zone
.origin <- as.POSIXct(ifelse(Sys.info()[['sysname']] == "Windows", "1899-12-30", "1904-01-01"))
tz(.origin) <- site_tz

date.vars <- grep('storm_start', names(wq), value = T)

for (i in 1:length(date.vars)) {
  wq[,date.vars[i]] <- as.POSIXct(wq[,date.vars[i]], origin = .origin, tz = site_tz, format = datetime_format)
}

# make "<" values equal to half of the censored value

# first, find which variables have a "<"
# and replace with 0.5 * value

for (i in 1:length(flagvars)) {
  flags <- grep('<', storms[, flagvars[i]])
  storms[flags, concvars[i]] <- 0.5*storms[flags, concvars[i]]
  print(paste0(length(flags), ' observations below detection limit for ', concvars[i]))
}

temp_filename <- file.path("data_cached", paste0(site, "_", "prepped_WQbystorm.csv"))
write.csv(wq, temp_filename, row.names = FALSE)
