# script to clean storm WQ data from EOF monitoring
library(lubridate)
source('scripts/0_import/get_StormWQ.R')

wq <- read.csv('data_raw/WQdata.csv', header = TRUE)

# set dates to GMT
.origin <- as.POSIXct(ifelse(Sys.info()[['sysname']] == "Windows", "1899-12-30", "1904-01-01"))
tz(.origin) <- 'ETC/GMT+6'

date.vars <- c('sample_start', 'sample_end', 'storm_start', 'storm_end')
for (i in 1:length(date.vars)) {
  temp <- as.POSIXct((3600*24*wq[,date.vars[i]]), origin = .origin, tz = 'ETC/GMT+6')
  wq[,date.vars[i]] <- force_tz(temp, 'America/Chicago')
}

# clean up comments to keep commenter name but remove '\n'
wq$comments <- gsub('\n', '',wq$comments)

# clean up site IDs where SW1 was named in slightly different ways
wq$site <- substr(wq$site, 1, 3)

# create a unique storm id list that does not use subset storms
wq$unique_storm_id <- gsub("(^.*)(SW1-[[:digit:]]+)([[:alpha:]]*)", "\\2", wq$storm_id, ignore.case = TRUE)
wq$unique_storm_id <- gsub("(^.*)(SW3-[[:digit:]]+)([[:alpha:]]*)", "\\2", wq$unique_storm_id, ignore.case= TRUE)

# clean up names
names.wq <- names(wq)
# remove lab analysis numbers
names.wq <- gsub('[[:digit:]]{2,}', "", names.wq)
# turn periods into underscores
names.wq <- gsub('\\.+', '_', names.wq)
# clean up units
names.wq <- gsub('Load_in_pounds', 'load_pounds', names.wq, ignore.case = TRUE)
names(wq) <- names.wq
write.csv(wq,'data_cached/cleaned_WQdata.csv', row.names = FALSE)

plot(log10(wq$Total.Nitrogen.Load..in.pounds)~log10(wq$peak_discharge))
