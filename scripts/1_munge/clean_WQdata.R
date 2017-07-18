# script to clean storm WQ data from EOF monitoring
library(lubridate)
source('scripts/0_import/get_StormWQ.R')

wq <- read.csv('data_raw/WQdata.csv', header = TRUE)

# set dates to GMT
.origin <- as.POSIXct(ifelse(Sys.info()[['sysname']] == "Windows", "1899-12-30", "1904-01-01"))
tz(.origin) <- 'ETC/GMT+6'
wq$sample_start <- as.Date(wq$sample_start, origin = .origin, tz = 'America/Chicago')
wq$sample_start <- as.POSIXct(wq$sample_start, tz = 'American/Chicago')
wq[,c('sample_start', 'sample_end', 'storm_start', 'storm_end')] <- as.POSIXct(3600*24*(wq[,c('sample_start', 'sample_end', 'storm_start', 'storm_end')]), origin = .origin, tz = "ETC/GMT+6")

date.vars <- c('sample_start', 'sample_end', 'storm_start', 'storm_end')
for (i in 1:length(date.vars)) {
  temp <- as.POSIXct((3600*24*wq[,date.vars[i]]), origin = .origin, tz = 'ETC/GMT+6')
  wq[,date.vars[i]] <- force_tz(temp, 'America/Chicago')
}

# clean up comments to keep commenter name but remove '\n'

wq$comments <- gsub('\n', '',dat.keep$comments)
