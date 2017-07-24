library(httr)
httr::set_config(config(ssl_verifypeer = 0L))

library(devtools)
install_github('USGS-R/Rainmaker')

library(Rainmaker)

# read in precipitation data
files <- list.files('data_raw')
sw1.id <- 5601
sw3.id <- 5001
precip.files <- grep('precip', files, value = TRUE, ignore.case = TRUE)
precip.sw1 <- grep(sw1.id, precip.files, value = TRUE)
precip.sw3 <- grep(sw3.id, precip.files, value = TRUE)

run.rain <- function(files, )
  