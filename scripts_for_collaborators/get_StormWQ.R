###################################
# import data from excel files
###################################

wq <- read.csv(file.path('data_raw', wq_file))
  
# check to see if all required columns are in data frame 
# use list in stickies to set this, not quite sure what the complete list is
must.haves <- c('storm_start', 'storm_end', 'runoff_volume')
df.names <- c('storm_id', 'lab_id', 'num_subsamples', 'sample_start', 'sample_end', 'storm_start', 'storm_end', 'peak_discharge', 'runoff_volume', 'instant_discharge', 'storm_type')
wqvars.names <- grep('load|mg/L|flag', names.1, ignore.case = TRUE, value = TRUE)






for (i in 1:length(rows)) {
  row.sum <- comments2[which(comments.rows == i+row.start)]
  row.sum <- row.sum[!is.na(row.sum)]
  
  if (length(row.sum) == 0) {
    comments3[i] <- NA
  } else if (length(row.sum) == 1) {
    comments3[i] <- row.sum
  } else {
    comments3[i] <- paste(row.sum, collapse = ";")
  }
}


# create a new column in dat.keep for comments
dat.keep$comments <- comments3


sheet.dat[[k]] <- dat.keep
remove()
} # closes k loop
cleaned.dat[[j]] <- sheet.dat
} # closes j loop

##############################
# extract data from list and save
#################################
library(data.table)
temp.c <- list()

for (i in 1:length(cleaned.dat)) {
  temp <- cleaned.dat[[i]]
  temp.c[[i]] <- do.call("rbind", temp)
  
  tn.cols <- grep('total nitrogen', names(temp.c[[i]]), ignore.case = T)
  temp.c[[i]][, tn.cols][temp.c[[i]][, tn.cols] == FALSE] <- NA
}

# look at names
# only difference is orthophosphate turns into dissolved reactive 

names.list <- lapply(temp.c, names)

# make list of dataframes into one data frame

extracted.dat <- rbindlist(temp.c)
# warning that this can go wrong if columns are not in the same order

# write data
# setwd("H:/Projects/GLRIeof")
write.csv(extracted.dat, 'data_raw/WQdata.csv', row.names = FALSE)
