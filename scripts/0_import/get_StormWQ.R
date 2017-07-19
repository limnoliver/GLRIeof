#library(openxlsx)


###################################
# automate import functions
###################################
options(java.parameters = "-Xmx1000m")

library(XLConnect)
library(xlsx)
#detach('package:XLConnect', unload = TRUE)

jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}  
# point to folder where data are stored
wd <- 'M:/NonPoint Evaluation/GLRI Edge-of-field/Upper East River GLRI'

# retrive all water years from folder
files.wd <- list.files(wd)
wy <- grep('WY[[:digit:]]{2}', files.wd, value = TRUE)

# right now, overwride wy to only include through 2016
wy <- wy[1:5]

# set site names to find in tab names
# for now, will use test cases of SW1 and SW3
sites <- c('SW1', 'SW3')

all.files <- c()

# this find the files we're looking for, which are probably
# wisconsin-specific

for (i in 1:length(wy)) {
  # read in workbook
setwd(paste(wd, wy[i], sep = "/"))
files <- list.files()
files <- grep('Loads and Yields with Formulas', files, value = TRUE, ignore.case = TRUE)
files.drop <- grep('\\$|copy|working|updated', files, ignore.case = TRUE)
if (length(files.drop) > 0){
  all.files[i] <- files[-files.drop]
} else {
  all.files[i] <- files
}
}

cleaned.dat <- list()
for (j in 1:length(all.files)) {

temp.file.path <- paste(wd, wy[j], all.files[j], sep = "/")
all.sheets <- XLConnect::loadWorkbook(temp.file.path)
sheet.names <- XLConnect::getSheets(all.sheets)
sheet.names.short <- substr(sheet.names, 1, 4)
sheet.names.short <- gsub("-", "", sheet.names.short)
sheet.names.num <- grep(paste('^', sites,'$', collapse = '|', sep = ""), sheet.names.short)
sheet.names <- sheet.names[sheet.names.num]

###################################
# import data from excel files
###################################
sheet.dat <- list()
for (k in 1:length(sheet.names)){
  jgc()
  message("Creating sheet", j, k)
  #sheet <- createSheet(wb, sheetName = names(the_data)[i])
  #message("Adding data frame", i)
  #addDataFrame(the_data[[i]], sheet)
  
  file.all <- xlsx::read.xlsx(temp.file.path, sheetIndex = sheet.names[k], header = FALSE)
  row.start <- grep('start', file.all[,1], ignore.case = TRUE)
  row.end <- grep('yearly', file.all[,1], ignore.case = TRUE)
  col.start <- 1
  file.dat <- file.all[c((row.start+1):(row.end-2)), ]
  #file.dat <- xlsx::read.xlsx(temp.file.path, 
  #                            sheetIndex = sheet.names[k], startRow = row.start+1, endRow = row.end-2, header = FALSE)
  

# define rows where names of columns are
  
#names.1 <- as.character(unlist(as.list(file.all[grep('sample information', file.all[,1], ignore.case = TRUE),])))
#names.2 <- as.character(unlist(as.list(file.all[grep('sample times', file.all[,1], ignore.case = TRUE),])))
#names.3 <- as.character(unlist(as.list(file.all[grep('start', file.all[,1], ignore.case = TRUE),])))

names.1 <- as.character(lapply(file.all[grep('sample information', file.all[,1], ignore.case = TRUE),], as.character))
names.2 <- as.character(lapply(file.all[grep('sample times', file.all[,1], ignore.case = TRUE),], as.character))
names.3 <- as.character(lapply(file.all[grep('start', file.all[,1], ignore.case = TRUE),], as.character))

col.end <- grep('organic nitrogen yield', names.1, ignore.case = TRUE)

sample.start <- grep('sample', names.2, ignore.case = TRUE)
sample.end <- sample.start + 1
field <- grep('field', names.3, ignore.case = TRUE)
start <- grep('storm times', names.2, ignore.case = TRUE)
stop <- start + 1
labid <- grep('uwsp|#', names.3, ignore.case = TRUE)
numsubsamples <- grep('subsample', names.1, ignore.case = TRUE)
discharge <- grep('peak discharge', names.1, ignore.case = TRUE)
runoff.vol <- grep('storm runoff.*cubic feet', names.1, ignore.case = TRUE)
instant.discharge <- grep('instant', names.1, ignore.case = TRUE)
stormtype <- grep('storm type', names.1, ignore.case = TRUE)

# consider changing the next grep to only include the variable names instead
# because if vars are in different order in each spreadsheet - vars might be off
wqvars <- grep('load|mg/L|flag', names.1, ignore.case = TRUE, value = FALSE)

# set which column indices to keep
col.keep <- c(field, labid, numsubsamples, sample.start, sample.end, start, stop, discharge, runoff.vol, instant.discharge, stormtype, wqvars)
# filter data frame with columns to keep
dat.keep <- file.dat[,col.keep]

df.names <- c('storm_id', 'lab_id', 'num_subsamples', 'sample_start', 'sample_end', 'storm_start', 'storm_end', 'peak_discharge', 'runoff_volume', 'instant_discharge', 'storm_type')
wqvars.names <- grep('load|mg/L|flag', names.1, ignore.case = TRUE, value = TRUE)

names(dat.keep) <- c(df.names, wqvars.names)

#######################################
# Define frozen/not frozen from the equations at the bottom of the spreadsheet
##############################################

file.all.formulas <- xlsx::read.xlsx(temp.file.path, 
                            sheetIndex = sheet.names[k], header = FALSE, keepFormulas = TRUE, startRow = row.start+1, endRow = row.end+2)
row.not.frozen <- grep('non', file.all.formulas[,1], ignore.case = TRUE)
row.frozen <- grep('^frozen', file.all.formulas[,1], ignore.case = TRUE)

# ID a column where the frozen/unfrozen distinction will always be made
eq.col <- grep('storm.*cubic feet', names.1, ignore.case = TRUE)

# get frozen rows
eq.frozen <- as.character(file.all.formulas[row.frozen, eq.col])
frozen.rows <- as.numeric(unlist(strsplit(unlist(eq.frozen),"[^0-9]+")))
frozen.rows <- frozen.rows[!is.na(frozen.rows)]
frozen.all.rows <- frozen.rows[1]:frozen.rows[2]

if (length(frozen.rows) > 2){
  temp <- frozen.rows[3]:frozen.rows[4]
  frozen.all.rows <- c(frozen.all.rows, temp)
} else {
  frozen.all.rows <- frozen.all.rows
}

# get non-frozen rows
eq.not.frozen <- as.character(file.all.formulas[row.not.frozen, eq.col])
not.frozen.rows <- as.numeric(unlist(strsplit(unlist(eq.not.frozen),"[^0-9]+")))
not.frozen.rows <- not.frozen.rows[!is.na(not.frozen.rows)]
not.frozen.all.rows <- not.frozen.rows[1]:not.frozen.rows[2]

if (length(not.frozen.rows) > 2){
  temp <- not.frozen.rows[3]:not.frozen.rows[4]
  not.frozen.all.rows <- c(not.frozen.all.rows, temp)
} else {
  not.frozen.all.rows <- not.frozen.all.rows
}

# subtract number of ignored rows so first row = 1
frozen.all.rows <- frozen.all.rows - row.start
not.frozen.all.rows <- not.frozen.all.rows - row.start  

# create a frozen column where frozen = TRUE, not frozen = FALSE
dat.keep$frozen <- NA
dat.keep$frozen[frozen.all.rows] <- TRUE
dat.keep$frozen[not.frozen.all.rows] <- FALSE

# create a site and water year column
dat.keep$site <- sheet.names[k]
dat.keep$water_year <- wy[j]
#########################################
# find rows that are estimated, not measured
# also find rows that are discrete
# these are blue rows in spreadsheet
# not going to use color but other clues from populated/unpopulated cells

dat.keep$estimated <- is.na(dat.keep$lab_id)&is.na(dat.keep$num_subsamples)
# 2016 documented differently, so above line results in estimated = TRUE for all samples
# below code is a way around this
dat.keep$estimated[dat.keep$water_year == 'WY16'] <- is.na(dat.keep$runoff_volume[dat.keep$water_year == 'WY16']) & is.na(dat.keep$instant_discharge[dat.keep$water_year == 'WY16']) 
# 2015 also has slightly different populated cells for estimated values
dat.keep$estimated[dat.keep$water_year == 'WY15'] <- is.na(dat.keep$sample_start[dat.keep$water_year == 'WY15'])
dat.keep$discrete <- !is.na(dat.keep$sample_start)&is.na(dat.keep$sample_end)& !is.na(dat.keep$lab_id)

#####################################
# extract comments from cells in excel
# and save them all as character strings in "comment" column

# import workbook
wb <- xlsx::loadWorkbook(temp.file.path)
sheet1 <- xlsx::getSheets(wb)[[k]]

# get all rows
rows  <- xlsx::getRows(sheet1, rowIndex = c((row.start+1):(row.end-2)))

# extract cells and comments in cells
cells <- xlsx::getCells(rows, colIndex=c(col.start:col.end))
comments <- lapply(cells, xlsx::getCellComment)
comments.address <- names(comments)
comments.rows <- gsub('([[:digit:]]+)(\\..+)', '\\1', comments.address)
comments.rows <- as.numeric(comments.rows)
comments.columns <- gsub('([[:digit:]]+\\.)([[:digit:]]+)', '\\2', comments.address)
comments.columns <- as.numeric(comments.columns)

comments2 <- c()


# save comments as strings
for (i in 1:length(cells)){
  if (is.null(comments[[i]])){
    comments2[i] <- NA
  } else {
    comments2[i] <- comments[[i]]$getString()$toString()
  }
}


comments3 <- c()

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
}

# look at names
# only difference is orthophosphate turns into dissolved reactive 

names.list <- lapply(temp.c, names)

# make list of dataframes into one data frame

extracted.dat <- rbindlist(temp.c)
# warning that this can go wrong if columns are not in the same order

# write data
setwd("H:/Projects/GLRIeof")
write.csv(extracted.dat, 'data_raw/WQdata.csv', row.names = FALSE)
