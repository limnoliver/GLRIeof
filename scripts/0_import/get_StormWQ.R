#library(openxlsx)


###################################
# automate import functions
###################################
library(XLConnect)
library(xlsx)
detach('package:XLConnect', unload = TRUE)


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
sheet.names.short <- substr(sheet.names, 1, 3)
sheet.names.num <- grep(paste(sites, collapse = '|'), sheet.names.short)
sheet.names <- sheet.names[sheet.names.num]

###################################
# import data from excel files
###################################
sheet.dat <- list()
for (k in 1:length(sheet.names)){
  file.all <- xlsx::read.xlsx(temp.file.path, sheetIndex = sheet.names[k], header = FALSE)
  row.start <- grep('start', file.all[,1], ignore.case = TRUE)
  row.end <- grep('yearly', file.all[,1], ignore.case = TRUE)
  file.dat <- xlsx::read.xlsx(temp.file.path, 
                              sheetIndex = sheet.names[k], startRow = row.start+1, endRow = row.end-2, header = FALSE)
  

# define rows where names of columns are
  
names.1 <- as.character(unlist(as.list(file.all[grep('sample information', file.all[,1], ignore.case = TRUE),])))
names.2 <- as.character(unlist(as.list(file.all[grep('sample times', file.all[,1], ignore.case = TRUE),])))
names.3 <- as.character(unlist(as.list(file.all[grep('start', file.all[,1], ignore.case = TRUE),])))

sample.start <- grep('sample', names.2, ignore.case = TRUE)
sample.end <- sample.start + 1
field <- grep('field', names.3, ignore.case = TRUE)
start <- grep('storm times', names.2, ignore.case = TRUE)
stop <- start + 1
labid <- grep('uwsp', names.3, ignore.case = TRUE)
numsubsamples <- grep('subsample', names.1, ignore.case = TRUE)
discharge <- grep('peak discharge', names.1, ignore.case = TRUE)
stormtype <- grep('storm type', names.1, ignore.case = TRUE)

# consider changing the next grep to only include the variable names instead
# because if vars are in different order in each spreadsheet - vars might be off
wqvars <- grep('load|mg/L|flag', names.1, ignore.case = TRUE, value = FALSE)

# set which column indices to keep
col.keep <- c(field, labid, numsubsamples, sample.start, sample.end, start, stop, discharge, stormtype, wqvars)
# filter data frame with columns to keep
dat.keep <- file.dat[,col.keep]

df.names <- c('storm_id', 'lab_id', 'num_subsamples', 'sample_start', 'sample_end', 'storm_start', 'storm_end', 'peak_discharge', 'storm_type')
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

#########################################
# find rows that are estimated, not measured
# also find rows that are discrete
# these are blue rows in spreadsheet
# not going to use color but other clues from populated/unpopulated cells

dat.keep$estimated <- is.na(dat.keep$lab_id)&is.na(dat.keep$num_subsamples)
dat.keep$discrete <- !is.na(dat.keep$sample_start)&is.na(dat.keep$sample_end)& !is.na(dat.keep$lab_id)

#####################################
# extract comments from cells in excel
# and save them all as character strings in "comment" column

# import workbook
wb <- xlsx::loadWorkbook(temp.file.path)
sheet1 <- xlsx::getSheets(wb)[[k]]

# get all rows
rows  <- xlsx::getRows(sheet1)

# extract cells and comments in cells
cells <- xlsx::getCells(rows[1:row.end])
comments <- sapply(cells, xlsx::getCellComment)
comments2 <- c()

# save comments as strings
for (i in 1:length(cells)){
  if (is.null(comments[[i]])){
    comments2[i] <- NA
  } else {
    comments2[i] <- comments[[i]]$getString()$toString()
  }
}
# make vector into data frame
comments3 <- as.data.frame(matrix(comments2, nrow = length(rows), byrow = TRUE))
comments.formatted <- ""

# find all non-NA comment values, and paste all comments from every row together into single
# character string
for (i in 1:nrow(comments3)){
  keep <- which(!is.na(comments3[i,]))
  if (length(keep) > 0) {
    comments.formatted[i] <- paste(as.character(comments3[i,keep]), collapse = ',')
  } else {
    comments.formatted[i] <- NA
  }
}

# create a new column in dat.keep for comments
dat.keep$comments <- comments.formatted
 
sheet.dat[[k]] <- dat.keep                        
} # closes k loop
cleaned.dat[[j]] <- sheet.dat
} # closes j loop
##############################
# end of useful script
#############################

styles <- sapply(cells, xlsx::getCellStyle)




values <- lapply(cells, xlsx::getCellValue(keepFormulas = TRUE))

test <- getCellValue(rows[row.not.frozen])
# 

cellColor <- function(style) {
  fg  <- style$getFillForegroundXSSFColor()
  rgb <- tryCatch(fg$getRgb(), error = function(e) NULL)
  rgb <- paste(rgb, collapse = "")
  return(rgb)
}
table.cols <- sapply(styles, cellColor)

table.cols <- unique(as.character(sapply(styles, cellColor)))
table.cols <- table.cols[c(1,3:9)]
table.cols <- paste('#', table.cols, sep = "")
plot(1:length(table.cols), 1:length(table.cols), pch = 16, col = table.cols, cex = 4)



# iterate through each WY and get site files
# i = wy
# j = site
