library(XLConnect)
#library(openxlsx)

library(xlsx)

###################################
# import data from excel files
###################################

file.all <- xlsx::read.xlsx('M:/NonPoint Evaluation/GLRI Edge-of-field/Upper East River GLRI/WY12/East River Water Year 2012 Runoff Volumes, Concentrations, Loads and Yields with Formulas.xlsx', 
                  sheetIndex = 1, header = FALSE)
row.start <- grep('start', file.all[,1], ignore.case = TRUE)
row.end <- grep('yearly', file.all[,1], ignore.case = TRUE)
file.dat <- xlsx::read.xlsx('M:/NonPoint Evaluation/GLRI Edge-of-field/Upper East River GLRI/WY12/East River Water Year 2012 Runoff Volumes, Concentrations, Loads and Yields with Formulas.xlsx', 
                      sheetIndex = 1, startRow = row.start+1, endRow = row.end-2, header = FALSE)

# define rows where names of columns are
names.1 <- as.character(unlist(as.list(file.all[grep('sample information', file.all[,1], ignore.case = TRUE),])))
names.2 <- as.character(unlist(as.list(file.all[grep('sample times', file.all[,1], ignore.case = TRUE),])))
names.3 <- as.character(unlist(as.list(file.all[grep('start', file.all[,1], ignore.case = TRUE),])))

sample.start <- grep('sample', names.2, ignore.case = TRUE)
sample.end <- sample.start + 1
field <- grep('field', names.3, ignore.case = TRUE)
start <- grep('storm times', names.2, ignore.case = TRUE)
stop <- start + 1
discharge <- grep('peak discharge', names.1, ignore.case = TRUE)
stormtype <- grep('storm type', names.1, ignore.case = TRUE)

# consider changing the next grep to only include the variable names instead
# because if vars are in different order in each spreadsheet - vars might be off
wqvars <- grep('load|mg/L|flag', names.1, ignore.case = TRUE, value = FALSE)

# filter data frame with columns to keep
dat.keep <- file.dat[,c(field, sample.start, sample.end, start, stop, discharge, stormtype, wqvars)]

df.names <- c('storm_id', 'sample_start', 'sample_end', 'storm_start', 'storm_end', 'peak_discharge', 'storm_type')
wqvars.names <- grep('load|mg/L|flag', names.1, ignore.case = TRUE, value = TRUE)

names(dat.keep) <- c(df.names, wqvars.names)

head(dat.keep)

# Define frozen/not frozen from the equations at the bottom of the spreadsheet

row.not.frozen <- grep('non', file.all[,1], ignore.case = TRUE)
row.frozen <- grep('^frozen', file.all[,1], ignore.case = TRUE)

# ID a column where the frozen/unfrozen distinction will always be made
eq.col <- grep('storm.*cubic feet', names.1, ignore.case = TRUE)

# ID which samples are estimated and discrete

for (i in 1:nrow(dat.keep))

#####################################
# track formatting in original excel file and code as
# a categorical variable
# blue == storm not sampled (likely will ignore these)

# likely use row IDs from above to ID where data starts

# find the columns of interest

wb <- xlsx::loadWorkbook('M:/NonPoint Evaluation/GLRI Edge-of-field/Upper East River GLRI/WY12/East River Water Year 2012 Runoff Volumes, Concentrations, Loads and Yields with Formulas.xlsx')
sheet1 <- xlsx::getSheets(wb)[[1]]

# get all rows
rows  <- getRows(sheet1)
not.frozen.row <- getCells(rows[row.not.frozen])
frozen.row <- getCells(rows[row.frozen])

cells <- getCells(rows[(row.start+2):(row.end-1)])
styles <- sapply(cells, xlsx::getCellStyle)



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
# point to folder where data are stored
wd <- 'M:/NonPoint Evaluation/GLRI Edge-of-field/Upper East River GLRI'

# retrive all water years from folder
files.wd <- list.files(wd)
wy <- grep('WY[[:digit:]]{2}', files.wd, value = TRUE)

# set site names to find in tab names

sites <- c('SW1', 'SW2', 'SW3')

# iterate through each WY and get site files
# i = wy
# j = site

for (i in 1:length(wy))
# read in workbook
setwd(paste(wd, wy[i], sep = "/"))
files <- list.files()
files <- grep('Loads and Yields with Formulas.xlsx', files, value = TRUE)
files.drop <- grep('\\$', files)
if (length(files.drop) > 0){
  files <- files[-files.drop]
}

all.sheets <- loadWorkbook(files)
sheet.names <- names(all.sheets)
sheet.names <- grep(paste(sites, collapse = '|'), sheet.names, value = TRUE)
styles(all.sheets)
for (j in 1:length(sheet.names)) {
  
}

test <- loadWorkbook('M:/NonPoint Evaluation/GLRI Edge-of-field/Upper East River GLRI/WY16/Final East River Water Year 2016 Runoff Volumes, Concentrations, Loads and Yields with Formulas.xlsx')
names.wb <- names(test)
