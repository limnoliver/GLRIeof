# this script creates a linear model between water quality and hydrologic variables, 
# including variables output by Rainmaker, as well as storm characteristics

# read in merged data
eof <- read.csv('data_cached/merged_wq_rain_discharge.csv', header = TRUE)
eof$frozen  <- as.logical(substr(eof$frozen, 1, 1))
response <- 'Suspended_Sediment_mg_L'
predictors <- names(eof)[c(20,36,37:51, 53:60)]
  
# get rid of correlated variables using a variance inflation factor (VIF)
# vif source code from https://gist.github.com/fawda123/4717702#file-vif_fun-r

sub.dat <- subset(eof, site == 'SW1')
sub.dat[,predictors] <- data.frame(scale(sub.dat[,predictors], center = TRUE, scale = TRUE))
sub.dat[,response] <- log(sub.dat[,response])

vars.keep <- vif_func(sub.dat[,predictors])

sub.dat <- sub.dat[,c(response, vars.keep)]

#mod.equation <- paste(response, paste(vars.keep, collapse = " + "), sep = " ~ ")
mod.equation <- createFullFormula(sub.dat, response)
sub.dat <- sub.dat[complete.cases(sub.dat),]

# kitchen sink 

returnPrelim <- prelimModelDev(sub.dat, response, mod.equation,
                               k = "BIC", transformResponse = 'normal', autoSinCos = FALSE)

mod <- lm(mod.equation, data = sub.dat)

simple.mod <- lm(Suspended_Sediment_Load_pounds ~ peak_discharge, data = sub.dat)

library(reshape)
head(melt(sub.dat))

library(ggplot2)
ggplot(data = melt(sub.dat), mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')
response <- 'Suspended_Sediment_Load_pounds'
sub.dat <- subset(eof, site == 'SW1')
sub.dat <- subset(sub.dat, !is.na(sub.dat$Suspended_Sediment_Load_pounds))
sub.dat <- subset(sub.dat, !is.na(sub.dat$rain))
sub.dat <- sub.dat[,c(response, predictors)]
sub.dat[,response] <- log10(sub.dat[,response])
test <- randomForest(x = sub.dat[,-1], y = sub.dat[,1], na.action = na.rm)

test2 <- varImpPlot(test)
vars.keep <- row.names(test2)[order(test2, decreasing = TRUE)][1:5]
partialPlot(test, sub.dat, x.var = peak_discharge)
