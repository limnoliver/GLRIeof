# this script creates a linear model between water quality and hydrologic variables, 
# including variables output by Rainmaker, as well as storm characteristics

# read in libraries
#library(GSqwsr)
#library(randomForest)
library(dplyr)
library(caret)

# source functions
source('scripts/3_analyze/fxn_vif.R')

# read in merged data
eof <- read.csv('data_cached/merged_dat.csv', header = TRUE, stringsAsFactors = FALSE,
                colClasses = c(sample_start = 'POSIXct'))
eof$crop <- eof$period_crop
eof$crop <- ifelse(eof$crop == "after (alfalfa)", "alfalfa", "corn")
eof$crop <- as.factor(eof$crop)

# create a water equivalent var that sums rain and 
# water equivalent of snow, estimated as 1:10
eof$weq <- ifelse(eof$snwd_diff > 0, eof$rain, eof$rain + (abs(eof$snwd_diff)/10))

# set responses and predictors
response <- 'Suspended_Sediment_mg_L'
# start with all predictors - 34 in total
predictors <- names(eof)[c(3,20,36:51, 53:60, 62:65, 68, 73:75)]


# reduce data to the site of interest, non-frozen periods
# also add before/transition/after BMP

sw1 <- eof %>%
  mutate(frozen = as.logical(substr(eof$frozen, 1, 1))) %>%
  filter(site == 'SW1') %>%
  #filter(frozen == FALSE) %>%
  mutate(period = ifelse(storm_start >= as.POSIXct('2015-06-01 00:00:01'), 'after', 'before'))

# remove highly correlated predictors
predictors.cor <- cor(eof[,predictors[-34]], use = 'complete.obs')
names.cor <- row.names(predictors.cor)
drop.predictors <- findCorrelation(predictors.cor, cutoff = 0.95, verbose = FALSE, exact = TRUE)


sw1$period[sw1$storm_start > as.POSIXct('2015-05-10 00:00:01')& sw1$storm_start < as.POSIXct('2015-06-01 00:00:01')] <- 'transition'

sw1 <- filter(sw1, period != 'transition')

# transform variables that need it - was determined with simple histograms

vars.transform <- c('peak_discharge', 'Chloride_mg_L', 'sum_runoff', 'Ievent', )

for (i in 1:length(responses)) {
  t.response <- sw1[responses[i],]
  
  
  
  
}
# get rid of correlated variables using a variance inflation factor (VIF)
# vif source code from https://gist.github.com/fawda123/4717702#file-vif_fun-r


sub.dat[,predictors] <- data.frame(scale(sub.dat[,predictors], center = TRUE, scale = TRUE))
sub.dat[,response] <- log(sub.dat[,response])

vars.keep <- vif_func(sub.dat[,predictors])

sub.dat2 <- sub.dat[,c(response, vars.keep)]

mod.equation <- paste(response, paste(vars.keep, collapse = " + "), sep = " ~ ")
mod.equation <- createFullFormula(sub.dat, response)
keep.rows <- complete.cases(sub.dat2)
sub.dat <- sub.dat[complete.cases(sub.dat),]

# kitchen sink 

returnPrelim <- prelimModelDev(sub.dat, "Suspended_Sediment_mg_L", mod.equation,
                               k = "BIC", transformResponse = 'normal', autoSinCos = FALSE)

mod2 <- lm(mod.equation, data = sub.dat, y = TRUE)

BorA <- sub.dat$intervention[keep.rows]
plot(as.numeric(mod2$fitted.values) ~ mod2$y, col = as.factor(BorA))

mod.resids <- data.frame(residuals = mod2$residuals,
                         intervention = BorA)

boxplot(mod.resids$residuals ~ mod.resids$intervention)
abline(0,1, col = 'red')
simple.mod <- lm(Suspended_Sediment_mg_L ~ peak_discharge, data = sub.dat)
sub.dat.all <- left_join(sub.dat, )
plot(simple.mod$residuals ~ as.POSIXct(eof.sw1$storm_start[keep.rows]))
plot(eof.sw1$sum_runoff ~ as.POSIXct(eof.sw1$storm_start))
abline(v = as.POSIXct("2014-11-01"), col = 'red')

eof.sw1$runof_per_rain <- eof.sw1$sum_runoff/eof.sw1$rain
plot(log(eof.sw1$runof_per_rain) ~ as.POSIXct(eof.sw1$storm_start))
abline(v = as.POSIXct("2014-11-01"), col = 'red')

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

plot(test$predicted~test$y)
