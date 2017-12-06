# this script creates a linear model between water quality and hydrologic variables, 
# including variables output by Rainmaker, as well as storm characteristics

# read in libraries
#library(GSqwsr)
#library(randomForest)
library(dplyr)
library(caret)
library(ggplot2)

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
predictors <- names(eof)[c(3,36:51, 53:60, 61:63, 70:73)]


# reduce data to the site of interest, non-frozen periods
# also add before/transition/after BMP

sw1 <- eof %>%
  mutate(frozen = as.logical(substr(eof$frozen, 1, 1))) %>%
  filter(site == 'SW1') %>%
  filter(frozen == FALSE) %>%
  mutate(period = ifelse(storm_start >= as.POSIXct('2015-06-01 00:00:01'), 'after', 'before'))

# remove highly correlated predictors
predictors.cor <- cor(eof[,predictors[-31]], use = 'complete.obs') # drop var "crop" from correlation since it's a categorical var
names.cor <- row.names(predictors.cor)
drop.predictors <- findCorrelation(predictors.cor, cutoff = 0.95, verbose = FALSE, exact = TRUE)


sw1$period[sw1$storm_start > as.POSIXct('2015-05-10 00:00:01')& sw1$storm_start < as.POSIXct('2015-06-01 00:00:01')] <- 'transition'

sw1 <- filter(sw1, period != 'transition')

# transform variables that need it - was determined with simple histograms
# look at histograms
pred.keep <- names.cor[-drop.predictors]
pred.keep.df <- reshape2::melt(sw1[,pred.keep])
ggplot(pred.keep.df, aes(x = value)) +
  geom_histogram() +
  facet_wrap(~variable, scales = 'free')
# vars to transform
# removed ARFdays from transformation even though they were lognormally distributed
# they have true zeros, so will just keep them unstransformed
# also do not transform ant_dis_2day_max
# vars.transform <- c('peak_discharge', 'Chloride_mg_L', 'sum_runoff', 'Ievent', 'I5', 'erosivity_m1', 'ARFdays1', 'ARFdays2',
#                     'ARFdays7', 'ARFdays14', 'ant_dis_2day_max', 'ant_dis_7day_mean', 'ant_dis_7day_max', 'ant_dis_14day_mean',
#                     'ant_dis_14day_max')
vars.transform <- c('peak_discharge', 'Chloride_mg_L', 'Ievent', 'I5', 'erosivity_m1', 'ant_dis_7day_mean', 'ant_dis_7day_max', 'ant_dis_14day_mean',
                    'ant_dis_14day_max')


# get rid of zeros from database - this should be fixed at some point
# should not be zeros for erosivity, etc, but there is an issue with getting sub events to count as a single event
#################Fix this after subevents are fixed, just get rid of next line
sw1 <- filter(sw1, erosivity_m1 > 0)


# transform vars
sw1[,vars.transform] <- log10(sw1[,vars.transform])

# log transform response variables
responses <- names(sw1)[c(2,4:11,13:19)]
responses_clean <- c('SS (mg/L)', 'NO2 + NO3 (mg/L)','NH4 (mg/L)','TKN (mg/L)',
                     'DRP (mg/L)','TP (mg/L)','TN (mg/L)','Org N (mg/L)',
                     'SS (pounds)', 'NO2 + NO3 (pounds)','NH4 (pounds)','TKN (pounds)',
                     'DRP (pounds)','TP (pounds)','TN (pounds)','Org N (pounds)')

sw1[,responses] <- log10(sw1[,responses])

sw1$period_crop <- factor(sw1$period_crop)
sw1$period_crop <- ordered(sw1$period_crop, levels = c('before', 'after (corn)', 'after (alfalfa)'))


######################################################
## create a loop that models all responses
######################################################

# test
mod1 <- lm(sw1$Suspended_Sediment_Load_pounds ~ sw1$Chloride_mg_L + sw1$sum_runoff + sw1$peak_discharge)
mod2 <- lm(sw1$Suspended_Sediment_mg_L ~ sw1$Chloride_mg_L + sw1$sum_runoff + sw1$peak_discharge)

mod1.2 <- 

plot(mod1$residuals ~ as.Date(sw1$storm_start), col = sw1$period_crop,
    xlab = 'Year', ylab = 'Residuals')
plot(mod2$residuals ~ as.Date(sw1$storm_start), col = sw1$period_crop,
                              xlab = 'Year', ylab = 'Residuals')

for (i in 1:length(responses)) {
  mod.equation <- as.formula(paste(responses[i], paste(pred.keep, collapse = " + "), sep = " ~ "))
  
  # now run through lasso model
  temp.mod <- lm(mod.equation, data = sw1)
  # plot model results in three ways
  # first, show fitted vs observed
  # second, show residuals boxplot by period
  # finally, show residuals throughtime, with lines for events
  
  fig.name <- paste0('figures/modsum_', responses[i], '.pdf')
  
  pdf(fig.name, height = 8, width = 5)
  layout_matrix <- matrix(c(1:3), nrow=3, ncol=1, byrow=TRUE)
  layout(layout_matrix)
  par(mar = c(6,6,3,1), oma = c(0,0,0,0), pch = 16)
  
  ####
  plot(sw1[,responses[i]] ~ temp.mod$fitted.values,
       xlab = "Fitted Values",
       ylab = "Observed Values", 
       main = paste('log10', responses_clean[i]))
  
  abline(0,1)
  
  text(x = min(temp.mod$fitted.values) + 0.2, y = max(sw1[,responses[i]])-0.2, 
       labels = paste0('R2 = ', round(summary(temp.mod)$adj.r.squared, 2)), 
       col = 'blue', pos = 4)
  ####
  boxplot(temp.mod$residuals ~ sw1$period_crop, 
          ylab = 'Residuals', col = c('darkgray', 'red', 'green'))
  
  ## #
  plot(temp.mod$residuals ~ as.Date(sw1$storm_start), col = sw1$period_crop,
       xlab = 'Year', ylab = 'Residuals')
  abline(h = 0, lwd = 2)
  dev.off()
}

# fit each model to the "pre" data, and then
# predict concentrations/loads from the storm characteristics of the 
# post data - then calculate percent decrease from predicted post events
# useing the pre model and observed events
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
