# this script creates a linear model between water quality and hydrologic variables, 
# including variables output by Rainmaker, as well as storm characteristics
source('scripts/3_analyze/holdout_cv_glmnet.R')

# read in libraries
#library(GSqwsr)
#library(randomForest)
library(dplyr)
library(caret)
library(ggplot2)
library(glmnet)

# source functions
# source('scripts/3_analyze/fxn_vif.R')
source('scripts/3_analyze/holdout_cv_glmnet.R')


# transform data

# do not need to do this if using random forest or other non-linear test
# remove highly correlated predictors
predictors.cor <- cor(eof[,predictors[-31]], use = 'complete.obs') # drop var "crop" from correlation since it's a categorical var
names.cor <- row.names(predictors.cor)
drop.predictors <- findCorrelation(predictors.cor, cutoff = 0.95, verbose = FALSE, exact = TRUE)

# transform variables that need it - was determined with simple histograms
# look at histograms
# also do not need to do this if we are using non-linear technique
pred.keep <- names.cor[-drop.predictors]
pred.keep.df <- reshape2::melt(sw1[,pred.keep])
ggplot(pred.keep.df, aes(x = value)) +
  geom_histogram() +
  facet_wrap(~variable, scales = 'free')

# vars to transform
vars.transform <- c('peak_discharge', 'Chloride_mg_L', 'Ievent', 'I5', 'ant_dis_7day_mean', 'ant_dis_7day_max', 'ant_dis_14day_mean',
                    'ant_dis_14day_max')

# add 0.001 to antecedent rainfall 0s so that I can log transform
hist(log10(sw1$ARFdays14 + 0.01), breaks = 30)
length(which(sw1$ant_dis_2day_max == 0))

# transform vars
sw1[,vars.transform] <- log10(sw1[,vars.transform])

# for vars that just have a couple of variables nad could benefit from 
# log transformation, will add 0.5 of minimum value tp log transformation
# this includes 7 and 4 day antecedent rainfall and erosivity

hist(log10(sw1$ARFdays14 + (0.5*min(sw1$ARFdays14[sw1$ARFdays14 > 0]))))
sw1$erosivity_m1 <- log10(sw1$erosivity_m1 + (0.5*min(sw1$erosivity_m1[sw1$erosivity_m1 > 0])))
sw1$ARFdays7 <- log10(sw1$ARFdays7 + (0.5*min(sw1$ARFdays7[sw1$ARFdays7 > 0])))
sw1$ARFdays14 <- log10(sw1$ARFdays14 + (0.5*min(sw1$ARFdays14[sw1$ARFdays14 > 0])))

# still do this if using non-linear techniques - response vars
# are very clearly skewed
# log transform responses
sw1[,responses] <- log10(sw1[,responses])

######################################################
## create a loop that models all responses
######################################################

############ kitchen sink linear model
for (i in 1:length(responses)) {
  mod.equation <- as.formula(paste(responses[i], paste(c(pred.keep, 'crop'), collapse = " + "), sep = " ~ "))
  
  temp.mod <- lm(mod.equation, data = sw1)
  # plot model results in three ways
  # first, show fitted vs observed
  # second, show residuals boxplot by period
  # finally, show residuals throughtime, with lines for events
  
  
  fig.name <- paste0('figures/modsum_', responses[i], '.pdf')
  
  pdf(fig.name, height = 8, width = 8)
  layout_matrix <- matrix(c(1:4), nrow=2, ncol=2, byrow=FALSE)
  layout(layout_matrix)
  par(mar = c(6,6,3,1), oma = c(0,0,0,0), pch = 16)
  
  ####
  plot(sw1[,responses[i]] ~ temp.mod$fitted.values,
       xlab = "Fitted Values",
       ylab = "Observed Values", 
       main = paste('log10', responses_clean[i]), 
       col = sw1$period)
  
  abline(0,1)
  
  text(x = min(temp.mod$fitted.values) + 0.2, y = max(sw1[,responses[i]])-0.2, 
       labels = paste0('R2 = ', round(summary(temp.mod)$adj.r.squared, 2)), 
       col = 'blue', pos = 4)
  ####
  plot(temp.mod$residuals ~ temp.mod$fitted.values, 
       xlab = "Fitted Values", 
       ylab = "Residuals", col = sw1$period)
  abline(h = 0)
  boxplot(temp.mod$residuals ~ sw1$period, 
          ylab = 'Residuals', col = c('darkgray', 'red'))
  
  ## #
  plot(temp.mod$residuals ~ as.Date(sw1$storm_start), col = sw1$period,
       xlab = 'Year', ylab = 'Residuals')
  abline(h = 0, lwd = 2)
  dev.off()
}

matIVs <- as.matrix(sw1[,pred.keep])
colnames(matIVs) <- pred.keep

mod.out <- list()
for (i in 1:length(responses)) {
  y <- sw1[,responses[i]]
  #mod.equation <- as.formula(paste(responses[i], paste(pred.keep, collapse = " + "), sep = " ~ "))
  
  # now run through lasso model
  #temp.mod <- lm(mod.equation, data = sw1)
  # plot model results in three ways
  # first, show fitted vs observed
  # second, show residuals boxplot by period
  # finally, show residuals throughtime, with lines for events
  
  mod.out[[i]] <- run.holdout(matIVs, y)
}

par(mfrow=c(4,4), mar = c(1,1,3,1))
for (i in 1:length(responses)) {
  plot(mod.out[[i]][[11]]$observed ~ mod.out[[i]][[11]]$predicted,
       main = responses_clean[i])
  abline(0,1)
}

######################
# colored by crop_period (if crop is not in the model)
for (i in 1:length(responses)) {
  
  mod.equation <- as.formula(paste(responses[i], paste(pred.keep, collapse = " + "), sep = " ~ "))
  
  temp.mod <- lm(mod.equation, data = sw1)
  
  fig.name <- paste0('figures/modsum_bycrop_', responses[i], '.pdf')
  
  pdf(fig.name, height = 8, width = 8)
  layout_matrix <- matrix(c(1:4), nrow=2, ncol=2, byrow=TRUE)
  layout(layout_matrix)
  par(mar = c(6,6,3,1), oma = c(0,0,0,0), pch = 16)
  
  ####
  plot(sw1[,responses[i]] ~ temp.mod$fitted.values,
       xlab = "Fitted Values",
       ylab = "Observed Values", 
       main = paste('log10', responses_clean[i]), col = sw1$period_crop)
  
  abline(0,1)
  
  text(x = min(temp.mod$fitted.values) + 0.2, y = max(sw1[,responses[i]])-0.2, 
       labels = paste0('R2 = ', round(summary(temp.mod)$adj.r.squared, 2)), 
       col = 'blue', pos = 4)
  ####
  boxplot(temp.mod$residuals ~ sw1$period_crop, 
          ylab = 'Residuals', col = c('darkgray', 'red', 'green'))
  
  ###
  plot(temp.mod$residuals ~ temp.mod$fitted.values, 
       xlab = "Fitted Values", 
       ylab = "Residuals", col = sw1$period_crop)
  abline(h = 0)
  ## #
  plot(temp.mod$residuals ~ as.Date(sw1$storm_start), col = sw1$period_crop,
       xlab = 'Year', ylab = 'Residuals')
  abline(h = 0, lwd = 2)
  dev.off()
}

# fit each model to the "pre" data, and then
# predict concentrations/loads from the storm characteristics of the 
# post data - then calculate percent decrease from predicted post events
# using the pre model and observed events
sw1.pre.df <- filter(sw1, period == "before")
sw1.post.df <- filter(sw1, period == "after")

# calculate percent change using both methods --
# kitchen sink and the lasso
# also could potentially use something else like random forest
# that is better than prediction
library(randomForest) # need to install
percent.change <- c()
for (i in 1:length(responses)) {
  
  mod.equation <- as.formula(paste(responses[i], paste(pred.keep, collapse = " + "), sep = " ~ "))
  # now run through lm model
  temp.mod.pre <- lm(mod.equation, data = sw1.pre.df)
  temp.mod.post <- lm(mod.equation, data = sw1.post.df)
  
  post.pred <- predict(temp.mod.post, sw1)
  pre.pred <- predict(temp.mod.pre, sw1)
  
  percent.change[i] <- median(((post.pred - pre.pred)/post.pred)*100)
  
}


