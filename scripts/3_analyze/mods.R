# read in libraries
#library(GSqwsr)
library(randomForest)
library(dplyr)
library(caret)
library(ggplot2)
library(glmnet)

# this script creates a linear model between water quality and hydrologic variables, 
# including variables output by Rainmaker, as well as storm characteristics
source('scripts/2_process/process_merged_data.R')

############################
# transform response variables
sw1[,responses] <- log10(sw1[,responses])

# get rid of highly correlated variables

predictors.cor <- cor(sw1[,predictors[-length(predictors)]], use = 'complete.obs') # drop var "crop" from correlation since it's a categorical var
names.cor <- row.names(predictors.cor)
drop.predictors <- caret::findCorrelation(predictors.cor, cutoff = 0.95, verbose = FALSE, exact = TRUE)

predictors.keep <- c(names.cor[-drop.predictors], 'frozen', 'sin_sdate', 'cos_sdate')

##############################
# approach #1 
# non-linear - random forest model

sw1.mod <- sw1[,predictors.keep]
sw1.mod <- complete.cases(sw1.mod)
sw1.mod <- sw1[sw1.mod, ]

# loop through responses to create equation and model
for (i in 1:length(responses)) {

  mod.equation <- as.formula(paste(responses[i], paste(predictors.keep, collapse = " + "), sep = " ~ "))
  
  mod <- randomForest(mod.equation, data = sw1.mod, importance = T, na.action = na.omit)
  resid <- sw1.mod[, responses[i]] - mod$predicted
  resid.test <- data.frame(resids = resid, 
                           period = sw1.mod$period_crop)
  diff.test <- lm(resid.test$resids ~ resid.test$period)
  diff.test.result <- anova(diff.test)
  pval <- diff.test.result$`Pr(>F)`[1]
  test.text <- ifelse(pval > 0.05, "No sig. differences between groups", "Sig. differences between groups")
  
  top.vars <- pdp::topPredictors(mod, n = 4)
  
  pdf(paste0('figures/', 'rf_pp_', responses[i], '.pdf'), heigh = 6, width = 6)
  par(mfcol = c(2,2), mar = c(4,2,2,2), oma = c(2,2,3,0))
  for (n in top.vars){
    partialPlot(mod, pred.data = sw1.mod, x.var = paste(n),
                xlab = n, main = "")
  }
  mtext(paste0("Partial Dependence plots - ", responses_clean[i]), side = 3, outer = T)
  dev.off()
  
  ##########
  # now create 4 plots
  # 1-obs vs pred, 2-residual boxplot, 3-residual~fitted, 4-resid~date
  fig.name = paste0('figures/rf_modsum_', responses[i], '.pdf')
  pdf(fig.name, height = 8, width = 8)
  layout_matrix <- matrix(c(1:4), nrow=2, ncol=2, byrow=TRUE)
  layout(layout_matrix)
  par(mar = c(6,6,3,1), oma = c(0,0,0,0), pch = 16)
  
  ####
  plot(mod$y ~ mod$predicted,
       xlab = "Fitted Values",
       ylab = "Observed Values", 
       main = paste('log10', responses_clean[i]), col = sw1$period_crop)
  
  abline(0,1)
  
  text(x = min(mod$predicted) + 0.2, y = max(mod$y)-0.2, 
       labels = paste0('% Var Exp = ', round(mod$rsq[500]*100, 1)), 
       col = 'blue', pos = 4)
  ####
  boxplot(resid ~ sw1.mod$period_crop, 
          ylab = 'Residuals', col = c('darkgray', 'red', 'green'),
          ylim = c(min(resid), max(resid)*1.3))
  text(x = 2, y = max(resid)*1.1, labels = test.text, col = 'blue', adj=c(0.5, 0))
  
  ###
  plot(resid ~ mod$predicted, 
       xlab = "Fitted Values", 
       ylab = "Residuals", col = sw1$period_crop)
  abline(h = 0)
  ## #
  plot(resid ~ as.Date(sw1.mod$storm_start), col = sw1$period_crop,
       xlab = 'Year', ylab = 'Residuals')
  abline(h = 0, lwd = 2)
  dev.off()
}

# now split data up into before and after, 
# and fit RF models. Then run all events through 
# both models.
sw1.mod.before <- filter(sw1.mod, period == 'before')
sw1.mod.after <- filter(sw1.mod, period == 'after')

before.fit <- c()
after.fit <- c()
mean.diff <- c()
mean.diff.sd <- c()
mean.diff.frozen <- c()
mean.diff.sd.frozen <- c()
mean.diff.nonfrozen <- c()
mean.diff.sd.nonfrozen <- c()

for (i in 1:length(responses)) {
  
  mod.equation <- as.formula(paste(responses[i], paste(predictors.keep, collapse = " + "), sep = " ~ "))
  
  mod.before <- randomForest(mod.equation, data = sw1.mod.before, importance = T, na.action = na.omit)
  mod.after <- randomForest(mod.equation, data = sw1.mod.after, importance = T, na.action = na.omit)
  
  pred.before <- predict(mod.before, sw1.mod)
  pred.after <- predict(mod.after, sw1.mod)
  
  # output model fit stats
  before.fit[i] <- round(mod.before$rsq[500]*100, 1)
  after.fit[i] <- round(mod.after$rsq[500]*100, 1)
  
  diff <- (pred.before - pred.after)/pred.before
  diff.frozen <- (pred.before[sw1.mod$frozen == TRUE] - pred.after[sw1.mod$frozen == TRUE])/pred.before[sw1.mod$frozen == TRUE]
  diff.nonfrozen <- (pred.before[sw1.mod$frozen == FALSE] - pred.after[sw1.mod$frozen == FALSE])/pred.before[sw1.mod$frozen == FALSE]
  
  mean.diff[i] <- mean(diff)
  mean.diff.sd[i] <- sd(diff)
  mean.diff.frozen[i] <- mean(diff.frozen)
  mean.diff.sd.frozen[i] <- sd(diff.frozen)
  mean.diff.nonfrozen[i] <- mean(diff.nonfrozen)
  mean.diff.sd.nonfrozen[i] <- sd(diff.nonfrozen)
}

########################
# transform data


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


