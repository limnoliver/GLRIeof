# read in libraries
#library(GSqwsr)
library(randomForest)
library(dplyr)
library(caret)
library(ggplot2)
#library(glmnet)

# this script creates a linear model between water quality and hydrologic variables, 
# including variables output by Rainmaker, as well as storm characteristics
site <- 'sw3'
temp_source <- file.path('scripts', '2_process', paste0('process_merged_data_', site, '.R'))
source(temp_source)
#sw1 <- read.csv('data_cached/sw1_mod_dat.csv')
############################
# transform response variables
dat[,responses] <- log10(dat[,responses])

# get rid of highly correlated variables

predictors.cor <- cor(dat[,predictors[-length(predictors)]], use = 'complete.obs') # drop var "crop" from correlation since it's a categorical var
names.cor <- row.names(predictors.cor)
drop.predictors <- caret::findCorrelation(predictors.cor, cutoff = 0.95, verbose = FALSE, exact = TRUE)

predictors.keep <- c(names.cor[-drop.predictors], 'frozen')

##############################
# approach #1 
# non-linear - random forest model

dat.mod <- dat[,predictors.keep]
dat.mod <- complete.cases(dat.mod)
dat.mod <- dat[dat.mod, ]
dat.mod.before <- filter(dat.mod, period == 'before')
dat.mod.after <- filter(dat.mod, period == 'after')

# save MDC as output from loop
if (site == 'sw1') {
  mdc.perc.corn <- c()
  mdc.perc.all <- c()
  mdc.perc.alfalfa <- c()
  pval.differences <- c()
  pval.after <- c()
  pval.corn <- c()
  pval.alfalfa <- c()
  perc.var <- c()
  
  # loop through responses to create equation and model
  for (i in 1:length(responses)) {
    
    mod.equation <- as.formula(paste(responses[i], paste(predictors.keep, collapse = " + "), sep = " ~ "))
    
    mod <- randomForest(mod.equation, data = sw1.mod, importance = T, na.action = na.omit)
    mod.before <- randomForest(mod.equation, data = sw1.mod.before, importance = T, na.action = na.omit, ntree = 1000)
    
    perc.var[i] <- round(mod$rsq[1000]*100, 1)
    # calculate minimum detectable change for each constituent based on this model
    mse.before <- mod.before$mse[length(mod.before$mse)]
    
    n.before <- nrow(sw1.mod.before[sw1.mod.before$period_crop == "before", ])
    n.after <- nrow(sw1.mod.after[sw1.mod.after$period == "after",])
    n.after.corn <- nrow(sw1.mod.after[sw1.mod.after$period_crop == "after (corn)",])
    n.after.alfalfa <- nrow(sw1.mod.after[sw1.mod.after$period_crop == "after (alfalfa)",])
    
    tval.corn <- qt(0.05, n.before + n.after.corn - 2, lower.tail = FALSE)
    tval.all <- qt(0.05, n.before + n.after -2, lower.tail = FALSE)
    tval.alfalfa <- qt(0.05, n.before + n.after.alfalfa -2, lower.tail = FALSE)
    
    mdc.corn <- tval.corn*sqrt((mse.before/n.before) + (mse.before/n.after.corn))
    mdc.all <- tval.all*sqrt((mse.before/n.before) + (mse.before/n.after))
    mdc.alfalfa <- tval.alfalfa*sqrt((mse.before/n.before) + (mse.before/n.after.alfalfa))
    mdc.perc.corn[i] <- (1-(10^-mdc.corn))*100
    mdc.perc.all[i] <- (1-(10^-mdc.all))*100
    mdc.perc.alfalfa[i] <- (1-(10^-mdc.alfalfa))*100
    
    
    resid <- sw1.mod[, responses[i]] - mod$predicted
    resid.test <- data.frame(resids = resid, 
                             period = sw1.mod$period_crop)
    resid.test.after <- data.frame(resids = resid[sw1$period == 'after'],
                                   period = 'after')
    resid.test.all <- bind_rows(resid.test, resid.test.after)
    
    diff.test <- lm(resid.test.all$resids ~ resid.test.all$period)
    diff.test.result <- anova(diff.test)
    pval <- diff.test.result$`Pr(>F)`[1]
    pval.differences[i] <- pval
    test.text <- ifelse(pval > 0.05, "No sig. differences between groups", "")
    
    # pairwise tests for denoting which groups are different
    resid.test.all$period <- factor(resid.test.all$period, levels = c('before', 'after', 'after (corn)', 'after (alfalfa)'))
    pair.test <- pairwise.t.test(resid.test.all$resids, resid.test.all$period, alternative = 'less')
    pval.after[i] <- pair.test$p.value[1,1]
    pval.corn[i] <- pair.test$p.value[2,1]
    pval.alfalfa[i] <- pair.test$p.value[3,1]
    
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
    pdf(fig.name, height = 10, width = 10)
    layout_matrix <- matrix(c(1:4), nrow=2, ncol=2, byrow=TRUE)
    layout(layout_matrix)
    par(mar = c(5,5,3,1), oma = c(0,0,0,0), pch = 16)
    
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
    temp <- boxplot(resid.test.all$resids ~ resid.test.all$period, 
                    ylab = 'Residuals', col = c('darkgray', 'lightgray', 'red', 'green'),
                    ylim = c(min(resid), max(resid)*1.3), main = test.text)
    
    text(x = 2, y = max(resid)*1.2, labels = paste0("MDC = ", round(mdc.perc.all[i],0), "%"), adj=c(0.5, 0))
    text(x = 3, y = max(resid)*1.2, labels = paste0("MDC = ", round(mdc.perc.corn[i],0), "%"), adj=c(0.5, 0))
    text(x = 4, y = max(resid)*1.2, labels = paste0("MDC = ", round(mdc.perc.alfalfa[i],0), "%"), adj=c(0.5, 0))
    
    if (pval < 0.05) {
      if (pval.after[i] < 0.05) {
        text(x = 2, y = temp$stats[5,2]*1.1, labels = "*", adj=c(0.5, .5), cex = 3)
        
      }
      if (pval.corn[i] < 0.05) {
        text(x = 3, y = temp$stats[5,3]*1.1, labels = "*", adj=c(0.5, .5), cex = 3)
      }
      if (pval.alfalfa[i] < 0.05) {
        text(x = 4, y = temp$stats[5,4]*1.1, labels = "*", adj=c(0.5, .5), cex = 3)
      }
    }
    
    
    
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
  
  # create a dataframe describing the residual models
  before_after_resid <- data.frame(variable = responses_clean,
                                   perc_var = perc.var,
                                   mdc_all = round(mdc.perc.all, 0),
                                   mdc_corn = round(mdc.perc.corn, 0),
                                   mdc_alfalfa = round(mdc.perc.alfalfa, 0),
                                   pval_groups = round(pval.differences, 2),
                                   pval_after = round(pval.after, 2),
                                   pval_corn = round(pval.corn, 2),
                                   pval_alfalfa = round(pval.alfalfa, 2))
  
  write.csv(before_after_resid, 'data_cached/residual_results.csv')
}

if (site == 'sw3') {
  mdc.perc.nbefore <- c()
  mdc.perc.nafter <- c()
  pval.differences <- c()
  perc.var <- c()
  
  # loop through responses to create equation and model
  for (i in 1:length(responses)) {
    
    mod.equation <- as.formula(paste(responses[i], paste(predictors.keep, collapse = " + "), sep = " ~ "))
    
    mod <- randomForest(mod.equation, data = dat.mod, importance = T, na.action = na.omit)
    mod.before <- randomForest(mod.equation, data = dat.mod.before, importance = T, na.action = na.omit, ntree = 1000)
    
    perc.var[i] <- round(mod$rsq[500]*100, 1)
    # calculate minimum detectable change for each constituent based on this model
    mse.before <- mod.before$mse[length(mod.before$mse)]
    
    n.before <- nrow(dat.mod.before[dat.mod.before$period == "before", ])
    n.after <- nrow(dat.mod.after[dat.mod.after$period == "after",])
    
    tval.nbefore <- qt(0.05, n.before + n.before - 2, lower.tail = FALSE)
    tval.nafter <- qt(0.05, n.before + n.after -2, lower.tail = FALSE)

    mdc.nbefore <- tval.nbefore*sqrt((mse.before/n.before) + (mse.before/n.before))
    mdc.nafter <- tval.nafter*sqrt((mse.before/n.before) + (mse.before/n.after))
    
    mdc.perc.nbefore[i] <- (1-(10^-mdc.nbefore))*100
    mdc.perc.nafter[i] <- (1-(10^-mdc.nafter))*100

    
    resid <- dat.mod[, responses[i]] - mod$predicted
    resid.test <- data.frame(resids = resid, 
                             period = dat.mod$period)
    
    resid.test.after <- data.frame(resids = resid[dat$period == 'after'],
                                   period = 'after')
    resid.test.all <- bind_rows(resid.test, resid.test.after)
    
    diff.test <- t.test(resid.test.all$resids~resid.test.all$period,alternative = 'less')
 
    pval <- diff.test$p.value
    pval.differences[i] <- pval
    
    test.text <- ifelse(pval > 0.05, "No sig. differences between groups", "")
    
    top.vars <- pdp::topPredictors(mod, n = 4)
    
    pdf(paste0('figures/', site, '_rf_pp_', responses[i], '.pdf'), heigh = 6, width = 6)
    par(mfcol = c(2,2), mar = c(4,2,2,2), oma = c(2,2,3,0))
    for (n in top.vars){
      partialPlot(mod, pred.data = dat.mod, x.var = paste(n),
                  xlab = n, main = "")
    }
    mtext(paste0("Partial Dependence plots - ", responses_clean[i]), side = 3, outer = T)
    dev.off()
    
    # change order of levels
    resid.test.all$period <- factor(resid.test.all$period, levels = c('before', 'after'))
    ##########
    # now create 4 plots
    # 1-obs vs pred, 2-residual boxplot, 3-residual~fitted, 4-resid~date
    fig.name = paste0('figures/', site, '_rf_modsum_', responses[i], '.pdf')
    pdf(fig.name, height = 10, width = 10)
    layout_matrix <- matrix(c(1:4), nrow=2, ncol=2, byrow=TRUE)
    layout(layout_matrix)
    par(mar = c(5,5,3,1), oma = c(0,0,0,0), pch = 16)
    
    ####
    plot(mod$y ~ mod$predicted,
         xlab = "Fitted Values",
         ylab = "Observed Values", 
         main = paste('log10', responses_clean[i]), col = as.factor(dat$period))
    
    abline(0,1)
    
    text(x = min(mod$predicted) + 0.2, y = max(mod$y)-0.2, 
         labels = paste0('% Var Exp = ', round(mod$rsq[500]*100, 1)), 
         col = 'blue', pos = 4)
    ####
    temp <- boxplot(resid.test.all$resids ~ resid.test.all$period, 
                    ylab = 'Residuals', col = c('darkgray', 'red'),
                    ylim = c(min(resid), max(resid)*1.3), main = test.text)
    
    text(x = 2, y = max(resid)*1.2, labels = paste0("MDC = ", round(mdc.perc.nafter[i],0), "%"), adj=c(0.5, 0))

    if (pval < 0.05) {
        text(x = 2, y = temp$stats[5,2]*1.1, labels = "*", adj=c(0.5, .5), cex = 3)
    }
    
    ###
    plot(resid ~ mod$predicted, 
         xlab = "Fitted Values", 
         ylab = "Residuals", col = as.factor(dat$period))
    abline(h = 0)
    ## #
    plot(resid ~ as.Date(dat.mod$storm_start), col = as.factor(dat$period),
         xlab = 'Year', ylab = 'Residuals')
    abline(h = 0, lwd = 2)
    dev.off()
  }
  
  # create a dataframe describing the residual models
  before_after_resid <- data.frame(variable = responses_clean,
                                   perc_var = perc.var,
                                   mdc_nbefore = round(mdc.perc.nbefore, 0),
                                   mdc_nafter = round(mdc.perc.nafter, 0),
                                   pvals = round(pval.differences, 2))
  
  write.csv(before_after_resid, 'data_cached/residual_results.csv')
}

# now split data up into before and after, 
# and fit RF models. Then run all events through 
# both models.
predictors.keep <- c(predictors.keep, "peak_discharge")
sw1.mod <- sw1[,predictors.keep]
sw1.mod <- complete.cases(sw1.mod)
sw1.mod <- sw1[sw1.mod, ]
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
median.diff <- c()
diff.sum <- c()
five.diff <- c()
ninetyfive.diff <- c()
pvals.ba <- c()
load.before <- c()
load.after <- c()


for (i in 1:(length(responses)-1)) {
  
  if (pval.differences[i] > 0.05) {
    before.fit[i] <- NA
    after.fit[i] <- NA
    pvals.ba[i] <- NA
    mean.diff[i] <- NA
    median.diff[i] <- NA
    diff.sum[i] <- NA
    five.diff[i] <- NA
    ninetyfive.diff[i] <- NA
    mean.diff.sd[i] <- NA
    mean.diff.frozen[i] <- NA
    mean.diff.sd.frozen[i] <- NA
    mean.diff.nonfrozen[i] <- NA
    mean.diff.sd.nonfrozen[i] <- NA
    load.before[i] <- NA
    load.after[i] <- NA
    next}

  mod.equation <- as.formula(paste(responses[i], paste(predictors.keep, collapse = " + "), sep = " ~ "))
  
  mod.before <- randomForest(mod.equation, data = sw1.mod.before, importance = T, na.action = na.omit, ntree = 1000)
  mod.after <- randomForest(mod.equation, data = sw1.mod.after, importance = T, na.action = na.omit, ntree = 1000)
  
  # get residuals from before model for MDC calc
  resid.before <- sw1.mod.before[, responses[i]] - mod.before$predicted
  
  pred.before <- predict(mod.before, sw1.mod)
  pred.after <- predict(mod.after, sw1.mod)
  
  
  # output model fit stats
  before.fit[i] <- round(mod.before$rsq[1000]*100, 1)
  after.fit[i] <- round(mod.after$rsq[1000]*100, 1)
  
  diff <- (10^pred.before - 10^pred.after)/10^pred.before
  diff.sum[i] <- (sum(10^pred.before) - sum(10^pred.after))/sum(10^pred.before)
  load.before[i] <- sum(10^pred.before)
  load.after[i] <- sum(10^pred.after)
  
  # test if these percent differences are different from zero
  change.test <- t.test(diff, alternative = 'greater')
  pvals.ba[i] <- round(change.test$p.value, 3)
  
  
  diff.frozen <- (pred.before[sw1.mod$frozen == TRUE] - pred.after[sw1.mod$frozen == TRUE])/pred.before[sw1.mod$frozen == TRUE]
  diff.nonfrozen <- (pred.before[sw1.mod$frozen == FALSE] - pred.after[sw1.mod$frozen == FALSE])/pred.before[sw1.mod$frozen == FALSE]
  
  mean.diff[i] <- mean(diff)
  median.diff[i] <- median(diff)
  five.diff[i] <- quantile(diff, 0.05)
  ninetyfive.diff[i] <- quantile(diff, 0.95)
  mean.diff.sd[i] <- sd(diff)
  mean.diff.frozen[i] <- mean(diff.frozen)
  mean.diff.sd.frozen[i] <- sd(diff.frozen)
  mean.diff.nonfrozen[i] <- mean(diff.nonfrozen)
  mean.diff.sd.nonfrozen[i] <- sd(diff.nonfrozen)
  
  
}

# create data frame of values
perc_reduction <- data.frame(response = responses[-length(responses)],
                             response_clean = responses_clean[-length(responses)],
                             before_r2 = before.fit,
                             after_r2 = after.fit,
                             perc_diff = round(mean.diff*100, 1),
                             sd_perc_diff = round(mean.diff.sd*100,1),
                             median_diff = round(median.diff*100, 1),
                             fifth_diff = round(five.diff*100, 1),
                             ninetyfifth_diff = round(ninetyfive.diff*100, 1),
                             diff_sum = round(diff.sum*100, 1),
                             load_before = round(load.before, 0),
                             load_after = round(load.after, 0),
                             pval = pvals.ba,
                             perc_diff_frozen = round(mean.diff.frozen*100, 1),
                             sd_perc_diff_frozen = round(mean.diff.sd.frozen*100,1),
                             perc_diff_nonfrozen = round(mean.diff.nonfrozen*100, 1),
                             sd_perc_diff_nonfrozen = round(mean.diff.sd.nonfrozen*100, 1))

write.csv(perc_reduction, "data_cached/percent_reduction_before_after.csv", row.names = F)


#####################################################
## Full model to evaluate top predictors
####################################################
predictors.keep <- c(predictors.keep, "peak_discharge", 'days_since_planting', 'days_since_fertilizer', 'crop')
sw1.mod <- sw1[,predictors.keep]
sw1.mod <- complete.cases(sw1.mod)
sw1.mod <- sw1[sw1.mod, ]

fit <- c()
ranks <- data.frame(matrix(ncol = length(responses)-1, nrow = length(predictors.keep)))

for (i in 1:(length(responses)-1)) {
  
  mod.equation <- as.formula(paste(responses[i], paste(predictors.keep, collapse = " + "), sep = " ~ "))
  
  mod <- randomForest(mod.equation, data = sw1.mod, importance = T, na.action = na.omit, ntrees = 1000)

  # output model fit stats
  fit[i] <- round(mod$rsq[500]*100, 1)
  
  varimp <- as.data.frame(mod$importance)
  varimp$variable <- row.names(varimp)
  varimp$rank <- rank(-varimp$`%IncMSE`)
  ranks[,i] <- varimp$rank
  
  
  top.vars <- pdp::topPredictors(mod, n = 4)
  
  pdf(paste0('figures/', 'rf_fullmod_pp_', responses[i], '.pdf'), heigh = 6, width = 6)
  par(mfcol = c(2,2), mar = c(4,2,2,2), oma = c(2,2,3,0))
  for (n in top.vars){
    partialPlot(mod, pred.data = sw1.mod, x.var = paste(n),
                xlab = n, main = "")
  }
  mtext(paste0("Partial Dependence plots - ", responses_clean[i]), side = 3, outer = T)
  dev.off()
  
}

names(ranks) <- responses_clean[-length(responses_clean)]
ranks$predictor <- predictors.keep

ranks$mean_rank <- rowMeans(ranks[,1:18])



ranks.long <- ranks %>%
  gather(key = variable, value = rank, -predictor)

rank.order <- group_by(ranks.long, predictor) %>%
  summarize(median_rank = median(rank)) %>%
  arrange(median_rank)

ranks.long$predictor <- factor(ranks.long$predictor, levels = rank.order$predictor)


  
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


