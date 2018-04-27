# read in data that is prepped for analysis
temp.filename <- paste0(site, '_mod_dat.csv')
dat <- read.csv(file.path('data_cached', temp.filename))

# define predictors
predictors <- mod_dat_env$predictors
dat.mod <- dat[,predictors]
dat.mod <- complete.cases(dat.mod)

n.lost <- length(dat.mod[dat.mod == FALSE])
message(paste0(n.lost, ' observations dropped due to missing predictor data.'))

dat.mod <- dat[dat.mod, ]
dat.mod.before <- filter(dat.mod, period == 'before')
dat.mod.after <- filter(dat.mod, period == 'after')

# set responses
responses <- mod_dat_env$responses

# transform response variables
dat.mod[,responses] <- log10(dat.mod[,responses])

# get rid of highly correlated variables
which.frozen <- which(predictors %in% 'frozen')
predictors.cor <- cor(dat[,predictors[-which.frozen]], use = 'complete.obs') # drop var "crop" from correlation since it's a categorical var
names.cor <- row.names(predictors.cor)
drop.predictors <- caret::findCorrelation(predictors.cor, cutoff = 0.95, verbose = FALSE, exact = TRUE)

predictors.keep <- c(names.cor[-drop.predictors], 'frozen')

# save MDC as output from loop
mdc.perc.nbefore <- c()
mdc.perc.nafter <- c()
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