dat.mod.before <- filter(dat.mod, period == 'before')
dat.mod.after <- filter(dat.mod, period == 'after')

# save MDC as output from loop

mdc.perc.nbefore <- c()
mdc.perc.nafter <- c()
pval.differences <- c()
perc.var <- c()

# loop through responses to create equation and model
for (i in 1:length(responses)) {
  
  mod.equation <- as.formula(paste(responses[i], paste(predictors.keep, collapse = " + "), sep = " ~ "))
  
  mod <- randomForest(mod.equation, data = dat.mod, importance = T, na.action = na.omit)
  #mod.before <- randomForest(mod.equation, data = dat.mod.before, importance = T, na.action = na.omit, ntree = 1000)
  
  perc.var[i] <- round(mod$rsq[500]*100, 1)
  
  resid <- dat.mod[, responses[i]] - mod$predicted
  resid.test <- data.frame(resids = resid, 
                           period = dat.mod$period)
  
  #resid.test.after <- data.frame(resids = resid[dat$period == 'after'],
  #                               period = 'after')
  #resid.test.all <- bind_rows(resid.test, resid.test.after)
  
  diff.test <- t.test(resid.test$resids~resid.test$period,alternative = 'less')
  
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
  mtext(paste0("Partial Dependence plots - ", clean_names[i]), side = 3, outer = T)
  dev.off()
  
  # change order of levels
  resid.test$period <- factor(resid.test$period, levels = c('before', 'after'))
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
       main = paste('log10', clean_names[i]), col = as.factor(resid.test$period))
  
  abline(0,1)
  
  text(x = min(mod$predicted) + 0.2, y = max(mod$y)-0.2, 
       labels = paste0('% Var Exp = ', round(mod$rsq[500]*100, 1)), 
       col = 'blue', pos = 4)
  ####
  temp <- boxplot(resid.test$resids ~ resid.test$period, 
                  ylab = 'Residuals', col = c('darkgray', 'red'),
                  ylim = c(min(resid), max(resid)*1.3), main = test.text)
  
  # add mdc after calculation back in for plots? 
  #text(x = 2, y = max(resid)*1.2, labels = paste0("MDC = ", round(mdc.perc.nafter[i],0), "%"), adj=c(0.5, 0))
  
  if (pval < 0.05) {
    text(x = 2, y = temp$stats[5,2]*1.1, labels = "*", adj=c(0.5, .5), cex = 3)
  }
  
  ###
  plot(resid ~ mod$predicted, 
       xlab = "Fitted Values", 
       ylab = "Residuals", col = as.factor(resid.test$period))
  abline(h = 0)
  ## #
  plot(resid ~ as.Date(dat.mod$storm_start), col = as.factor(resid.test$period),
       xlab = 'Year', ylab = 'Residuals')
  abline(h = 0, lwd = 2)
  dev.off()
}

# create a dataframe describing the residual models
before_after_resid <- data.frame(variable = clean_names,
                                 perc_var = perc.var,
                                 pvals = round(pval.differences, 2))