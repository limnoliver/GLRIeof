# read in data that is prepped for analysis
temp.filename <- paste0(site, '_mod_dat.csv')
dat <- read.csv(file.path('data_cached', temp.filename))

# define predictors
dat.mod <- dat[,mod_dat_env$predictors]
dat.mod <- complete.cases(dat.mod)

n.lost <- length(dat.mod[dat.mod == FALSE])
message(paste0(n.lost, ' observations dropped due to missing predictor data.'))


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
    
    perc.var[i] <- round(mod$rsq[500]*100, 1)
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