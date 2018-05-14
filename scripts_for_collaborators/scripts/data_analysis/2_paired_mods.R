# loop through each response var
# uses vars set up in 1_mds_paired.R
dat <- wq
out <- data.frame(matrix(nrow = length(trt_vars), ncol = 7))
names(out) <- c('response', 'rsq', 'pval_period_int', 'intercept', 'slope', 
                'int_period_before', 'slope_period_before')
for (i in 1:length(trt_vars)) {
  
  temp_trt <-  trt_vars[i]
  temp_con <- gsub(test_site, control_site, temp_trt)
  
  # log transform responses
  temp_dat <- data.frame(trt = log10(dat[,temp_trt]), 
                         con = log10(dat[,temp_con]), 
                         period = dat[, 'period'], 
                         datetime = as.POSIXct(dat[,'con_storm_start']))
  temp_dat <- mutate(temp_dat, diff = trt-con)
  
  before_mean_diff <- mean(temp_dat$diff[temp_dat$period == 'before'])
  
  temp_dat$stand_diff <- temp_dat$diff-before_mean_diff
  
  # create linear model
  mod_temp = lm(trt ~ con*period, data=temp_dat)
  mod_anova <- anova(mod_temp)
  pval_period <- round(mod_anova$`Pr(>F)`[3], 2)
  rsq <- round(summary(mod_temp)$r.squared, 2)
  
  out[i, 1] <- clean_names[i]
  out[i, 2:7] <- c(rsq, pval_period, as.numeric(round(summary(mod_temp)$coefficients[,1], 3)))
  
  p.label <- ifelse(pval_period <= 0.05, paste0("Sig. difference after BMP implementation (p = ", pval_period, ")"),
                    paste0("No sig. difference after BMP \nimplementation (p = ", pval_period, ")"))
                                        
  rsq_label <- paste("R^2 == ", rsq)
  # make plot of interactions with data points
  p <- interact_plot(mod_temp, pred = con, modx = period, plot.points = T) +
    scale_color_manual(values = c('before' = 'black', 'after' = 'red')) +
    scale_fill_manual(values = c('before' =  'black', 'after' = 'red')) +
    scale_linetype_manual(values = c(1,1)) +
    labs(x = paste0('Control site - log ', clean_names[i]), y = paste0('Test site - log ', clean_names[i])) +
    annotate('text', x = -Inf, y = Inf, label = rsq_label, vjust = 1.1, hjust = -0.1, parse = T) +
    annotate('text', x = -Inf, y = Inf, label = p.label, vjust = 2, hjust = -0.04)
  
  # save figure 
  temp_filename <- file.path('figures', paste0(site,'_', site_paired, '_regression_lines.png'))
  ggsave(temp_filename, p, height = 4, width = 6)
  
  # create a difference plot
  p2 <- ggplot(temp_dat, aes(x = datetime, y = stand_diff)) +
    geom_point(aes(color = period)) +
    theme_bw() +
    scale_color_manual(values = c('black', 'red'), labels = c('before', 'after')) +
    labs(x = '', y = 'Treatment - Control \n(standardized to calibration period)')
  
}

##############################
# calculate percent change
for (i in 1:length(trt_vars)) {
  
  if (out$pval_period_int[i] > 0.05) {
    
  } else {
    
  
  temp_trt <-  trt_vars[i]
  temp_con <- gsub(test_site, control_site, temp_trt)
  
  # log transform responses
  temp_dat <- data.frame(trt = log10(dat[,temp_trt]), 
                         con = log10(dat[,temp_con]), 
                         period = dat[, 'period'])
  
  temp_after <- filter(temp_dat, period == "after")
  
  pred_before <- temp_after
  pred_before$period <- 'before'
  
  pred_after <- temp_dat
  pred_before$period <- 'after'
  
  predict_baseline <- predict(mod_temp, pred_before)
  predict_treatment <- predict(mod_temp, pred_after)
  
  plot(temp_after$trt ~ temp_after$con, pch = 16)
  points(predict_baseline ~ temp_after$con, pch = 16, col = 'red')
  # create linear model
  mod_before <- lm(trt ~ con, data=temp_before)
  mod_after <-  lm(trt ~ con, data=temp_after)
  
  mod_anova <- anova(mod_temp)
  pval_period <- round(mod_anova$`Pr(>F)`[3], 2)
  rsq <- round(summary(mod_temp)$r.squared, 2)
  
  out[i, 1] <- clean_names[i]
  out[i, 2:7] <- c(rsq, pval_period, as.numeric(round(summary(mod_temp)$coefficients[,1], 3)))
  
  p.label <- ifelse(pval_period <= 0.05, paste0("Sig. difference after BMP implementation (p = ", pval_period, ")"),
                    paste0("No sig. difference after BMP \nimplementation (p = ", pval_period, ")"))
  
  rsq_label <- paste("R^2 == ", rsq)
  # make plot of interactions with data points
  p <- interact_plot(mod_temp, pred = con, modx = period, plot.points = T) +
    scale_color_manual(values = c('before' = 'black', 'after' = 'red')) +
    scale_fill_manual(values = c('before' =  'black', 'after' = 'red')) +
    scale_linetype_manual(values = c(1,1)) +
    labs(x = paste0('Control site - log ', clean_names[i]), y = paste0('Test site - log ', clean_names[i])) +
    annotate('text', x = -Inf, y = Inf, label = rsq_label, vjust = 1.1, hjust = -0.1, parse = T) +
    annotate('text', x = -Inf, y = Inf, label = p.label, vjust = 2, hjust = -0.04)
  
  # save figure 
  temp_filename <- file.path('figures', paste0(site,'_', site_paired, '_regression_lines.png'))
  ggsave(temp_filename, p, height = 4, width = 6)
  
}
}
temp_filename <- file.path('data_cached', paste0(site,'_', site_paired, '_mod_outputs.csv'))
write.csv(out, temp_filename, row.names = F)