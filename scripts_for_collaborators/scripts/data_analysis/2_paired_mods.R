# loop through each response var
# uses vars set up in 1_mds_paired.R
dat <- wq
out <- data.frame(matrix(nrow = length(trt_vars), ncol = 10))
names(out) <- c('response', 'rsq', 'pval_period_int', 'pval_period_slope', 'intercept', 'slope', 
                'int_period_before', 'slope_period_before', 'mean_event_pct_change', 'total_pct_change')
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
  pval_period_slope <- round(mod_anova$`Pr(>F)`[3], 2)
  pval_period_int <- round(mod_anova$`Pr(>F)`[2], 2)
  rsq <- round(summary(mod_temp)$r.squared, 2)
  
  out[i, 1] <- clean_names[i]
  out[i, 2:8] <- c(rsq, pval_period_int, pval_period_slope, as.numeric(round(summary(mod_temp)$coefficients[,1], 3)))
  
  p.label <- ifelse(pval_period_int <= 0.05, paste0("Sig. difference after BMP implementation (p = ", pval_period_int, ")"),
                    paste0("No sig. difference after BMP \nimplementation (p = ", pval_period_int, ")"))
                                        
  rsq_label <- paste("R^2 == ", rsq)
  
  # make plot of interactions with data points
  p <- interact_plot(mod_temp, pred = con, modx = period, plot.points = T) +
    scale_color_manual(values = c('before' = 'black', 'after' = 'red'), guide = T) +
    scale_fill_manual(values = c('before' =  'black', 'after' = 'red'), guide = F) +
    scale_linetype_manual(values = c(1,1), guide = F) +
    labs(x = paste0('Control site - log ', clean_names[i]), y = paste0('Test site - log ', clean_names[i])) +
    annotate('text', x = -Inf, y = Inf, label = rsq_label, vjust = 1.1, hjust = -0.1, parse = T) +
    annotate('text', x = -Inf, y = Inf, label = p.label, vjust = 2, hjust = -0.04) +
    guides(color=guide_legend(reverse=TRUE))
  
  # save figure
  temp_response <- gsub(paste0(test_site, '_'), '', trt_vars[i])
  temp_filename <- file.path('figures', paste0(temp_response, '_', site,'_', site_paired, '_regression_lines.png'))
  ggsave(temp_filename, p, height = 4, width = 6)
  
  # create a difference plot
  p2 <- ggplot(temp_dat, aes(x = datetime, y = stand_diff)) +
    geom_point(aes(color = period)) +
    theme_bw() +
    scale_color_manual(values = c('black', 'red'), labels = c('before', 'after')) +
    labs(x = '', y = 'Treatment - Control \n(standardized to calibration period)')
  
  # save figure 
  temp_filename <- file.path('figures', paste0(temp_response, '_', site,'_', site_paired, '_diff_throughtime.png'))
  ggsave(temp_filename, p2, height = 4, width = 6)
  
  
  if (out$pval_period_int[i] > 0.05) {
    
    out$mean_event_pct_change[i] <- NA
    out$total_pct_change[i] <- NA
    
  } else {
  
    # reduce to just "after" data
    temp_after <- filter(temp_dat, period == "after")
    
    # create two datasets - where period is either before or after for prediction using
    # the interaction model
    pred_before <- temp_after
    pred_before$period <- 'before'
    
    pred_after <- temp_after
    pred_after$period <- 'after'
    
    # predict the post events based on both fitted lines
    predict_baseline <- predict(mod_temp, pred_before)
    predict_treatment <- predict(mod_temp, pred_after)
    
    temp_after$pred_baseline <- as.numeric(predict_baseline)
    temp_after$pred_treatment <- as.numeric(predict_treatment)
    
    # calculate the difference and percent difference
    temp_after$pred_diff <- temp_after$pred_treatment - temp_after$pred_baseline
    temp_after$pred_diff_perc <- (temp_after$pred_diff/temp_after$pred_baseline)*100
    
    # calculate percent reduction in two ways -- 1) mean of the event-level
    # percent differences 2) percent difference of sum of loads
    
    out$mean_event_pct_change[i] <- mean(temp_after$pred_diff_perc)
    out$total_pct_change[i] <- ((sum(temp_after$pred_treatment) - sum(temp_after$pred_baseline))/sum(temp_after$pred_baseline))*100
  }  
}

temp_filename <- file.path('data_cached', paste0(site,'_', site_paired, '_mod_outputs.csv'))
write.csv(out, temp_filename, row.names = F)
