# loop through each response var
# uses vars set up in 1_mds_paired.R
dat <- wq
for (i in 1:length(trt_vars)) {
  
  temp_trt <-  trt_vars[i]
  temp_con <- gsub(test_site, control_site, temp_trt)
  
  # log transform responses
  temp_dat <- data.frame(trt = log10(dat[,temp_trt]), 
                         con = log10(dat[,temp_con]), 
                         period = dat[, 'period'])
  
  # create linear model
  mod_temp = lm(trt ~ con*period, data=temp_dat)
  mod_anova <- anova(mod_temp)
  
  # make plot of interactions with data points
  interact_plot(mod_temp, pred = con, modx = period, plot.points = T) +
    scale_color_manual(values = c('red','black')) +
    scale_fill_manual(values = c('red', 'black')) +
    scale_linetype_manual(values = c(1,1)) +
    labs(x = paste0('Control site - ', clean_names[i]), y = paste0('Test site - ', clean_names[i]))
}