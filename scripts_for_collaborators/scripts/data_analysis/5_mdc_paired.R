#load paired data
paired_filename <- file.path("data_cached", paste0(site, "_", site_paired, "_prepped_WQbystorm.csv"))
paired <- read.csv(paired_filename, stringsAsFactors = F)

# vars to test
# get conc/load vars
if (length(loads) == 1 & !is.na(loads)){
  loadvars <- grep(loads, names(wq), ignore.case = TRUE, value = TRUE)
  trt_loadvars <- grep(test_site, loadvars, ignore.case = TRUE, value = TRUE)
  con_loadvars <- grep(control_site, loadvars, ignore.case = TRUE, value = TRUE)
} else if (is.na(loads)) {
  trt_loadvars <- NA
  con_loadvars <- NA
} else {
  loadvars <- loads
  trt_loadvars <- grep(test_site, loadvars, ignore.case = TRUE, value = TRUE)
  con_loadvars <- grep(control_site, loadvars, ignore.case = TRUE, value = TRUE)
}

if (length(concentrations) == 1 & !is.na(concentrations)){
  concvars <- grep(concentrations, names(wq), ignore.case = TRUE, value = TRUE)
  trt_concvars <- grep(test_site, concvars, ignore.case = TRUE, value = TRUE)
  con_concvars <- grep(control_ste, concvars, ignore.case = TRUE, value = TRUE)
} else if (is.na(concentrations)) {
  trt_concvars <- NA
  con_concvars <- NA
} else {
  concvars <- concentrations
  trt_concvars <- grep(test_site, concvars, ignore.case = TRUE, value = TRUE)
  con_concvars <- grep(control_ste, concvars, ignore.case = TRUE, value = TRUE)
}

# create a dummy variable for period (before/after = 0/1)
paired$period_num <- ifelse(paired$period == 'before', 0, 1)

out <- as.data.frame(matrix(ncol = 6, nrow = length(trt_loadvars)))
names(out) <- c('mdc_before', 'r2_before', 'mdc_after', 'r2_all', 'p_slopes', 'p_intercepts')

for (i in 1:length(trt_loadvars)) {

  temp_trt <-  trt_loadvars[i]
  temp_con <- gsub(test_site, control_site, temp_trt)
  
  #####################################
  # calculate MDC just using before dat
  before_dat <- filter(paired, period == 'before')
  
  # linear model
  temp_mod <- lm(log10(before_dat[,temp_trt]) ~ log10(before_dat[,temp_con]))
  stats <- summary(temp_mod)
  out$r2_before[i] <- round(stats$r.squared, 2)
  
  # calc MSE
  temp_mse <- mean(temp_mod$residuals^2)
  
  # get degrees of freedom and mean squared error
  degf <- temp_mod$df.residual
  
  # get tval based on deg of freedom
  tval <- qt(0.05, degf, lower.tail = FALSE)
  
  mdc <- tval*sqrt(2*(temp_mse/(degf+2)))
  out$mdc_before[i] <- round((1-(10^-mdc))*100, 0)
  
  #####################################
  # calculate MDC using before and after data

  # linear model
  temp_mod <- lm(log10(paired[,temp_trt]) ~ log10(paired[,temp_con])+paired$period_num)
  
  # calc MSE
  stats <- summary(temp_mod)
  s <- stats$coefficients[3,2]
  
  out$r2_all[i] <- round(stats$r.squared, 2)
  
  # get degrees of freedom and mean squared error
  degf <- temp_mod$df.residual
  
  # get tval based on deg of freedom
  tval <- qt(0.05, degf, lower.tail = FALSE)
  
  mdc <- tval*s
  out$mdc_after[i] <- round((1-(10^-mdc))*100, 0)
  
  ################################
  # test for before and after differences in relationship
  temp_mod <- lm(log10(paired[,temp_trt]) ~ log10(paired[,temp_con])*paired$period_num)
  out$p_slopes[i] <- round(summary(temp_mod)$coefficients[4,4], 3)
  out$p_intercepts[i] <- round(summary(temp_mod)$coefficients[3,4], 3)
  
}

temp_filename <- paste0(site, '_', site_paired, '_mdc.csv')
write.csv(file.path('data_cached', temp_filename))
