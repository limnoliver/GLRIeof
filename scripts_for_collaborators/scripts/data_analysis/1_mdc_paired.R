#load paired data
paired_filename <- file.path("data_cached", paste0(site, "_", site_paired, "_prepped_WQbystorm.csv"))
wq <- read.csv(paired_filename, stringsAsFactors = F)

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

if (length(other_responses) == 1 & !is.na(other_responses)){
  othervars <- grep(concentrations, names(wq), ignore.case = TRUE, value = TRUE)
  trt_othervars <- grep(test_site, othervars, ignore.case = TRUE, value = TRUE)
  con_othervars <- grep(control_ste, othervars, ignore.case = TRUE, value = TRUE)
} else if (is.na(concentrations)) {
  trt_othervars <- NA
  con_othervars <- NA
} else {
  othervars <- concentrations
  trt_othervars <- grep(test_site, othervars, ignore.case = TRUE, value = TRUE)
  con_othervars <- grep(control_ste, othervars, ignore.case = TRUE, value = TRUE)
}

# create a dummy variable for period (before/after = 0/1)
wq$period_num <- ifelse(wq$period == 'before', 0, 1)

# combine conc and load vars together
trt_vars <- c(trt_concvars, trt_loadvars, trt_othervars)
trt_vars <- trt_vars[!is.na(trt_vars)]

#con_vars <- c(con_concvars, con_loadvars)
#con_vars <- con_vars[!is.na(con_vars)]

out <- as.data.frame(matrix(ncol = 3, nrow = length(trt_vars)))
names(out) <- c('variable', 'mdc', 'r2')



for (i in 1:length(trt_vars)) {

  temp_trt <-  trt_vars[i]
  temp_con <- gsub(test_site, control_site, temp_trt)
  
  #####################################
  # calculate MDC just using before dat
  before_dat <- filter(wq, period == 'before')
  
  # linear model
  temp_mod <- lm(log10(before_dat[,temp_trt]) ~ log10(before_dat[,temp_con]))
  stats <- summary(temp_mod)
  out$r2[i] <- round(stats$r.squared, 2)
  
  # calc MSE
  temp_mse <- mean(temp_mod$residuals^2)
  
  # get degrees of freedom and mean squared error
  degf <- temp_mod$df.residual
  
  # get tval based on deg of freedom
  tval <- qt(0.05, degf, lower.tail = FALSE)
  
  mdc <- tval*sqrt(2*(temp_mse/(degf+2)))
  out$mdc[i] <- round((1-(10^-mdc))*100, 0)
}

out$variable <- clean_names

temp_filename <- paste0(site, '_', site_paired, '_mdc.csv')
write.csv(file.path('data_cached', temp_filename))
