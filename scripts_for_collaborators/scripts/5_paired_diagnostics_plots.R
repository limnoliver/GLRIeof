# diagnostic plots for paired design

tempfile <- file.path('data_cached', paste0(site, '_', site_paired, "_prepped_WQbystorm.csv"))
wq <- read.csv(tempfile, stringsAsFactors = F)

# for each constituent, plot time vs conc or load, size of dot = discharge
# get load and conc vars
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

temp_date <- paste0(site, '_storm_start')
wq[,temp_date] <- as.POSIXct(wq[,temp_date])

for (i in 1:length(trt_loadvars)) {
  
  temp_trt <-  trt_loadvars[i]
  temp_con <- gsub(test_site, control_site, temp_trt)
  temp_clean <- clean_names[i]
  
  temp <- select_(wq, temp_trt, temp_con, temp_date, 'period') %>%
    gather(key = 'site', value = 'value', -temp_date, -period) %>%
    mutate(site = ifelse(site %in% temp_trt, 'treatment', 'control'))
  
  #names(temp)[which(names(temp) %in% 'value')] <- clean_names[i]
  
  p <- ggplot(temp, aes_string(x = temp_date, y = 'value')) +
    geom_point(aes(color = period, shape = site)) +
    coord_trans(y = 'log10') +
    scale_y_continuous(breaks = c(0.1, 1, 10, 100, 300, 600)) +
    scale_shape_manual(values = c(21, 16)) +
    theme_bw() +
    theme(panel.grid.minor.y = element_blank()) +
    labs(x = 'Storm Date', y = clean_names[i])
  
  short_col_name <- paste0(control_site, '_', temp_trt, '_throughtime.png')
  tempname <- file.path('figures', 'diagnostic', short_col_name)
  ggsave(tempname, p, height = 4, width = 8)
}
