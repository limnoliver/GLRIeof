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
wq$Date <- wq[,temp_date]
wq <- dataRetrieval::addWaterYear(wq)

if (!is.na(discharge_col)){
  discharge_con <- paste0(control_site, '_', discharge_col)
  discharge_trt <- paste0(test_site, '_', discharge_col)
  
  discharge <- select_(wq, discharge_con, discharge_trt, temp_date, 'period') %>%
    gather(key = 'site', value = 'runoff', -temp_date, -period) %>%
    mutate(site = ifelse(site %in% discharge_trt, 'treatment', 'control'))
  
}

plot_all_vars <- c(con_concvars, trt_concvars, con_loadvars, trt_loadvars)
plot_all_vars <- plot_all_vars[!is.na(plot_all_vars)]

plot_trt_vars <- c(trt_concvars, trt_loadvars)
plot_trt_vars <- plot_trt_vars[!is.na(plot_trt_vars)]

for (i in 1:length(plot_trt_vars)) {
  
  temp_trt <-  trt_loadvars[i]
  temp_con <- gsub(test_site, control_site, temp_trt)
  temp_clean <- clean_names[i]
  
  temp <- select_(wq, temp_trt, temp_con, temp_date, 'period') %>%
    gather(key = 'site', value = 'value', -temp_date, -period) %>%
    mutate(site = ifelse(site %in% temp_trt, 'treatment', 'control'))
  
  if (!is.na(discharge_col)){
    temp <- left_join(temp, discharge, by = c('site', 'period', temp_date))
  }

  p <- ggplot(data = temp, aes_string(x = temp_date, y = 'value')) +
    coord_trans(y = 'log10') +
    scale_y_continuous(breaks = c(0.1, 1, 10, 100, 300, 600)) +
    scale_shape_manual(values = c(21, 16)) +
    theme_bw() +
    theme(panel.grid.minor.y = element_blank()) +
    labs(x = 'Storm Date', y = clean_names[i])
  
  if (!is.na(discharge_col)){ 
    p <- p + geom_point(aes(color = period, shape = site, size = runoff)) +
      scale_size_continuous(trans = 'log10')
  } else {
    p <- p + geom_point(aes(color = period, shape = site))
  }
  
  short_col_name <- paste0(control_site, '_', temp_trt, '_throughtime.png')
  tempname <- file.path('figures', 'diagnostic', short_col_name)
  ggsave(tempname, p, height = 4, width = 8)
}

# calculate summary statistics by water year and export to tables/figures for review

sum_stats <- wq %>%
  group_by(waterYear) %>%
  summarize(n_before = length(which(period == 'before')),
            n_after = length(which(period == 'after')))

if (!is.na(con_loadvars[1])){
  temp <- wq %>%
    group_by(waterYear) %>%
    summarize_at(.vars = c(con_loadvars, trt_loadvars), sum, na.rm = T)
  
  names(temp)[which(names(temp) %in% c(con_loadvars, trt_loadvars))] <- 
    paste0('sum_', c(con_loadvars, trt_loadvars))

  sum_stats <- left_join(sum_stats, temp)
}

if(!is.na(con_concvars[1])) {
  
  sum_stats <- wq %>%
    group_by(waterYear) %>%
    summarize_at(.vars = c(con_concvars, trt_concvars), mean, na.rm = T) %>%
    left_join(sum_stats)
}

if (!is.na(discharge_col)) {
  disvars <- grep(discharge_col, names(wq), ignore.case = T, value = T)
  sum_stats <- wq %>%
    group_by(waterYear) %>%
    summarize_at(.vars = disvars, sum, na.rm = T) %>%
    left_join(sum_stats)
}

temp_table_name <- paste0(site, "_", site_paired, '_response_summary.csv')
write.csv(sum_stats, file.path('figures', 'diagnostic', temp_table_name), row.names = F)

# test if figures were written

test <- list.files('figures/diagnostic')
time.figs <- grep('throughtime', test)
summary.table <- grep('response_summary', test)

if (length(time.figs) != length(plot_trt_vars)|
    length(summary.table) != 1) {
  warning("Not all diagnostic plots or tables were generated. To debug, see code in '5_diagnostic_plots.R'", call. = F)
} else {
  message("Diagnostic plots and tables have been made. See figures in figures/diagnostic as a visual test of proper importing, merging, and cleaning. If you would like to add figures to diagnostics, you can modify the script in scripts/data_processing/5_paired_diagnostic_plots.R.")
}

