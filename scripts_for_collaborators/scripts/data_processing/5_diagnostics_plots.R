# diagnostic plots for before/after design

tempfile <- file.path('data_cached', paste0(site, "_mod_dat.csv"))
wq <- read.csv(tempfile, stringsAsFactors = F)


# for each constituent, plot time vs conc or load, size of dot = discharge
# get load and conc vars
if (length(loads) == 1 & !is.na(loads)){
  
  loadvars <- grep(loads, names(wq), ignore.case = TRUE, value = TRUE)

} else if (is.na(loads)) {
  loadvars <- NA
} else {
  loadvars <- loads
}

if (length(concentrations) == 1 & !is.na(concentrations)){
  
  concvars <- grep(concentrations, names(wq), ignore.case = TRUE, value = TRUE)
  
} else if (is.na(concentrations)) {
  concvars <- NA
} else {
  concvars <- concentrations
}

wq[,'storm_start'] <- as.POSIXct(wq[,'storm_start'])
wq$Date <- wq[,'storm_start']
wq <- dataRetrieval::addWaterYear(wq)

plot_all_vars <- c(concvars, loadvars)
plot_all_vars <- plot_all_vars[!is.na(plot_all_vars)]



for (i in 1:length(plot_all_vars)) {
  
  temp_var <-  plot_all_vars[i]

  p <- ggplot(data = wq, aes_string(x = 'storm_start', y = temp_var)) +
    geom_point(aes(color = period, size = runoff_volume), alpha = 0.5) +
    #scale_size_continuous(trans = 'log10') +
    #coord_trans(y = 'log10') +
    theme_bw() +
    theme(panel.grid.minor.y = element_blank()) +
    labs(x = 'Storm Date', y = clean_names[i])
  
  short_col_name <- paste0(site, '_', temp_var, '_throughtime.png')
  tempname <- file.path('figures', 'diagnostic', short_col_name)
  ggsave(tempname, p, height = 4, width = 8)
}
##############################################################################
# calculate summary statistics by water year and export to tables/figures for review

sum_stats <- wq %>%
  group_by(waterYear) %>%
  summarize(n_before = length(which(period == 'before')),
            n_after = length(which(period == 'after')))

if (!is.na(loadvars[1])){
  temp <- wq %>%
    group_by(waterYear) %>%
    summarize_at(.vars = c(loadvars, 'runoff_volume'), sum, na.rm = T)
  
  names(temp)[which(names(temp) %in% c(loadvars, 'runoff_volume'))] <- 
    paste0('sum_', c(loadvars, 'runoff_volume'))

  sum_stats <- left_join(sum_stats, temp)
}

if(!is.na(concvars[1])) {
  
  sum_stats <- wq %>%
    group_by(waterYear) %>%
    summarize_at(.vars = concvars, mean, na.rm = T) %>%
    left_join(sum_stats)
}

temp_table_name <- paste0(site, '_response_summary.csv')
write.csv(sum_stats, file.path('figures', 'diagnostic', temp_table_name), row.names = F)

##########################################################################
# create frozen/non frozen loads  plot
# ignore first and last year if the difference between start and end months
# is greater than 3

dif <- abs(month((min(wq$storm_start))) - month((max(wq$storm_start))))

library(tidyr)
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#group by frozen/non frozen and sum loads over both seasons
if (dif > 3) {
  wq.temp <- filter(wq, waterYear != min(waterYear)) %>%
    filter(waterYear != max(waterYear))
} else {
  wq.temp <- wq
}
# calculate sum of loads
prop <- wq.temp %>%
  group_by(frozen) %>%
  summarise_at(vars(loadvars), sum)

# change names to be shortened/clean versions
names(prop)[2:ncol(prop)] <- clean_names[(length(concvars) + 1):(length(clean_names))]
prop.long <- prop %>%
  gather(variable, value, -frozen)

# change true/false to frozen/not frozen
prop.long$frozen <- as.factor(prop.long$frozen)
levels(prop.long$frozen) <- c('not frozen', 'frozen') 
prop.long$frozen <- as.character(prop.long$frozen)

# get sums across seasons
prop.long.tot <- prop.long %>%
  group_by(variable) %>%
  summarise_at('value', sum)

names(prop.long.tot)[2]<- 'total'

# merge seasonal and total sums, calculate percentage
prop.bar.plot <- left_join(prop.long, prop.long.tot) %>%
  mutate(percentage = value/total)

# order variables by percentage of not frozen loads
var.order <- filter(prop.bar.plot, frozen == 'not frozen') %>%
  arrange(percentage)

prop.bar.plot$variable <- factor(prop.bar.plot$variable, levels = var.order$variable)

# plot
p <- ggplot(data = prop.bar.plot, aes(x = variable, y = percentage, fill = frozen)) +
  geom_bar(stat = 'identity') +
  #geom_hline(aes(yintercept = mean(var.order$percentage), linetype = "Mean proportion of \nnon-frozen loads"), size = 2, color = 'darkgray') +
  scale_fill_manual(values = c(gg_color_hue(2)[2], gg_color_hue(2)[1])) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = '', y = 'Proportion of Load', linetype = "")

# save plot
temp_figname <- paste0(site, "_seasonal_loads.png")
ggsave(file.path('figures', 'diagnostic', temp_figname), p, height = 5, width = 8)


###############################################################
# plot some predictor variables to make sure they calculated ok

# rain

p <- ggplot(wq, aes(x = weq, y = runoff_volume)) +
  geom_point(aes(color = frozen)) +
  theme_bw() +
  annotate('text', x = Inf, y = Inf, label = paste0(length(which(is.na(wq$weq))), ' missing rain or snow values.'), hjust = 1, vjust = 1, col = 'red') +
  labs(x = 'Snow + Rain (water equivalent, in inches)', y = "Total Runoff")

temp_figname <- paste0(site, '_runoff_vs_weq.png')
ggsave(file.path('figures', 'diagnostic', temp_figname), p, width = 6, height = 4)


# test if figures were written

test <- list.files('figures/diagnostic')
time.figs <- grep('throughtime', test)
seasonal.fig <- grep('seasonal_loads', test)
predictor.fig <- grep('runoff', test)

if (length(time.figs) != length(plot_all_vars)|
    length(seasonal.fig) != 1 |
    length(predictor.fig) != 1) {
  warning("Not all diagnostic plots were generated. To debug, see code in '5_diagnostic_plots.R'", call. = F)
} else {
  message("Diagnostic plots have been made. See figures in figures/diagnostic as a visual test of proper importing, merging, and cleaning. If you would like to add figures to diagnostics, you can modify the script in scripts/data_processing/5_diagnostic_plots.R.")
}
