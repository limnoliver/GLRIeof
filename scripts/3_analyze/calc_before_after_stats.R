# before and after statistics for frozen and unfrozen times
library(dplyr)
library(tidyr)
library(ggplot2)
eof <- read.csv('data_cached/merged_dat.csv', header = TRUE, 
                colClasses = c(storm_start = 'POSIXct', storm_end = 'POSIXct'))

# add columns for frozen, runoff and peak discharge coefs
sw1 <- eof %>%
  filter(site == 'SW1') %>%
  mutate(frozen = as.logical(substr(frozen, 1, 1))) %>%
  mutate(runoff_coef = sum_runoff/rain) %>%
  mutate(peak_discharge_coef = peak_discharge/rain) %>%
  mutate(period = ifelse(storm_start >= as.POSIXct('2015-06-01 00:00:01'), 'after', 'before'))

sw1$period[sw1$storm_start > as.POSIXct('2015-05-10 00:00:01')& sw1$storm_start < as.POSIXct('2015-06-01 00:00:01')] <- 'transition'

# create another period that includes distinguising between
# after - corn and after - alfalfa
sw1$period_crop <- sw1$period
sw1$period_crop[sw1$period_crop == 'after'] <- "after (corn)"
sw1$period_crop[sw1$storm_start >= as.POSIXct('2016-05-04 00:00:01')] <- "after (alfalfa)"

# calculate statistics for runoff and peak discharge coefs  
sw1.ba.stats <- sw1 %>%
  group_by(frozen, period) %>%
  summarise_at(vars(c(runoff_coef, peak_discharge_coef, rain, duration, Ievent)), funs(median, sd), na.rm = TRUE)

# calculate number of events
sw1.count <- sw1 %>%
  group_by(frozen, period) %>%
  summarize(count = n())

# calculate total storm loads for each constituent
# break these up by frozen/unfrozen

# loads
sw1.loads.sums <- sw1 %>%
  group_by(frozen, period) %>%
  select(frozen, period, Suspended_Sediment_Load_pounds:Organic_Nitrogen_Load_pounds, sum_runoff) %>%
  summarise_all(sum)

sw1.loads.medians <- sw1 %>%
  group_by(frozen, period) %>%
  select(frozen, period, Suspended_Sediment_Load_pounds:Organic_Nitrogen_Load_pounds, sum_runoff) %>%
  summarise_all(median, na.rm = TRUE)

# concentrations
sw1.conc.medians <- sw1 %>%
  group_by(frozen, period) %>%
  select(frozen, period, Suspended_Sediment_mg_L:Organic_Nitrogen_computed_mg_L, sum_runoff) %>%
  summarise_all(median, na.rm = TRUE)
str(sw1.conc.medians)

sw1.conc.long <- sw1 %>%
  select(frozen, period, period_crop, Suspended_Sediment_mg_L:Organic_Nitrogen_computed_mg_L, sum_runoff) %>%
  gather(variable, value, -frozen, -period, -period_crop) %>%
  filter(period != 'transition') %>%
  mutate(frozen = ifelse(frozen == TRUE, 'frozen', 'thawed'))

sw1.conc.long$period <- ordered(as.factor(sw1.conc.long$period), levels = c('before', 'after'))
sw1.conc.long$period_crop <- ordered(as.factor(sw1.conc.long$period_crop), levels = c('before', 'after (corn)', 'after (alfalfa)'))

sw1.conc.long$variable <- as.factor(sw1.conc.long$variable)
sw1.conc.long$variable <- factor(sw1.conc.long$variable, levels(sw1.conc.long$variable)[c(7, 2, 9, 8, 5, 4, 1, 10, 3, 6)])
levels(sw1.conc.long$variable) <- c('SS (mg/L)', 'Cl (mg/L)', 'TN (mg/L)', 'TKN (mg/L)', 'Org N (mg/L)', 'NO2 + NO3 (mg/L)',
                                    'NH4 (mg/L)', 'TP (mg/L)', 'DRP (mg/L)', 'Storm Runoff')


geom_text(data = n.cens, aes(x = site, y = y, label = label)) 
  
n.vals <- sw1.conc.long %>%
  group_by(variable, frozen, period_crop) %>%
  summarize(n = length(value), y = ifelse(max(log10(value)) > 1.1, 
                                          max(log10(value)) + (.15*max(log10(value))), 
                                          ifelse(max(log10(value)) < 0.1, 
                                                 0.15, max(log10(value)) + (.8*max(log10(value))))))

p <- ggplot(sw1.conc.long, aes(y = log10(value), x = frozen, group = interaction(period_crop, frozen))) +
  geom_boxplot(aes(fill = period_crop)) +
  facet_wrap(~variable, scales = 'free_y') +
  geom_text(data = n.vals, aes(y = y, label = n), position = position_dodge(width = .8)) +
  theme_bw() +
  labs(x = '', y = 'log10 Concentration') +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        strip.text = element_text(size = 14))

ggsave('figures/before_after_concentrations.png', p)

head(sw1.conc.medians.long)


