# before and after statistics for frozen and unfrozen times
library(dplyr)
eof <- read.csv('data_cached/merged_wq_rain_discharge.csv', header = TRUE)

# add columns for frozen, runoff and peak discharge coefs
sw1 <- eof %>%
  filter(site == 'SW1') %>%
  mutate(frozen = as.logical(substr(frozen, 1, 1))) %>%
  mutate(runoff_coef = sum_runoff/rain) %>%
  mutate(peak_discharge_coef = peak_discharge/rain) %>%
  mutate(period = ifelse(as.POSIXct(storm_start) >= as.POSIXct('2014-11-01 00:00:01'), 'after', 'before'))

# calculate statistics for runoff and peak discharge coefs  
sw1.ba.stats <- sw1 %>%
  group_by(frozen, period) %>%
  summarise_at(vars(c(runoff_coef, peak_discharge_coef)), funs(median, sd), na.rm = TRUE)

# calculate number of events
sw1.count <- sw1 %>%
  group_by(frozen, period) %>%
  summarize(count = n())

# calculate total storm loads for each constituent
# break these up by frozen/unfrozen
load_vars <- grep()
sw1.loads.sums <- sw1 %>%
  group_by(frozen, period) %>%
  select(frozen, period, Suspended_Sediment_Load_pounds:Organic_Nitrogen_Load_pounds, sum_runoff) %>%
  summarise_all(sum)

sw1.loads.medians <- sw1 %>%
  group_by(frozen, period) %>%
  select(frozen, period, Suspended_Sediment_Load_pounds:Organic_Nitrogen_Load_pounds, sum_runoff) %>%
  summarise_all(median, na.rm = TRUE)

eof$frozen  <- as.logical(substr(eof$frozen, 1, 1))
sub.dat <- subset(sub.dat, site == 'SW1')


