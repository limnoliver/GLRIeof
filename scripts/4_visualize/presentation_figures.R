# figs for presentation

p <- ggplot(sw1, aes(x = energy_m1, y = Suspended_Sediment_Load_pounds)) +
  geom_point(aes(color = frozen)) +
  theme_bw() +
  labs(x = "Storm Energy", y = 'log10 SS Load (pounds)')

ggsave('figures/hydro_conditions_example.png', p, height = 3, width = 6)

dat <- filter(before_after_resid, variable != "Peak Discharge")
dat$type = c(rep("concentration", 9), rep("load", 9))

p2 <- ggplot(dat, aes(x = reorder(variable, mdc_all), y = mdc_all)) +
  geom_col(aes(fill = type)) +
  geom_hline(yintercept = 50) +
  theme_bw() +
  labs(y = "% Minimum Detectable Change", x = "") +
  coord_cartesian(ylim = c(0, 65)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave('figures/MDC_allvars.png', p2, height = 5, width = 8.5)

p3 <- ggplot(dat, aes(x = reorder(variable, mdc_all), y = mdc_corn)) +
  geom_col(aes(fill = type)) +
  geom_hline(yintercept = 50) +
  theme_bw() +
  labs(y = "% Minimum Detectable Change", x = "") +
  coord_cartesian(ylim = c(0, 65)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave('figures/MDC_corn.png', p3, height = 5, width = 8.5)
plot(before_after_resid$mdc_all ~ before_after_resid$perc_var)

names(sw1)

p4 <- ggplot(sw1, aes(x = storm_start, y = Suspended_Sediment_Load_pounds)) +
  geom_point(aes(size = sum_runoff, color = frozen), alpha = 0.5) +
  scale_size_continuous(trans = 'log10', breaks = c(200, 2000, 20000), name = 'Total Runoff (cubic ft)') +
  geom_vline(xintercept = as.POSIXct('2015-05-10 00:00:01')) +
  theme_bw() +
  labs(x = "Date", y = "SS Load (pounds)")

ggsave('figures/SW1_SS_load_throughtime.png', p4, height = 5, width = 12)
         
