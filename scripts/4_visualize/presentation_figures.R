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


#####################################
#### better looking residual figs ###
#####################################
library(ggplot2)
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
df <- data.frame(
  x = c(2,3,4),
  y = rep(max(resid)*1.4, 3),
  y2 = rep(max(resid)*1.2, 3),
  label = c(paste0("MDC = ", round(mdc.perc.all[i],0), "%"),
            paste0("MDC = ", round(mdc.perc.corn[i],0), "%"),
            paste0("MDC = ", round(mdc.perc.alfalfa[i],0), "%")),
  label2 = c("p = 0.009", "p = 0.54", "p = 0.006"))

p5 <- ggplot(resid.test.all, aes(y = resids, x = period)) +
  geom_boxplot(aes(fill = period)) +
  scale_fill_manual(values = c('darkgray', 'lightgray', gg_color_hue(2)[1], gg_color_hue(2)[2]), guide = F) +
  theme_bw() +
  geom_text(data = df, aes(x = x, y = y, label = label)) + 
  geom_text(data = df, aes(x = x, y = y2, label = label2), fontface = c('bold', 'plain', 'bold')) +
  labs(y = 'Residuals', x = '') +
  theme(panel.grid = element_blank())
  
ggsave('figures/SS_residuals.png', p5, height = 3, width = 6.7)

dat <- data.frame(x = as.Date(sw1.mod$storm_start),
                  y = resid,
                  period = sw1$period_crop)
p6 <- ggplot(dat, aes(y = y, x = x)) +
  geom_point(aes(color = period)) +
  scale_color_manual(values = c('darkgray', gg_color_hue(2)[1], gg_color_hue(2)[2])) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  labs(x = "", y = 'Residual') +
  theme(panel.grid.minor.y = element_blank())

ggsave('figures/SS_residuals_time.png', p6, height = 3, width = 8)

###########################################
# figure to show proportion of loads that 
# occur in frozen vs non frozen periods
###########################################
source('scripts/2_process/process_merged_data.R')
library(tidyr)
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
sw1.prop <- sw1 %>%
  group_by(frozen) %>%
  summarise_at(vars(Suspended_Sediment_Load_pounds:Organic_Nitrogen_Load_pounds), sum)

names(sw1.prop)[2:10] <- c('SS', 'Cl', 'NO3', 'NH4', 'TKN', 'DRP', 'TP', 'TN', 'ON')
sw1.long <- sw1.prop %>%
  gather(variable, value, -frozen)

sw1.long$frozen <- as.factor(sw1.long$frozen)
levels(sw1.long$frozen) <- c('not frozen', 'frozen') 
sw1.long$frozen <- as.character(sw1.long$frozen)

sw1.long.tot <- sw1.long %>%
  group_by(variable) %>%
  summarise_at('value', sum)

names(sw1.long.tot)[2]<- 'total'

sw1.bar.plot <- left_join(sw1.long, sw1.long.tot) %>%
  mutate(percentage = value/total)

var.order <- filter(sw1.bar.plot, frozen == 'not frozen') %>%
  arrange(percentage)

sw1.bar.plot$variable <- factor(sw1.bar.plot$variable, levels = var.order$variable)

p7 <- ggplot(data = sw1.bar.plot, aes(x = variable, y = percentage, fill = frozen)) +
  geom_bar(stat = 'identity') +
  geom_hline(aes(yintercept = mean(var.order$percentage), linetype = "Mean proportion of \nnon-frozen loads"), size = 2, color = 'darkgray') +
  scale_fill_manual(values = c(gg_color_hue(2)[2], gg_color_hue(2)[1])) +
  theme_bw() +
  labs(x = '', y = 'Proportion of Load', linetype = "")

ggsave('figures/seasonal_loads_barchart.png', p7, height = 4.2, width = 8)

#####################################
## variable ranks
####################################

p8 <- ggplot(ranks.long, aes(x = predictor, y = rank)) +
  geom_boxplot() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
  labs(x = "", y = 'Rank across all responses\n1 = top predictor')

ggsave('figures/predictor_rank_sw1.png', p8, height = 4, width = 7)
