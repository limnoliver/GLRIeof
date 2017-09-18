library(ggplot2)
library(tidyr)
# visualize response variables through time

layout(matrix(1:4, ncol = 1))
plot(wq.dat$peak_discharge ~ wq.dat$storm_start, pch = 16, cex = 0.8, col = 'blue', type = 'p')
plot(wq.dat$Suspended_Sediment_Load_pounds ~ wq.dat$storm_start, pch = 16, cex = 0.8, col = 'orange', type = 'p')
plot(wq.dat$TP_Unfiltered_Load_pounds ~ wq.dat$storm_start, pch = 16, cex = 0.8, col = 'purple', type = 'p')
plot(wq.dat$TKN_Unfiltered_Load_pounds ~ wq.dat$storm_start, pch = 16, cex = 0.8, col = 'green', type = 'p')

layout(matrix(1:4, ncol = 1))
plot(wq.dat$peak_discharge ~ wq.dat$storm_start, pch = 16, cex = 0.8, col = 'blue', type = 'p')
plot(wq.dat$Suspended_Sediment_mg_L ~ wq.dat$storm_start, pch = 16, cex = 0.8, col = 'orange', type = 'p')
plot(wq.dat$TP_Unfiltered_mg_L ~ wq.dat$storm_start, pch = 16, cex = 0.8, col = 'purple', type = 'p')
plot(wq.dat$TKN_Unfiltered_mg_L ~ wq.dat$storm_start, pch = 16, cex = 0.8, col = 'green', type = 'p')

# prep data to show what proportion of loads
# are from frozen and non-frozen seasons

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

ggsave('figures/seasonal_loads_barchart.png', 
ggplot(data = sw1.bar.plot, aes(x = variable, y = percentage, fill = frozen)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 0.62, size = 2, color = 'darkgray') +
  scale_x_discrete(limits = c('SS', 'TP', 'ON', 'TKN', 'NO3', 'Cl', 'DRP', 'NH4')) +
  scale_fill_manual(values = c('lightskyblue1', 'lightsalmon')) +
  theme_base() +
  labs(x = '', y = 'Proportion of Load'),
device = 'png')


