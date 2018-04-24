ggplot(wq, aes(x = con_SS_load, y = trt_SS_load)) +
  geom_point(aes(color = period)) +
  coord_trans(x = 'log10', y = 'log10') +
  labs(x = paste('Control - ', ))