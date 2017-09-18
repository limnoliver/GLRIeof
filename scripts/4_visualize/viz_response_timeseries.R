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
