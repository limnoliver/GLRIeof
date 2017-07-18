# this script visually checks WQ data
library(psych)
wq <- read.csv('data_cached/cleaned_WQdata.csv', header = TRUE)
wq <- subset(wq, estimated == FALSE)
# which columns to test?
loads <- grep('load', names(wq), ignore.case = TRUE)
p.discharge <- grep('peak', names(wq), ignore.case = TRUE)
wq.loads <- wq[, c(loads, p.discharge)]
log.wq.loads <- log10(wq.loads)
log.wq.loads <- subset(log.wq.loads, Dissolved_Reactive_Phosphorus_Load_pounds != -Inf)
names(log.wq.loads) <- c("SS Load", "Cl Load", "NO2+NO3 Load", "NH4 Load", "TKN Load", "DRP Load", "TP Load", "TN Load", "ON Load", "Peak Dis.")
png('figures/log_wq_data_check.png', height = 1000, width = 1000)
pairs.panels(log.wq.loads, method = 'pearson', hist.col = '#00AFBB', density = FALSE,
             ellipses = FALSE, cex = 1.9, cex.cor = .8, col = "red", cex.lab = 1.4)
dev.off()