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

conc <- grep('mg_L', names(wq))
conc <- wq[,c(conc, p.discharge)]
drop <- grep('<', conc$Suspended_Sediment_mg_L)
conc <- conc[-drop, ]
drop <- grep('<', conc$NO2_NO3_N_mg_L)
conc <- conc[-drop, ]
conc <- conc[!is.na(conc$peak_discharge), ]
conc$Suspended_Sediment_mg_L <- as.numeric(conc$Suspended_Sediment_mg_L)
conc$NO2_NO3_N_mg_L <- as.numeric(conc$NO2_NO3_N_mg_L)
log.conc <- log10(conc)
names(log.conc) <- c("SS mg_L", "Cl mg_L", "NO2+NO3 mg_L", "NH4 mg_L", "TKN mg_L", "DRP mg_L", "TP mg_L", "TN mg_L", "ON mg_L", "Peak Dis.")

png('figures/log_wq_conc_data_check.png', height = 800, width = 800)
pairs.panels(log.conc, method = 'pearson', hist.col = '#00AFBB', density = FALSE,
             ellipses = FALSE, cex = 2.4, cex.cor = .8, col = "red", cex.lab = 1.8)
dev.off()
