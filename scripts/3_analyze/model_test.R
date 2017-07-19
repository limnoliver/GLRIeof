SW1 <- subset(wq.bystorm, site == 'SW1')
SW3 <- subset(wq.bystorm, site == 'SW3')
tp.mod <- lm(log10(TP_Unfiltered_Load_pounds)~log10(peak_discharge), data = SW1)
plot(residuals(tp.mod)~as.Date(SW1$storm_start))

tn.mod <- lm(log10(Total_Nitrogen_load_pounds)~log10(peak_discharge), data = SW1)
plot(residuals(tn.mod)~as.Date(SW1$storm_start))
plot(log10(SW1$peak_discharge)~as.Date(SW1$storm_start))

tp.mod.2 <- lm(log10(TP_Unfiltered_Load_pounds)~log10(peak_discharge), data = SW3)
plot(residuals(tp.mod.2)~as.Date(SW3$storm_start))

tn.mod.2 <- lm(log10(Total_Nitrogen_load_pounds)~log10(peak_discharge), data = SW3)
plot(residuals(tn.mod.2)~as.Date(SW3$storm_start))
plot(log10(SW3$peak_discharge)~as.Date(SW3$storm_start))
