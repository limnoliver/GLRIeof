# deomonstration of how field level data may change 
# after implementation

plot(sw1$NO2_NO3_N_mg_L ~ sw1$days_since_fertilizer, 
     pch = 16, col = sw1$period_crop, xlab = "Days since fert/manure application", 
     ylab = "NO2 + NO3 (mg/L)")

sw1_after <- sw1 %>%
  subset(period == "after")

sw1_rel <- lm(sw1$NO2_NO3_N_mg_L ~ sw1$days_since_fertilizer)
sw1_after_rel <- lm(sw1_after$NO2_NO3_N_mg_L ~ sw1_after$days_since_fertilizer)
summary(sw1_after_rel)

sw1_before <- sw1 %>%
  subset(period == "before")

sw1_before_rel <- lm(sw1_before$NO2_NO3_N_mg_L ~ sw1_before$days_since_fertilizer)
summary(sw1_after_rel)
abline(sw1_after_rel, col = 'green')
#abline(sw1_rel, col = 'blue')
abline(sw1_before_rel, col = 'black')

plot(sw1$TP_Unfiltered_Load_pounds ~ sw1$peak_discharge,
     col = sw1$period_crop, pch = 16)

test <- lm(sw1$peak_discharge ~ log10(sw1$sum_runoff))
test.resid <- test$residuals

plot(sw1$TP_Unfiltered_Load_pounds ~ test.resid,
     xlab = "Residual of Peak ~ Total Runoff",
     ylab = "log10 TP Load (pounds)")
