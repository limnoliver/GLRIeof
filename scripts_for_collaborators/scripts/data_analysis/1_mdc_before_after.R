# read in data that is prepped for analysis
temp.filename <- paste0(site, '_mod_dat.csv')
dat <- read.csv(file.path('data_cached', temp.filename))

# define predictors
load('data_cached/modvars.Rdata')
dat.mod <- dat[,predictors]
dat.mod <- complete.cases(dat.mod)

n.lost <- length(dat.mod[dat.mod == FALSE])
message(paste0(n.lost, ' observations dropped due to missing predictor data.'))

dat.mod <- dat[dat.mod, ]

# get rid of highly correlated variables
which.frozen <- which(predictors %in% 'frozen')
predictors.cor <- cor(dat[,predictors[-which.frozen]], use = 'complete.obs') # drop var "crop" from correlation since it's a categorical var
names.cor <- row.names(predictors.cor)
drop.predictors <- caret::findCorrelation(predictors.cor, cutoff = 0.95, verbose = FALSE, exact = TRUE)

predictors.keep <- c(names.cor[-drop.predictors], 'frozen')

# log transform response vars
dat.mod[,responses] <- log10(dat.mod[,responses])
sums <- colSums(dat.mod[,responses])
if(any(is.infinite(sums))) {
  stop('Zeros in the response variables caused values to be infinite when log transformed. Please see code in scripts/data_analysis/1_mdc_before_after.R to debug.', call. = F)
}

responses[is.infinite(sums) == TRUE]

# keep just "before" data

dat.mod.before <- filter(dat.mod, period == 'before')



# save MDC as output from loop
mdc.perc.nbefore <- c()
perc.var <- c()


# loop through responses to create equation and model
for (i in 1:length(responses)) {
  
  # create model equation
  mod.equation <- as.formula(paste(responses[i], paste(predictors.keep, collapse = " + "), sep = " ~ "))
  
  # create random forest model - before data
  #mod <- randomForest(mod.equation, data = dat.mod, importance = T, na.action = na.omit)
  mod.before <- randomForest(mod.equation, data = dat.mod.before, importance = T, na.action = na.omit, ntree = 1000)
  
  perc.var[i] <- round(mod.before$rsq[1000]*100, 1)
  
  # calculate minimum detectable change for each constituent based on this model
  
  # pull out mod.before mean squared error
  mse.before <- mod.before$mse[length(mod.before$mse)]
  
  n.before <- nrow(dat.mod.before[dat.mod.before$period == "before", ])

  
  
  tval.nbefore <- qt(0.05, n.before + n.before - 2, lower.tail = FALSE)

  mdc.nbefore <- tval.nbefore*sqrt((mse.before/n.before) + (mse.before/n.before))

  mdc.perc.nbefore[i] <- (1-(10^-mdc.nbefore))*100

}
# create a dataframe describing the residual models
mdc <- data.frame(variable = c(clean_names, "Peak discharge"),
                                 model_fit = perc.var,
                                 mdc = round(mdc.perc.nbefore, 0))

temp_filename <- file.path('data_cached', paste0(site, '_mdc.csv'))
write.csv(mdc, temp_filename)

if (nrow(mdc) == length(responses)) {
  message('Minimum detectable change has been calculated. See results in data_cached/mdc.csv')
} else {
  stop("Somethign went wrong with calculating the minimum detectable change. To debug, see code in scripts/data_analysis/1_mdc_before_after.R.")
}
