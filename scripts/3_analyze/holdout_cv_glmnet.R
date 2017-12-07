# run cv.glmnet over different hold out testing sets

run.holdout <- function(predictors, response, df) {
  #tuning.pars <- list()
  #varimps <- data.frame(var.names = predictors, imp = NA)
  coefs <- data.frame(var.names = c('intercept', predictors), coef = NA)
  r2.1se <- c()
  r2.2se <- c()
  mse.1se <- c()
  mse.2se <- c()
  df.1se <- c()
  df.2se <- c()
  hold.predict <- list()
  train.predict <- list()
  lambda.1se <- c()
  lambda.2se <- c()
  hold.mse <- c()
  train.mse <- c()
  
  
  for (i in 1:10){
    out <- sample(1:nrow(df), size = round(nrow(df)*.2, 0), replace = FALSE)
    y.train <- y[-out]
    matIVs.train <- matIVs[-out,]
    y.test <- y[out]
    matIVs.test <- matIVs[out,]
    
    
    mod <- cv.glmnet(matIVs.train, y.train, nfolds = 5)
    
    # calulcate the range of acceptable error
    # and find the maximum lambda that falls within range
    best.min.row <- which(mod$lambda == mod$lambda.min)
    best.row <- which(mod$lambda == mod$lambda.1se)
    high <- mod$cvm[best.min.row] + 2*mod$cvsd[best.min.row]
    potential.keep <- mod$lambda[which(mod$cvm <= high)]
    lambda.2se[i] <- max(potential.keep)
    lambda.2se.row <- which(mod$lambda == lambda.2se[i])
    
    lambda.1se[i] <- mod$lambda.1se
    #varimp <- varImp(mod)
    #varimps[,i+1] <- varimp$importance[,1]
    coefs[,i+1] <- as.numeric(coef(mod, s = lambda.2se[i]))
    r2.1se[i] <- mod$glmnet.fit$dev.ratio[best.row]
    r2.2se[i] <- mod$glmnet.fit$dev.ratio[lambda.2se.row]
    mse.1se[i] <- mod$cvm[best.row]
    mse.2se[i] <- mod$cvm[lambda.2se.row]
    df.1se[i] <- mod$glmnet.fit$df[best.row]
    df.2se[i] <- mod$glmnet.fit$df[lambda.2se.row]
    
    
    
    #test.dat <- as.data.frame(matIVs.test)
    #test.dat[,response] <- y.test
    hold.predict[[i]] <- data.frame(hold.obs = y.test,
                                    hold.pred = predict(mod, matIVs.test, s = lambda.1se[i]))
    hold.mse[i] <- mean((hold.predict[[i]][[1]] - hold.predict[[i]][[2]])^2)
    train.predict[[i]] <- data.frame(train.obs = y.train,
                                     train.pred = predict(mod, matIVs.train, s = lambda.1se[i]))
    train.mse[i] <- mean((train.predict[[i]][[1]] - train.predict[[i]][[2]])^2)
    
  }
  
  mod.final <- glmnet(matIVs, y, lambda = mean(lambda.1se))
  matIVs.scaled <- scale(matIVs)
  mod.final.stand <- glmnet(matIVs.scaled, y, lambda = mean(lambda.1se), standardize = FALSE)
  coefs$final_stand <- as.numeric(coef(mod.final.stand))
  coefs$final_unstand <- as.numeric(coef(mod.final))
  final.predict <- predict(mod.final, matIVs)
  final.mse <- mean((y - final.predict)^2)
  final.r2 <- mod.final$dev.ratio
  pred.df <- data.frame(observed = y,
                        predicted = final.predict)
  # mod.reduced <- cv.glmnet(matIVs, y, alpha = tuning.pars[[1]][[1]], nfolds = 5)
  # mod.reduced.noalpha <- cv.glmnet(matIVs, y, nfolds = 5)
  # 
  # #err.up <- mod$cvm + 2*mod$cvsd
  # #err.lo <- mod$cvm - 2*mod$cvsd
  # min.plus.error <- mod.reduced$cvm[which.min(mod.reduced$cvm)] + 2*mod.reduced$cvsd[which.min(mod.reduced$cvm)]
  # row.optim <- max(which(mod.reduced$cvm > min.plus.error))+1
  # lambda.optim <- mod.reduced$lambda[row.optim]
  # coef(mod.reduced, s = lambda.optim)
  
  out <- list(coefs, r2.1se, r2.2se, final.r2, 
              mse.1se, mse.2se, final.mse, df.1se, df.2se, mean(lambda.2se), pred.df)
  
  return(out)
}