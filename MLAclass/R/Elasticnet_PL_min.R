# Elastic net penalized logistic regression lambda.min

PLResults_min <- function(measures, nfold = 10, featuresBase = c("Roll", "Roll.Impact", "Kyle", "Amihud", "VPIN"),
                          labels = c("Real.Vol", "Kurtosis")) {
  # Perform purged cross-validation
  allSets <- purged.cross.validation(measures, howManyPurgeBefore = 5, howManyPurgeAfter = 5, nfold = nfold)
  # Make vector of cross-features (e.g., Roll.Small, Roll.Big,...,VPIN.Small, VPIN.Big)
  featuresCross <- c(sapply(featuresBase, function(f) paste0(f, ".Small")), sapply(featuresBase, function(f) paste0(f, ".Big")))

  sizes <- c("Big", "Small")
  types <- c("No CE", "CE")
  rocList <- list() # to store info for ROC curves

  for(ell in labels) { # loop over labels to predict (e.g., Kurtosis)
    print(ell)
    rocList[[ell]] <- list()
    for(es in sizes) { # loop over sizes to predict (e.g., Kurtosis.Big)
      print(es)
      rocList[[ell]][[es]] <- list()
      for(et in types) { # loop over whether to include cross-effects
        if(et == "No CE") {
          features <- paste0(featuresBase, ".", es)
        } else {
          features <- featuresCross
        }
        toStoreROC <- list()
        for(i in 1:nfold) { # loop over folds
          train <- allSets$Train[[i]]
          test <- allSets$Test[[i]]
          lab <- paste0("Sign", ".", ell, ".", es)
          ytrain <- train[,lab]
          cvfit <- cv.glmnet(x=as.matrix(train[,features]), y = ytrain, family = "multinomial", type.measure = "class")
          pl <- glmnet(x=train[,features], y = ytrain, family = 'multinomial', lambda=cvfit$lambda.min)
          xtest <- test[,features]
          ytest <- test[,lab]
          predictions <- as.data.frame(predict(pl, newx = as.matrix(xtest), s=cvfit$lambda.min, type="response"))
          predictions <- rename(predictions, c('-1.1'='-1', '0.1'='0', '1.1'='1'))
          predictions$Predicted <- names(predictions)[1:3][apply(predictions[,1:3], 1, which.max)]
          predictions$Actual <- ytest
          toStoreROC[[i]] <- predictions
        }
        names(toStoreROC) <- paste0("Fold", 1:nfold)
        rocList[[ell]][[es]][[et]] <- toStoreROC
      }
    }
  }
  return(rocList)
}
