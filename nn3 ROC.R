#NN3
NN3result <- function(measures, nfold = 10, featuresBase = c("Roll", "Roll.Impact", "Kyle", "Amihud", "VPIN"),
                      labels = c("Real.Vol", "Kurtosis")) {
  # Perform purged cross-validation 
  allSets <- purged.cross.validation(measures, howManyPurgeBefore = 5, howManyPurgeAfter = 5, nfold = nfold) 
  # Make vector of cross-features (e.g., Roll.Small, Roll.Big,...,VPIN.Small, VPIN.Big)
  featuresCross <- c(sapply(featuresBase, function(f) paste0(f, ".Small")), sapply(featuresBase, function(f) paste0(f, ".Big")))
  
  sizes <- c("Big", "Small")
  types <- c("No CE", "CE")
  rocList <- list() 
  for(ell in labels) { # loop over labels to predict (e.g., Kurtosis)
    print(ell)
    rocList[[ell]] <- list()
    for(es in sizes) { # loop over sizes to predict (e.g., Kurtosis.Big)
      print(es)
      rocList[[ell]][[es]] <- list()
      for(et in types) { # loop over whether to include cross-effects
        if(et == "No CE") {
          features <- paste0(featuresBase, ".", es)
          print(et)
          print(features)
        } else {
          features <- featuresCross
          print(et)
          print(features)
        }
        toStoreROC <- list()
        for(i in 1:nfold) { # loop over folds
          train <- allSets$Train[[i]] 
          test <- allSets$Test[[i]] 
          lab <- paste0("Sign", ".", ell, ".", es) 
          ytrain <- train[,lab]
          xtrain <- train[,features]
          subs <- data.frame(cbind(ytrain, xtrain))
          lay <- ((ncol(subs)-1)/3)^(1/4)
          nn3 <- neuralnet(ytrain~., data=subs, hidden=c(floor(3*lay^3), floor(3*lay^2), floor(3*lay)), threshold = 0.4, linear.output = FALSE) 
          xtest <- test[,features]
          ytest <- test[,lab]
          predictions <- as.data.frame(predict(nn3, xtest))
          predictions <- rename(predictions, c(V1='-1', V2='0', V3='1'))
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

nn3Res <- NN3result(new, featuresBase = c("Roll", "Amihud", "VPIN")) 

#------- Realized volatility - big firms--------------
noCE <- do.call(rbind, nn3Res$Real.Vol$Big$`No CE`)
withCE <- do.call(rbind, nn3Res$Real.Vol$Big$CE)
rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))

##without cross effect each ROC curve
plot(rocNoCE_neg, xaxt="n",legacy.axes = T, main = "NN3", col="red")
plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)), 
                                 paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)), 
                                 paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

##with cross effect each ROC curve
plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN3", col="red")
plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)), 
                                 paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)), 
                                 paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

#------- Realized volatility - small firms--------------
noCE <- do.call(rbind, nn3Res$Real.Vol$Small$`No CE`)
withCE <- do.call(rbind, nn3Res$Real.Vol$Small$CE)
rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))

##without cross effect each ROC curve
plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "NN3", col="red")
plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)), 
                                 paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)), 
                                 paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

##with cross effect each ROC curve
plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN3", col="red")
plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)), 
                                 paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)), 
                                 paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

#------- Kurtosis - Large firms--------------
noCE <- do.call(rbind, nn3Res$Kurtosis$Big$`No CE`)
withCE <- do.call(rbind, nn3Res$Kurtosis$Big$CE)
rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))

##without cross effect each ROC curve
plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "NN3", col="red")
plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)), 
                                 paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)), 
                                 paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)



##with cross effect each ROC curve
plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN3", col="red")
plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)), 
                                 paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)), 
                                 paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

#------- Kurtosis - small firms--------------
noCE <- do.call(rbind, nn3Res$Kurtosis$Small$`No CE`)
withCE <- do.call(rbind, nn3Res$Kurtosis$Small$CE)
rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))

##without cross effect each ROC curve
plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "NN3", col="red")
plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)), 
                                 paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)), 
                                 paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)



##with cross effect each ROC curve
plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN3", col="red")
plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)), 
                                 paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)), 
                                 paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
