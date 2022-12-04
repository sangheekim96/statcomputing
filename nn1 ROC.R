library(pROC)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(plyr)
library(dplyr)
library(tidyverse)
library(neuralnet)
library(caret)
options(scipen=3)

load("average_measures_for_financials_Deciles_1_and_7.RData") # load("average_measures_for_ict_sector_Deciles_1_and_7.RData") #load("average_measures_for_all_sectors_Deciles_1_and_7.RData")
str(avgMeasures)

new <- data.frame(avgMeasures[,-(19:22)])
val <- avgMeasures[,c(8:9,17:18)]
musigma11 <- mean(val$`Change Real Vol.Big`)+sd(val$`Change Real Vol.Big`)
musigma12 <- mean(val$`Change Real Vol.Big`)-sd(val$`Change Real Vol.Big`)
musigma21 <- mean(val$`Change Real Vol.Small`)+sd(val$`Change Real Vol.Small`)
musigma22 <- mean(val$`Change Real Vol.Small`)-sd(val$`Change Real Vol.Small`)
musigma31 <- mean(val$`Change Kurtosis.Big`)+sd(val$`Change Kurtosis.Big`)
musigma32 <- mean(val$`Change Kurtosis.Big`)-sd(val$`Change Kurtosis.Big`)
musigma41 <- mean(val$`Change Kurtosis.Small`)+sd(val$`Change Kurtosis.Small`)
musigma42 <- mean(val$`Change Kurtosis.Small`)-sd(val$`Change Kurtosis.Small`)

val <- transform(val, 'Sign Real Vol.Big' = as.factor(ifelse(`Change Real Vol.Big` < musigma12, -1, 
                                                             ifelse(`Change Real Vol.Big` > musigma11, 1, 0))),
                 'Sign Real Vol.Small' = as.factor(ifelse(`Change Real Vol.Small` < musigma22, -1, 
                                                          ifelse(`Change Real Vol.Small` > musigma21, 1, 0))), 
                 'Sign Kurtosis.Big' = as.factor(ifelse(`Change Kurtosis.Big` < musigma32, -1, 
                                                        ifelse(`Change Kurtosis.Big` > musigma31, 1, 0))),
                 'Sign Kurtosis.Small' = as.factor(ifelse(`Change Kurtosis.Small` < musigma42, -1, 
                                                          ifelse(`Change Kurtosis.Small` > musigma41, 1, 0))))
new <- cbind(new, val[,5:8])
summary(new)


#purged cross validation
purged.cross.validation <- function(X, nfold = 10, howManyPurgeBefore = 3, howManyPurgeAfter = 3) {
  # Split the sample into nfold intervals
  foldIDs <- cut(1:nrow(X), nfold, FALSE)
  foldCuts <- c( order(foldIDs)[!duplicated(sort(foldIDs))], nrow(X) )
  foldCuts[nfold + 1] <- foldCuts[nfold + 1] + 1
  # Get all trading days in the sample
  allDates <- unique( c(X$BEGINDATE, X$ENDDATE) ) 
  n <- length(allDates)
  sets <- list("Train" = list(), "Test" = list())
  for(i in 1:nfold) { 
    test <- X[ foldCuts[i]:(foldCuts[i+1] - 1), ] # hold out one interval at a time as the test set
    sets$Test[[i]] <- test
    # Purge approximately one week's worth of data around the test set
    # Remove howManyPurgeBefore days from the front and howManyPurgeAfter days from the back
    onewkbefore <- allDates[ max(1, which(allDates == test[1,'BEGINDATE']) - howManyPurgeBefore) ]
    onewkafter <- allDates[ min(n, which(allDates == test[nrow(test),'ENDDATE']) + howManyPurgeAfter) ]
    sets$Train[[i]] <- X[X$ENDDATE < onewkbefore | X$BEGINDATE > onewkafter,] 
  } 
  sets
}

#NN1
NN1result <- function(measures, nfold = 10, featuresBase = c("Roll", "Roll.Impact", "Kyle", "Amihud", "VPIN"),
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
          lay <- floor(sqrt((ncol(subs)-1)*3))
          nn1 <- neuralnet(ytrain~., data=subs, hidden=lay, threshold = 0.4, linear.output = FALSE) 
          xtest <- test[,features]
          ytest <- test[,lab]
          predictions <- as.data.frame(predict(nn1, xtest, type="prob"))
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

nn1Res <- NN1result(new, featuresBase = c("Roll", "Amihud", "VPIN")) 

#------- Realized volatility - big firms--------------
noCE <- do.call(rbind, nn1Res$Real.Vol$Big$`No CE`)
withCE <- do.call(rbind, nn1Res$Real.Vol$Big$CE)
rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))

##no CE & CE comapre
plot(rocNoCE_pos, xaxt="n",legacy.axes = T, main = "", col="blue")
plot(rocWithCE_pos, legacy.axes = T, add = T, xacol = "black")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (noCE) =", round(auc(rocNoCE_pos),3)), 
                                 paste("AUC (CE) =", round(auc(rocWithCE_pos),3))), 
                                 fill = c("blue", "black"), bty = "n", cex=1.2)



##without cross effect each ROC curve
plot(rocNoCE_neg, xaxt="n",legacy.axes = T, main = "NN1", col="red")
plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)), 
                                 paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)), 
                                 paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

##with cross effect each ROC curve
plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN1", col="red")
plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)), 
                                 paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)), 
                                 paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

#------- Realized volatility - small firms--------------
noCE <- do.call(rbind, nn1Res$Real.Vol$Small$`No CE`)
withCE <- do.call(rbind, nn1Res$Real.Vol$Small$CE)
rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))

##without cross effect each ROC curve
plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "NN1", col="red")
plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)), 
                                 paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)), 
                                 paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

##with cross effect each ROC curve
plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN1", col="red")
plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)), 
                                 paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)), 
                                 paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

#------- Kurtosis - Large firms--------------
noCE <- do.call(rbind, nn1Res$Kurtosis$Big$`No CE`)
withCE <- do.call(rbind, nn1Res$Kurtosis$Big$CE)
rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))

##without cross effect each ROC curve
plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "NN1", col="red")
plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)), 
                                 paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)), 
                                 paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)



##with cross effect each ROC curve
plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN1", col="red")
plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)), 
                                 paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)), 
                                 paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

#------- Kurtosis - small firms--------------
noCE <- do.call(rbind, nn1Res$Kurtosis$Small$`No CE`)
withCE <- do.call(rbind, nn1Res$Kurtosis$Small$CE)
rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))

##without cross effect each ROC curve
plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "NN1", col="red")
plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)), 
                                 paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)), 
                                 paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

##with cross effect each ROC curve
plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN1", col="red")
plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)), 
                                 paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)), 
                                 paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

