## Installing all packages needed

You can download the `MLAclass` package by devtools::install\_github
function. `MLAclass` package contains the functions that fit each model
on the high-frequency data. The models are Elatic-Net penalized logistic
regression, gradient boosting, random forest, and feed-forward neural
network.

    devtools::install_github("sangheekim96/statcomputing/MLAclass")
    install.packages(c('pROC', 'glmnet', 'gbm', 'randomForest', 'neuralnet', 'ggplot2', 'RColorBrewer', 'stringer', 'dplyr'))
    library(pROC)
    library(dplyr)
    library(plyr)
    library(ggplot2)
    library(RColorBrewer)
    library(stringr)
    library(glmnet)
    library(gbm)
    library(randomForest)
    library(neuralnet)
    library(MLAclass)

## Downloading the data

The data used for this analysis is stored in data folder. After
downloading it to your computer and setting the appropriate working
directory, you may use load function as below.

    load("average_measures_for_financials_Deciles_1_and_7.RData")

## Description of `MLAclass` package

The `MLAclass` package consists of 10 functions-purged.cross.validation,
PLResults\_min, PLResults\_1se, GBResults, RFresult, NN1result,
NN3result, NN3result, NN4result, NN5result.`purged.cross.validation`
function does purged cross validation on the preprocessed data by adding
some data points before and after our selected test interval. each
result function prints a list of predicted sign of realized volatility
and kurtosis. If cross features are considered, there will be 6 features
to predict the response variable and if cross features are not
considered, only 3 features will be used to predict.

## Data preprocessing

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

## Fit each ML model

    plRes_min <- PLResults_min(new, featuresBase = c("Roll", "Amihud", "VPIN"))
    plRes_1se <- PLResults_1se(new, featuresBase = c("Roll", "Amihud", "VPIN"))
    gbRes <- GBResults(new, featuresBase = c("Roll", "Amihud", "VPIN"))
    rfRes <- RFresult(new, featuresBase = c("Roll", "Amihud", "VPIN"))
    nn1Res <- NN1result(new, featuresBase = c("Roll", "Amihud", "VPIN"))
    nn2Res <- NN2result(new, featuresBase = c("Roll", "Amihud", "VPIN"))
    nn3Res <- NN3result(new, featuresBase = c("Roll", "Amihud", "VPIN"))
    nn4Res <- NN4result(new, featuresBase = c("Roll", "Amihud", "VPIN"))
    nn5Res <- NN5result(new, featuresBase = c("Roll", "Amihud", "VPIN"))

## Plot ROC curves

For each ML model, ROC curves are plotted for 8 different scenarios.

1.  Predicting the sign of realized volatility for large firms - no CE
2.  Predicting the sign of realized volatility for large firms - CE
3.  Predicting the sign of realized volatility for small firms - no CE
4.  Predicting the sign of realized volatility for small firms - CE
5.  Predicting the sign of kurtosis for large firms - no CE
6.  Predicting the sign of krutosis for large firms - CE
7.  Predicting the sign of kurtosis for small firms - no CE
8.  Predicting the sign of krutosis for small firms - CE

Hence there will be 72 ROC curves printed.

    ############ 1. elastic net penalized logistic lambda.min
    #------- Realized volatility - big firms--------------
    noCE <- do.call(rbind, plRes_min$Real.Vol$Big$`No CE`)
    withCE <- do.call(rbind, plRes_min$Real.Vol$Big$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n",legacy.axes = T, main = "PL lambda.min", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "PL lambda.min", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Realized volatility - small firms--------------
    noCE <- do.call(rbind, plRes_min$Real.Vol$Small$`No CE`)
    withCE <- do.call(rbind, plRes_min$Real.Vol$Small$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "PL lambda.min", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "PL lambda.min", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Kurtosis - Large firms--------------
    noCE <- do.call(rbind, plRes_min$Kurtosis$Big$`No CE`)
    withCE <- do.call(rbind, plRes_min$Kurtosis$Big$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "PL lambda.min", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "PL lambda.min", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Kurtosis - small firms--------------
    noCE <- do.call(rbind, plRes_min$Kurtosis$Small$`No CE`)
    withCE <- do.call(rbind, plRes_min$Kurtosis$Small$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "PL lambda.min", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "PL lambda.min", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)


    ############ 2. elastic net penalized logistic lambda.1se
    #------- Realized volatility - big firms--------------
    noCE <- do.call(rbind, plRes_1se$Real.Vol$Big$`No CE`)
    withCE <- do.call(rbind, plRes_1se$Real.Vol$Big$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n",legacy.axes = T, main = "PL lambda.1se", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "PL lambda.1se", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Realized volatility - small firms--------------
    noCE <- do.call(rbind, plRes_1se$Real.Vol$Small$`No CE`)
    withCE <- do.call(rbind, plRes_1se$Real.Vol$Small$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "PL lambda.1se", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "PL lambda.1se", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Kurtosis - Large firms--------------
    noCE <- do.call(rbind, plRes_1se$Kurtosis$Big$`No CE`)
    withCE <- do.call(rbind, plRes_1se$Kurtosis$Big$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "PL lambda.1se", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "PL lambda.1se", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Kurtosis - small firms--------------
    noCE <- do.call(rbind, plRes_1se$Kurtosis$Small$`No CE`)
    withCE <- do.call(rbind, plRes_1se$Kurtosis$Small$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "PL lambda.1se", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "PL lambda.1se", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    ############ 3. gradient boosting
    #------- Realized volatility - big firms--------------
    noCE <- do.call(rbind, gbRes$Real.Vol$Big$`No CE`)
    withCE <- do.call(rbind, gbRes$Real.Vol$Big$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n",legacy.axes = T, main = "GB", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.1)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "GB", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.1)

    #------- Realized volatility - small firms--------------
    noCE <- do.call(rbind, gbRes$Real.Vol$Small$`No CE`)
    withCE <- do.call(rbind, gbRes$Real.Vol$Small$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "GB", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.1)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "GB", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.1)

    #------- Kurtosis - Large firms--------------
    noCE <- do.call(rbind, gbRes$Kurtosis$Big$`No CE`)
    withCE <- do.call(rbind, gbRes$Kurtosis$Big$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "GB", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "GB", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Kurtosis - small firms--------------
    noCE <- do.call(rbind, gbRes$Kurtosis$Small$`No CE`)
    withCE <- do.call(rbind, gbRes$Kurtosis$Small$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "GB", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "GB", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)


    ############ 4. random forest
    #------- Realized volatility - big firms--------------
    noCE <- do.call(rbind, rfRes$Real.Vol$Big$`No CE`)
    withCE <- do.call(rbind, rfRes$Real.Vol$Big$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n",legacy.axes = T, main = "RF", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "RF", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Realized volatility - small firms--------------
    noCE <- do.call(rbind, rfRes$Real.Vol$Small$`No CE`)
    withCE <- do.call(rbind, rfRes$Real.Vol$Small$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "RF", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "RF", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Kurtosis - Large firms--------------
    noCE <- do.call(rbind, rfRes$Kurtosis$Big$`No CE`)
    withCE <- do.call(rbind, rfRes$Kurtosis$Big$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "RF", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "RF", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Kurtosis - small firms--------------
    noCE <- do.call(rbind, rfRes$Kurtosis$Small$`No CE`)
    withCE <- do.call(rbind, rfRes$Kurtosis$Small$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "RF", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "Predicting Kurtosis for Small Firms \nw/ cross-features", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    ############ 5. neural network 1
    #------- Realized volatility - big firms--------------
    noCE <- do.call(rbind, nn1Res$Real.Vol$Big$`No CE`)
    withCE <- do.call(rbind, nn1Res$Real.Vol$Big$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##no CE & CE compare
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


    ############ 6. neural network 2
    #------- Realized volatility - big firms--------------
    noCE <- do.call(rbind, nn2Res$Real.Vol$Big$`No CE`)
    withCE <- do.call(rbind, nn2Res$Real.Vol$Big$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n",legacy.axes = T, main = "NN2", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN2", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Realized volatility - small firms--------------
    noCE <- do.call(rbind, nn2Res$Real.Vol$Small$`No CE`)
    withCE <- do.call(rbind, nn2Res$Real.Vol$Small$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "NN2", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN2", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Kurtosis - Large firms--------------
    noCE <- do.call(rbind, nn2Res$Kurtosis$Big$`No CE`)
    withCE <- do.call(rbind, nn2Res$Kurtosis$Big$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "NN2", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN2", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Kurtosis - small firms--------------
    noCE <- do.call(rbind, nn2Res$Kurtosis$Small$`No CE`)
    withCE <- do.call(rbind, nn2Res$Kurtosis$Small$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "NN2", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN2", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)


    ############ 7. neural network 3
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


    ############ 8. neural network 4
    #------- Realized volatility - big firms--------------
    noCE <- do.call(rbind, nn4Res$Real.Vol$Big$`No CE`)
    withCE <- do.call(rbind, nn4Res$Real.Vol$Big$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n",legacy.axes = T, main = "NN4", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN4", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Realized volatility - small firms--------------
    noCE <- do.call(rbind, nn4Res$Real.Vol$Small$`No CE`)
    withCE <- do.call(rbind, nn4Res$Real.Vol$Small$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "NN4", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN4", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Kurtosis - Large firms--------------
    noCE <- do.call(rbind, nn4Res$Kurtosis$Big$`No CE`)
    withCE <- do.call(rbind, nn4Res$Kurtosis$Big$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "NN4", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN4", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Kurtosis - small firms--------------
    noCE <- do.call(rbind, nn4Res$Kurtosis$Small$`No CE`)
    withCE <- do.call(rbind, nn4Res$Kurtosis$Small$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "NN4", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN4", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)


    ############ 9. neural network 5
    #------- Realized volatility - big firms--------------
    noCE <- do.call(rbind, nn5Res$Real.Vol$Big$`No CE`)
    withCE <- do.call(rbind, nn5Res$Real.Vol$Big$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n",legacy.axes = T, main = "NN5", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN5", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Realized volatility - small firms--------------
    noCE <- do.call(rbind, nn5Res$Real.Vol$Small$`No CE`)
    withCE <- do.call(rbind, nn5Res$Real.Vol$Small$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "NN5", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN5", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Kurtosis - Large firms--------------
    noCE <- do.call(rbind, nn5Res$Kurtosis$Big$`No CE`)
    withCE <- do.call(rbind, nn5Res$Kurtosis$Big$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "NN5", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN5", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)

    #------- Kurtosis - small firms--------------
    noCE <- do.call(rbind, nn5Res$Kurtosis$Small$`No CE`)
    withCE <- do.call(rbind, nn5Res$Kurtosis$Small$CE)
    rocNoCE_neg <- roc(response = ifelse(noCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(noCE$`-1`))
    rocNoCE_neu <- roc(response = ifelse(noCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(noCE$`0`))
    rocNoCE_pos <- roc(response = ifelse(noCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(noCE$`1`))
    rocWithCE_neg <- roc(response = ifelse(withCE$Actual=='-1', 'negative','non-negative'), predictor = as.numeric(withCE$`-1`))
    rocWithCE_neu <- roc(response = ifelse(withCE$Actual=='0', 'neutral','non-neutral'), predictor = as.numeric(withCE$`0`))
    rocWithCE_pos <- roc(response = ifelse(withCE$Actual=='1', 'positive','non-positive'), predictor = as.numeric(withCE$`1`))
    ##without cross effect each ROC curve
    plot(rocNoCE_neg, xaxt="n", legacy.axes = T, main = "NN5", col="red")
    plot(rocNoCE_neu, legacy.axes = T, add = T, xacol = "black")
    plot(rocNoCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocNoCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocNoCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocNoCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
    ##with cross effect each ROC curve
    plot(rocWithCE_neg, xaxt="n", legacy.axes = T, main = "NN5", col="red")
    plot(rocWithCE_neu, legacy.axes = T, add = T, col = "black")
    plot(rocWithCE_pos, legacy.axes = T, add = T, col = "blue")
    axis(1, at=c(0,0.5,1), labels=c(1,0.5,0), pos=-0.04)
    legend("bottomright", legend = c(paste("AUC (negative) =", round(auc(rocWithCE_neg),3)),
                                     paste("AUC (neutral) =", round(auc(rocWithCE_neu),3)),
                                     paste("AUC (positive) =", round(auc(rocWithCE_pos),3))), fill = c("red", "black","blue"), bty = "n", cex=1.2)
