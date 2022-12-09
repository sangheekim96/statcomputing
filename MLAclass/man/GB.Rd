\name{Gradient boosting}
\alias{Gradient boosting}
\title{Gradient boosting}
\usage{
GBResults(measures, nfold = 10, ntrees = 1000, featuresBase, labels)
}
\description{
The function fits gradient boosting model to the data.

measures : data to fit
nfold : number of cross validation (default is 10)
ntrees : number of trees to fit (default is 1000)
featureBase : what feature(X) to use for prediction
labels : base labels of response variables (default is c("Real.Vol", "Kurtosis"))
}
\examples{
GBResults(dataframe, featuresBase = c("Roll", "Amihud", "VPIN"))
}
