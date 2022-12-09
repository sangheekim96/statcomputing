\name{Random forest}
\alias{Random forest}
\title{Randomf forest}
\usage{
RFResult(measures, nfold = 10, ntrees = 1000, featuresBase, labels)
}
\description{
The function fits random forest model to the data.

measures : data to fit
nfold : number of cross validation (default is 10)
ntrees : number of trees to fit (default is 1000)
featureBase : what feature(X) to use for prediction
labels : base labels of response variables (default is c("Real.Vol", "Kurtosis"))
}
\examples{
RFResult(dataframe, featuresBase = c("Roll", "Amihud", "VPIN"))
}
