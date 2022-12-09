\name{Elastic Net penalized logistic regression lambda.min}
\alias{Elastic Net penalized logistic regression lambda.min}
\title{Elastic Net penalized logistic regression lambda.min}
\usage{
PLResults_min(measures, nfold=10, featuresBase, labels)
}
\description{
The function fits elastic net penalized logistic regression model to the data. The lambda parameter is lambda.min which is the lambda value where the cross-validated error error is the minimum.

measures : data to fit
nfold : number of cross validation (default is 10)
featureBase : what feature(X) to use for prediction
labels : base labels of response variables (default is c("Real.Vol", "Kurtosis"))
}
\examples{
PLResults_min(dataframe, featuresBase = c("Roll", "Amihud", "VPIN"))
}
