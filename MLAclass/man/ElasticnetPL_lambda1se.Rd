\name{Elastic Net penalized logistic regression lambda.1se}
\alias{Elastic Net penalized logistic regression lambda.1se}
\title{Elastic Net penalized logistic regression lambda.1se}
\usage{
PLResults_1se(measures, nfold=10, featuresBase, labels)
}
\description{
The function fits elastic net penalized logistic regression model to the data. The lambda parameter is lambda.1se which is the lambda value where cross-validated error is within one standard error of the minimum.

measures : data to fit
nfold : number of cross validation (default is 10)
featureBase : what feature(X) to use for prediction
labels : base labels of response variables (default is c("Real.Vol", "Kurtosis"))
}
\examples{
PLResults_1se(dataframe, featuresBase = c("Roll", "Amihud", "VPIN"))
}
