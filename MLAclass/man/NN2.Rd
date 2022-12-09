\name{Neural network with 2 hidden layers}
\alias{NN2}
\title{Neural network with 2 hidden layers}
\usage{
NN2Result(measures, nfold = 10, featuresBase, labels)
}
\description{
The function fits neural network with 2 hidden layers to the data.

measures : data to fit
nfold : number of cross validation (default is 10)
featureBase : what feature(X) to use for prediction
labels : base labels of response variables (default is c("Real.Vol", "Kurtosis"))
}
\examples{
NN2Result(dataframe, featuresBase = c("Roll", "Amihud", "VPIN"))
}
