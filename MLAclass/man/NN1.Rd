\name{Neural network with 1 hidden layer}
\alias{NN1}
\title{Neural network with 1 hidden layer}
\usage{
NN1Result(measures, nfold = 10, featuresBase, labels)
}
\description{
The function fits neural network with 1 hidden layer to the data.

measures : data to fit
nfold : number of cross validation (default is 10)
featureBase : what feature(X) to use for prediction
labels : base labels of response variables (default is c("Real.Vol", "Kurtosis"))
}
\examples{
NN1Result(dataframe, featuresBase = c("Roll", "Amihud", "VPIN"))
}
