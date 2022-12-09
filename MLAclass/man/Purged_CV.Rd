\name{Purged Cross Validation}
\alias{Purged Cross Validation}
\title{Purged Cross Validation}
\usage{
purged.cross.validation(X, nfold, howManyPurgeBefore, howManyPurgeAfter)
}
\description{
The function does pruged cross validation to high frequency data

X : data set you want to split
nfold : how many intervals to sample into
howManyPurgeBefore : how many intervals to purge before
howManyPurgeAfter : how many intervals to purge after
}
\examples{
purged.cross.validation(allSets, 10, 3, 3)
}
