# Purged Cross Validation

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
