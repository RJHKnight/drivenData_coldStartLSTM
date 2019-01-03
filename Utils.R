library(dplyr)
library(magrittr)
library(stringr)

# Add the lagged variables.
addLaggedVariables <- function(originalDF, numTimeSteps, variableToLag) {

  
  # Create new columns based on previous steps
  for (t in 1:(numTimeSteps)) {
    thisColName <- paste0(variableToLag, ".lag.", t)
    originalDF %<>%
      mutate(lagTmp = lag(!!rlang::sym(variableToLag), n = t)) %>%
      rename(!!thisColName := lagTmp)
  }
  
  originalDF %<>%
    select(-temperature) %>%
    filter(complete.cases(.))
  
  return (originalDF)
}


fitOne <- function(thisSeriesID, allPredictors, resetStates = TRUE) {
  
  cat(paste("Training on series", thisSeriesID, "\n"))
  
  thisSeriesData <- filter(allPredictors, series_id == thisSeriesID)
  thisSeriesData <- addLaggedVariables(thisSeriesData, lag, "consumption.norm")
  
  X <- thisSeriesData %>%
    select(contains("lag")) %>%
    as.matrix()
  
  dim(X) <- c(nrow(X), 1, ncol(X))
  
  y <- thisSeriesData %>%
    pull(consumption.norm)
  
  model %>%
    fit(X, y, epochs=1, batch_size=batchSize, shuffle=FALSE, verbose = 2)
  
  if (resetStates) {
    model %>%
      reset_states()
  }
}