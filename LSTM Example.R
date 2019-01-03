library(keras)
library(tidyverse)
library(recipes)

#source("Load.R")
source("Utils.R")

# Scale consumption to [-1,1], normally I'd use recipes for this but since its such a trivial example we can just do it manually.
maxConsumption <- max(consumption_train$consumption)
minConsumption <- min(consumption_train$consumption)
rangeConsumption <- 0.5 * (maxConsumption - minConsumption)

consumption_train <- mutate(consumption_train, consumption.norm = (consumption / rangeConsumption) - 1)

# Add the lagged columns
# consumption_train <- addLaggedVariables(consumption_train, 5, "consumption.norm")

# Create the model
lag <- 24
numNeurons <- 24
batchSize <- 1
batchInputShape <- c(batchSize, 1, lag)

model <- keras_model_sequential()

model %>%
  layer_lstm(units = numNeurons, batch_input_shape = batchInputShape, stateful = TRUE) %>%
  layer_dense(units = 1)

model %>%
  compile(loss='mean_absolute_error', optimizer='adam')

epochs <- 1

for (i in 1:epochs) {
  
  cat(paste("****** Starting Epoch", i, " ******\n"))
  sapply(unique(consumption_train$series_id), function(x) fitOne(x, consumption_train))
}

# Now apply the prediction... I'm not too concerned about the aggregation here as we just want an idea of the performance.
for (seriesID in unique(cold_start_test$series_id)[1]) {
  
  model %>%
    reset_states()
  
  # First train on cold start data
  cat(paste("Training on series", seriesID, "\n"))
  
  thisSeriesData <- filter(cold_start_test, series_id == seriesID)
  thisSeriesData <- mutate(thisSeriesData, consumption.norm = (consumption / rangeConsumption) - 1)
  thisSeriesData <- addLaggedVariables(thisSeriesData, lag, "consumption.norm")
  
  X <- thisSeriesData %>%
    select(contains("lag")) %>%
    as.matrix()
  
  dim(X) <- c(nrow(X), 1, ncol(X))
  
  y <- thisSeriesData %>%
    pull(consumption.norm)
  
  model %>%
    fit(X, y, epochs=1, batch_size=batchSize, shuffle=FALSE)
  
  # Predict for 24 hours.
  for (i in 1:24) {

    # Back to 2d for manipulation
    dim(X) <- c(dim(X)[1], dim(X)[3])
    
    thisX <- tail(X,1)
    dim(thisX) <- c(1,1,ncol(X))

    model %>%
      predict(thisX, epochs=1, batch_size=batchSize, shuffle=FALSE)
    
  }
}



