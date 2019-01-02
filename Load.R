library(readr)
library(tidyverse)
library(magrittr)

# Load the training data - skip the first column (row num I guess...)
consumption_train <- read_csv("consumption_train.csv", 
                              col_types = cols(X1 = col_skip(), temperature = col_number(), 
                                               timestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

# Load the meta
meta <- read_csv("meta.csv", col_types = cols(friday_is_day_off = col_logical(), 
                                              monday_is_day_off = col_logical(), saturday_is_day_off = col_logical(), 
                                              sunday_is_day_off = col_logical(), thursday_is_day_off = col_logical(), 
                                              tuesday_is_day_off = col_logical(), wednesday_is_day_off = col_logical()))

# Test details
cold_start_test <- read_csv("cold_start_test.csv", 
                            col_types = cols(X1 = col_skip(), timestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S")))


# And the submission details...
submission_format <- read_csv("submission_format.csv", 
                              col_types = cols(pred_id = col_skip(), 
                                               timestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S")))


# Distribution of required periods
submission_format %>%
  group_by(series_id) %>%
  summarise(predictionType = prediction_window[1]) %>%
  ungroup() %>%
  group_by(predictionType) %>%
  summarise(numPrediction = length(series_id))

# Plot of frequency of length of the period
cold_start_test %>%
  group_by(series_id) %>%
  summarise(numPeriods = length(timestamp)/24) %>%
  ungroup() %>%
  group_by(numPeriods) %>%
  summarise(count = length(series_id)) %>%
  ungroup() %>%
  ggplot(., aes(numPeriods, count)) + 
  geom_col() 


# Add prediction window to test data
predictionWindowByID <- submission_format %>%
  group_by(series_id) %>%
  summarise(predictionWindow = prediction_window[1]) %>%
  ungroup()

cold_start_test %<>%
  left_join(predictionWindowByID, by = "series_id")

# Repeat the plot by predictionWindow as well
cold_start_test %>%
  group_by(series_id, predictionWindow) %>%
  summarise(numPeriods = length(timestamp)/24) %>%
  ungroup() %>%
  group_by(numPeriods, predictionWindow) %>%
  summarise(count = length(series_id)) %>%
  ungroup() %>%
  ggplot(., aes(numPeriods, count, fill = predictionWindow)) + 
  geom_col(position = "dodge") 


# Replicate the plot
seriesToPlot <- sample(unique(cold_start_test$series_id), 40)

cold_start_test %>%
  left_join(submission_format, by = "series_id") %>%
  filter(series_id %in% seriesToPlot) %>%
  group_by(series_id) %>%
  summarise(
    min_ColdStart = min(timestamp.x),
    max_ColdStart = max(timestamp.x),
    min_Prediction = min(timestamp.y),
    max_Prediction = max(timestamp.y),
    randomVariable = runif(1)
  ) %>%
  ungroup() %>%
  gather(variable, value, -series_id, -randomVariable) %>%
  separate(variable, c("dateType", "predictionType")) %>%
  spread(dateType, value) %>%
  ggplot(.) + 
  geom_rect(mapping=aes(xmin=min, xmax=max, ymin=randomVariable-0.02, ymax=randomVariable+0.02, fill=predictionType)) + 
  geom_label(aes(label = series_id, x = min, y = randomVariable-0.02))

# Hmm... some series have quite a long gap - for example, end of cold start = 2013-10-08, start of submission = 2013-10-15
summary(filter(cold_start_test, series_id == 102171))
summary(filter(submission_format, series_id == 102171))
