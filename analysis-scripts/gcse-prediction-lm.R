# libs ----------------------------------
library(tidyverse)
library(caret)

# data ----------------------------------

gcses <- read_csv("data/gcses-cleaned.csv") %>% 
  filter(school_type == "State School") %>% 
  select(num_pupils, gender, perc_fsm, perc_sen, perc_sec_lang, att8_17, att8) %>% 
  drop_na()

# data prep -----------------------------

set.seed(2115)

trainIndex <- createDataPartition(gcses$att8, p = .7, list = FALSE)

training <- gcses[ trainIndex,]

test <- gcses[-trainIndex,]

# training ------------------------------

gcse_fit <- train(att8 ~ .,
                  data = training, 
                  method = "lm")

summary(gcse_fit)

# prediction -----------------------------

predict <- predict(gcse_fit, test)

postResample(pred = predict, obs = test$att8)

# plots ----------------------------------

residuals <- resid(gcse_fit)

plot(training$att8, residuals)

abline(0,0)

plot(test$att8, predict)


