library(tensorflow)
library(keras)
library(tidyverse)
library(tidymodels)
library(recipes)

use_virtualenv('r-reticulate')
tensorflow::tf_config()
reticulate::py_config()

###################################################################
#get data
###################################################################


url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"
col_names <- c("mpg","cylinders","displacement","horsepower","weight","acceleration","model_year", "origin","car_name")

raw_dataset <- read.table(
  url,
  header = T,
  col.names = col_names,
  na.strings = "?"
)

dataset <- raw_dataset %>% select(-car_name)
tail(dataset)

###################################################################
#clean data
###################################################################

lapply(dataset, function(x) sum(is.na(x))) %>% str()
dataset <- na.omit(dataset)



dataset <- recipe(mpg ~ ., dataset) %>%
  step_num2factor(origin, levels = c("USA", "Europe", "Japan")) %>%
  step_dummy(origin, one_hot = TRUE) %>%
  prep() %>%
  bake(new_data = NULL)


###################################################################
#split data
###################################################################
split <- initial_split(dataset, 0.8)
train_dataset <- training(split)
test_dataset <- testing(split)


###################################################################
#inspect data
###################################################################
train_dataset %>%
  select(mpg, cylinders, displacement, weight) %>%
  GGally::ggpairs()


train_features <- train_dataset %>% select(-mpg)
test_features <- test_dataset %>% select(-mpg)

train_labels <- train_dataset %>% select(mpg)
test_labels <- test_dataset %>% select(mpg)


###################################################################
#normalization
###################################################################
my_skim <- skimr::skim_with(numeric = skimr::sfl(mean, sd))
train_dataset %>%
  select(where(~is.numeric(.x))) %>%
  pivot_longer(
    cols = everything(), names_to = "variable", values_to = "values") %>%
  group_by(variable) %>%
  summarise(mean = mean(values), sd = sd(values))

###################################################################
#layer 
###################################################################
normalizer <- keras::layer_normalization(axis = -1L)


horsepower <- matrix(train_features$horsepower)
horsepower_normalizer <- keras::layer_normalization(input_shape = shape(1), 
                                             axis = NULL)
horsepower_normalizer %>% adapt(horsepower)


###################################################################
#Build the Keras Sequential model
###################################################################

horsepower_model <- keras_model_sequential() %>%
  horsepower_normalizer() %>%
  layer_dense(units = 1)

summary(horsepower_model)


horsepower_model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.1),
  loss = 'mean_absolute_error'
)

history <- horsepower_model %>% fit(
  as.matrix(train_features$horsepower),
  as.matrix(train_labels),
  epochs = 100,
  # Suppress logging.
  verbose = 0,
  # Calculate validation results on 20% of the training data.
  validation_split = 0.2
)

plot(history)



test_results <- list()
test_results[["horsepower_model"]] <- horsepower_model %>% evaluate(
  as.matrix(test_features$horsepower),
  as.matrix(test_labels),
  verbose = 0
)

x <- seq(0, 250, length.out = 251)
y <- predict(horsepower_model, x)


ggplot(train_dataset) +
  geom_point(aes(x = horsepower, y = mpg, color = "data")) +
  geom_line(data = data.frame(x, y), aes(x = x, y = y, color = "prediction"))
