# Define the custom log normal layer
log_normal_layer <- function(x){
  # Get the number of dimensions of the input
  num_dims <- length(dim(x))
  
  # Separate the parameters
 # mu, sigma <- unlist(x, use.names = FALSE)
  
  # Add one dimension to make the right shape
  mu <- expand_dims(mu, dim(mu) + 1)
  sigma <- expand_dims(sigma, dim(sigma) + 1)
  
  # Apply a softplus to make positive
  sigma <- softplus(sigma)
  
  # Join back together again
  out_tensor <- c(mu, sigma)
  
  return (out_tensor)
}

# Define inputs with predefined shape
inputs <- layer_input(shape = input_shape)

# Build network with some predefined architecture
output1 <- Layer1(inputs)
output2 <- Layer2(output1)

# Predict the parameters of a log normal distribution
outputs <- layer_dense(2, activation = "linear")(output2)
distribution_outputs <- layer_lambda(log_normal_layer)(outputs)

# Construct model
model <- keras_model(inputs = inputs, outputs = distribution_outputs)

# Define the log normal loss function
log_normal_loss <- function(y_true, y_pred){
  # Separate the parameters
 # mu, sigma <- unlist(y_pred, use.names = FALSE)
  
  # Add one dimension to make the right shape
  mu <- expand_dims(mu, dim(mu) + 1)
  sigma <- expand_dims(sigma, dim(sigma) + 1)
  
  # Calculate the negative log likelihood
  nll <- -dlnorm(y_true, meanlog = mu, sdlog = sigma, log = TRUE)
  
  return (nll)
}

# Compile the model with the custom loss function and optimizer
model %>% compile(
  loss = log_normal_loss,
  optimizer = optimizer_adam()
)

# Fit the model to the training data
history <- model %>% fit(
  train_X, train_Y,
  epochs = num_epochs,
  validation_data = list(val_X, val_Y)
)

# Perform inference for a single sample
pred_params <- model %>% predict(test_X)
mu <- pred_params[, 1]
sigma <- pred_params[, 2]

# Calculate the probability density function
y <- seq(min(y_true), max(y_true), length.out = 100)
probs <- dlnorm(y, meanlog = mu, sdlog = sigma)

# Calculate the expectation value
y_pred <- qlnorm(0.5, meanlog = mu, sdlog = sigma)

# Calculate the 80% confidence interval
lower <- qlnorm(0.1, meanlog = mu, sdlog = sigma)
upper <- qlnorm(0.9, meanlog = mu, sdlog = sigma)
