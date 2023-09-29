library(keras)

# Define inputs with predefined shape
inputs = layer_input(shape = input_shape)

# Build network with some predefined architecture
output1 = layer1(inputs)
output2 = layer2(output1)

# Predict the parameters of a lognormal distribution
outputs = layer_dense(units = 2, activation = "linear")(output2)
distribution_outputs = layer_lambda(function(x) {
  # Separate the parameters
  mu <- x[,1]
  sigma <- x[,2]
  
  # Apply an exponential activation to make positive
  sigma <- exp(sigma)
  
  # Join back together again
  c(mu, sigma)
})(outputs)

# Construct model
model <- keras_model(inputs = inputs, outputs = distribution_outputs)

# Compile the model
model %>% compile(
  loss = function(y_true, y_pred) {
    # Separate the parameters
    mu <- y_pred[,1]
    sigma <- y_pred[,2]
    
    # Calculate the negative log likelihood
    nll <- sum(dlnorm(y_true, meanlog = mu, sdlog = sigma, log = TRUE))
    
    return(nll)
  },
  optimizer = optimizer_adam()
)

# Fit the model
history <- model %>% fit(train_X, train_Y, epochs = num_epochs,
                         validation_data = list(val_X, val_Y))

# Perform inference for a single sample
pred_params <- model %>% predict(test_X)
mu <- pred_params[,1]
sigma <- pred_params[,2]

# Calculate the probability density function
y <- seq(from = min(test_Y), to = max(test_Y), by = 0.01)
probs <- dlnorm(y, meanlog = mu, sdlog = sigma)

# Calculate the expectation value
y_pred <- qlnorm(0.5, meanlog = mu, sdlog = sigma)

# Calculate the 80% confidence interval
lower <- qlnorm(0.1, meanlog = mu, sdlog = sigma)
upper <- qlnorm(0.9, meanlog = mu, sdlog = sigma)
