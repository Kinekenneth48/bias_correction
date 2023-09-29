
simulate_data_lnorm <- function(sample_size = 1000, error_sd = 0.5) {
  
  # Set the mean and covariance for each feature
  mu <- rnorm(10, mean = 5, sd = 5)
  
  # Set the covariance matrix with a range of 0 to 0.7
  cov_range <- seq(0.0, 0.7, length.out = 10)
  sigma <- diag(10) * 1.8
  for (i in 1:10) {
    for (j in 1:10) {
      if (i != j) {
        sigma[i, j] <- cov_range[abs(i - j)]
      }
    }
  }
  
  
  # Generate 1000 samples from the multivariate normal distribution
  data <- data.frame((((mvrnorm(n = sample_size, mu = mu, Sigma = sigma)))) / 10)
  
 
  # create error
  error <- rnorm(sample_size, 0, error_sd)
  
  # create y by sum x's and add error
  data$y <- exp(rowSums(data[, 1:10])) + exp(error)

  return(data)
}

