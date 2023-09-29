

simulate_gaussian_data <- function(sample_size = 10000, error_sd = 2) {
  
  # Set the mean for each feature (10)
  mu <- rnorm(10, mean = 5, sd = 5)
  
  # Set the covariance matrix with a range of 0 to 0.5
  cov_range <- seq(0.0, 0.5, length.out = 10)
  sigma <- diag(10) * 1.8
  for (i in 1:10) {
    for (j in 1:10) {
      if (i != j) {
        sigma[i, j] <- cov_range[abs(i - j)]
      }
    }
  }
  
  
  # Generate samples from the multivariate normal distribution
  data <- data.frame(mvrnorm(n = sample_size, mu = mu, Sigma = sigma))
  
  
  # create error
  error <- rnorm(sample_size, 0, error_sd)
  
  # create y by sum x's and add error
  data$y <- rowSums(data[, 1:10]) + error
  
  return(data)
}

