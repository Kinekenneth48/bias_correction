

log_normal_layer <- function(x) {
  # Lambda function for generating log-normal parameters
  # mu and sigma from a Dense(2) output.
  # Assumes tensorflow 2 backend.
  
  # Usage
  # -----
  # outputs <- Dense(2)(final_layer)
  # distribution_outputs <- Lambda(log_normal_layer)(outputs)
  
  # Parameters
  # ----------
  # x : tf.Tensor
  #     output tensor of Dense layer
  #     
  # Returns
  # -------
  # out_tensor : tf.Tensor
  
  # Get the number of dimensions of the input
  num_dims <- length(dim(x))
  
  # Separate the parameters
  #mu, sigma <- tf$unstack(x, num = 2, axis = -1)
  
  # Add one dimension to make the right shape
  mu <- tf$expand_dims(mu, -1)
  sigma <- tf$expand_dims(sigma, -1)
  
  # Apply an exponential to mu to make positive
  mu <- tf$exp(mu)
  
  # Apply a softplus to sigma to make positive
  sigma <- tf$keras$activations$softplus(sigma)
  
  # Join back together again
  out_tensor <- tf$concat(list(mu, sigma), axis = num_dims-1)
  
  return(out_tensor)
}
