#############################################################################
# This script is created to compare the sensitivity of the 98th and 99th percentiles
# of a log-normal distribution when sigma is changing
##############################################################################

#=============================================================================#
#Sensitivity of the 98th percentiles
#=============================================================================#

# Calculate the 98th percentile  mean =0 and sd =1
percentile_98 <- exp(qnorm(0.98, mean = 0, sd = 1))
percentile_98

# Calculate the 98th percentile  mean =0 and sd =1.2
percentile_98_11 <- exp(qnorm(0.98, mean = 0, sd = 1.1))
percentile_98_11

# Calculate the 98th percentile  mean =0 and sd =1.2
percentile_98_12 <- exp(qnorm(0.98, mean = 0, sd = 1.2))
percentile_98_12

# Calculate the 98th percentile  mean =0 and sd =1.3
percentile_98_13 <- exp(qnorm(0.98, mean = 0, sd = 1.3))
percentile_98_13


#=============================================================================#
#Sensitivity of the 99th percentiles
#=============================================================================#
# Calculate the 99th percentile for mean =0 and sd =1
percentile_99 <- exp(qnorm(0.99, mean = 0, sd = 1))
percentile_99

# Calculate the 99th percentile  mean =0 and sd =1.1
percentile_99_11 <- exp(qnorm(0.99, mean = 0, sd = 1.1))
percentile_99_11

# Calculate the 99th percentile  mean =0 and sd =1.2
percentile_99_12 <- exp(qnorm(0.99, mean = 0, sd = 1.2))
percentile_99_12

# Calculate the 99th percentile  mean =0 and sd =1.3
percentile_99_13 <- exp(qnorm(0.99, mean = 0, sd = 1.3))
percentile_99_13