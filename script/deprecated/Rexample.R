# An example R script demonstrating how to use the Random Forest
# described in the paper "CREATING A UNIVERSAL DEPTH-TO-LOAD
# CONVERSION  TECHNIQUE FOR THE CONTERMINOUS UNITED STATES USING
# RANDOM FORESTS", Wheeler, et al 2020
#
# @author: Jesse Wheeler
# @date: Aug 25, 2020

library(dplyr)
library(randomForest)

# Load the Random Forest object described in the paper
RF <-  readRDS(file = 'R/RObject/RF.RDS')

# Load the example dataset. This example dataset contains PRISM climate
# normals.
df <- read.csv('data-raw/data.csv')

colnames(df)

# Note that the names in the dataset don't match exactly the names
# used in the random forest. The names in the random forest were changed
# to improve readability in the paper. Therefore in order for the
# random forest to work "as is", the names of the relevant variables
# in the dataset must be changed. We also need to make a few variable
# transformations:

df <- df %>%
  dplyr::mutate(
  logSNWD = log(maxv_SNWD),
  logPPTWT = log(PPTWT)
  ) %>%
  dplyr::rename(
    SMONTH = snow_month_SNWD,
    D2C = dist2coast,
    ELEV = ELEVATION,
    RATIO = maxRatio
  )

# Now that all of the variable names in the data match the names
# in the RF object, we can get an estimate of the specific gravity
# for each observation. The true specific gravity is saved in a
# variable called "maxRatio"
predicted_SG <- predict(RF, newdata = df)

# To get an estimate of SWE in mm, we need to multiply by the snow depth,
# in mm:
df$predicted_SWE <- df$maxv_SNWD * predicted_SG

# Creating a new Random Forest Object -------------------------------------

# Setting a random seed allows the results to be reproducible in the future.
set.seed(1856321)

# Creating a new random forest object.
new_RF <- randomForest(
  RATIO ~ logSNWD + SMONTH + D2C + logPPTWT + MCMT + MWMT +
    TD + ELEV,
  data = df[df$data == 'Train', ],  # Use only training data
  importance = TRUE,
  keep.forest = TRUE,
  ntree = 100,
  nodesize = 20,
  do.trace = 5
)


# Default Plots -----------------------------------------------------------

# This plot shows that as the number of trees grows, the predictive error
# decreases
plot(new_RF)

# This plot shows the variable importance of each plot, suggesting the time
# when the observation was taken is the most important variable in predicting
# the ratio maxv_WESD/maxv_SNWD
varImpPlot(new_RF)

# This plot shows how the SMONTH variable affects the estimation of the
# ratio maxv_WESD/maxv_SNWD, on average, integrating out the remaining variables.
partialPlot(new_RF, pred.data = df[df$data == 'Test', ], x.var = 'SMONTH')

