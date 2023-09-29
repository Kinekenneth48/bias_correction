

#############################################################################
############ Step 0: Load libraries and data
#############################################################################

# ===========================================================================#
# load libraries and functions
# ===========================================================================#
library(dplyr)
library(randomForest)
library(renv)


source(file = "R/fit_observations.R")

# ===========================================================================#
# load data
# ===========================================================================#
# Load the Random Forest object described in the paper
RF <- readRDS(file = "R/RObject/RF.RDS")

# Load the example dataset. This example dataset contains PRISM climate
# normals.
df <- read.csv("data-raw/data.csv")

colnames(df)



#############################################################################
############ Step 1: Prepare data and make prediction
#############################################################################


# ===========================================================================#
# clean data and make SWE PREDICTION USING rf
# ===========================================================================#
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





#############################################################################
############ Step 2: Fit distr.
#############################################################################




# ===========================================================================#
# filter stations
# ===========================================================================#
# get stations with annual max more than 30
id <- df %>%
  count(ID) %>%
  filter(n >= 30)



df <- df %>%
  filter(ID %in% id$ID)





# ===========================================================================#
# fit GEV distr to observations per stations
# ===========================================================================#

set.seed(234567)

# Convert dataframe of data into list by ID
ls_df <- df %>%
  dplyr::mutate(ID = factor(ID, levels = unique(ID))) %>%
  dplyr::group_split(ID) %>%
  stats::setNames(., unique(df$ID))

converted_load_para <- do.call(rbind, lapply(ls_df, FUN = fit_obs_pred))

direct_load_para <- do.call(rbind, lapply(ls_df, FUN = fit_obs_direct))


comb_para <- inner_join(direct_load_para, converted_load_para, by = c("ID")) %>%
  mutate(
    ratio_loc = LOC_PRED / LOC_DIRECT,
    ratio_scale = SCALE_PRED / SCALE_DIRECT,
    ratio_shape = SHAPE_PRED / SHAPE_DIRECT
  )






