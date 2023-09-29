

#First, install the tensorflow R package from GitHub as follows
install.packages("tensorflow")


#Next, configure R with a Python installation it can use, like this:
library(reticulate)
path_to_python <- install_python()
virtualenv_create("r-reticulate", python = path_to_python)
virtualenv_create("r-reticulate")

#Then, use the install_tensorflow() function to install TensorFlow
library(tensorflow)
tensorflow::install_tensorflow(envname = "r-reticulate")


#You can also use keras::install_keras(), which installes Tensorflow, in
# addition to some commonly used packages like “scipy” and “tensorflow-datasets”.
install.packages("keras")
library(keras)
keras::install_keras(envname = "reticulate")



#You can confirm that the installation succeeded with:
library(tensorflow)
tf$constant("Hello Tensorflow!")


use_virtualenv('r-reticulate')

# sudo !pip install --upgrade --force-reinstall tensorflow

tensorflow::tf_config()
reticulate::py_config()

