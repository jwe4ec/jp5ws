# ---------------------------------------------------------------------------- #
# Multiple Imputation (With Linear)
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Load packages and set seed ----
# ---------------------------------------------------------------------------- #

library(groundhog)
groundhog_day <- "2020-07-12"

groundhog.library(jomo, groundhog_day)
groundhog.library(mitml, groundhog_day)

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/jp5ws/).

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./Data/Clean/final.RData")

# ---------------------------------------------------------------------------- #
# Compute time variables for linear spline ----
# ---------------------------------------------------------------------------- #

# Compute time1 for treatment phase and time2 for follow-up phase

final$time1 <- NA
final$time1[final$session_int == 0] <- 0
final$time1[final$session_int == 1] <- 1
final$time1[final$session_int == 2] <- 2
final$time1[final$session_int == 3] <- 3
final$time1[final$session_int == 4] <- 4
final$time1[final$session_int == 5] <- 4

final$time2 <- NA
final$time2[final$session_int == 0] <- 0
final$time2[final$session_int == 1] <- 0
final$time2[final$session_int == 2] <- 0
final$time2[final$session_int == 3] <- 0
final$time2[final$session_int == 4] <- 0
final$time2[final$session_int == 5] <- 1

# ---------------------------------------------------------------------------- #
# Multiply impute data ----
# ---------------------------------------------------------------------------- #

# TODO




