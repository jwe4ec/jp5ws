# ---------------------------------------------------------------------------- #
# Multiple Imputation (With Linear Spline)
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Check R version, load packages, and set seed ----
# ---------------------------------------------------------------------------- #

script_R_version <- "R version 4.1.0 (2021-05-18)"
current_R_version <- R.Version()$version.string

if(current_R_version != script_R_version) {
  warning(paste0("This script is based on ", script_R_version,
                 ". You are running ", current_R_version, "."))
}

library(groundhog)
groundhog_day <- "2021-05-20"

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

# Conduct multiple imputation with jomo given some missing data at Level 2 (in 
# age and educationGrpNew2). Treat age and educationGrpNew2 as numeric.

final$age <- as.numeric(final$age)
final$educationGrpNew2 <- as.numeric(final$educationGrpNew2)

## Impute for an analysis model with 5 conditions. It may take a few hours 
## depending on computing power.

# TODO: Consult about this model before running. Waiting to hear back.





fml <- list(posExpBiasScale + negExpBiasScale + depressionScale + anxietyScale + selfEffScale + growthMindScale + optimismScale ~ time1 + time2 + (1 + time1 + time2 | participantId),
            age + educationGrpNew2 ~ condition)

imp <- jomoImpute(final, formula = fml, n.burn = 10000, n.iter = 250, m = 100, seed = 1234)
impList <- mitmlComplete(imp, print = "all")