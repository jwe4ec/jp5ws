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
meta.groundhog("2021-07-01")
groundhog_day <- "2021-05-20"

groundhog.library(jomo, groundhog_day)
groundhog.library(mitml, groundhog_day)

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/jp5ws/).

# Store working directory

wd_dir <- getwd()

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
# Multiply impute data with jomo ----
# ---------------------------------------------------------------------------- #

# Conduct multiple imputation with jomo given some missing data at Level 2 (in 
# age and educationGrpNew2) and include condition as a predictor of these Level
# 2 missing data. Treat age and educationGrpNew2 as numeric.

final$age <- as.numeric(final$age)
final$educationGrpNew2 <- as.numeric(final$educationGrpNew2)

## Impute for an analysis model with 5 conditions. It may take a few hours 
## depending on computing power.

fml <- list(posExpBiasScale + negExpBiasScale + depressionScale + anxietyScale + selfEffScale + growthMindScale + optimismScale ~ condition + time1 + time2 + condition*time1 + condition*time2 + (1 + time1 | participantId),
            age + educationGrpNew2 ~ condition)
imp_ls <- jomoImpute(final, formula = fml, n.burn = 25000, n.iter = 5000, m = 20, seed = 1234)
impList_ls <- mitmlComplete(imp_ls, print = "all")

### Potential scale reduction factor is close to 1 (i.e., < 1.050) for all 
### parameters. Trace plots show no sign of drifting or substantial change
### after burn-in phase, indicating 25,000 iterations were sufficient for
### parameters to reach respective target distributions. Autocorrelation
### dies out by lag 5,000, indicating imputations spread 5,000 iterations
### apart can be considered independent. Thus, parameters have converged
### and the imputed datasets constitute independent draws from posterior
### predictive distribution of the missing data.

sink("./Results/Imputation/impList_linear_spline/mitmlSummary/summary.txt")
summary(imp_ls)
sink()

setwd("./Results/Imputation/impList_linear_spline")
plot(imp_ls, trace = "all", export = "pdf")
setwd(wd_dir)

### Save imputation object and list of imputed datasets

save(imp_ls, file = "./Data/Imputed/imp_ls.RData")
save(impList_ls, file = "./Data/Imputed/impList_ls.RData")

## Impute for an analysis model with 3 conditions. It may take a few hours 
## depending on computing power.

### Combine POSITIVE_NEGATION and POSITIVE into BothPositive and 
### FIFTY_FIFTY_BLOCKED and FIFTY_FIFTY_RANDOM into BothFiftyFifty. Make
### NEUTRAL the reference group.

table(final$condition, final$session_int)

final2 <- final

final2$condition <- gsub('POSITIVE_NEGATION', 'BothPositive', final2$condition)
final2$condition <- gsub('POSITIVE', 'BothPositive', final2$condition)
final2$condition <- gsub('FIFTY_FIFTY_BLOCKED', 'BothFiftyFifty', final2$condition)
final2$condition <- gsub('FIFTY_FIFTY_RANDOM', 'BothFiftyFifty', final2$condition)

final2$condition <- factor(final2$condition, 
                           levels = c("NEUTRAL", "BothPositive", "BothFiftyFifty"))

table(final2$condition, final2$session_int)

### Run the imputation model

fml <- list(posExpBiasScale + negExpBiasScale + depressionScale + anxietyScale + selfEffScale + growthMindScale + optimismScale ~ condition + time1 + time2 + condition*time1 + condition*time2 + (1 + time1 | participantId),
            age + educationGrpNew2 ~ condition)
imp_ls2 <- jomoImpute(final2, formula = fml, n.burn = 25000, n.iter = 5000, m = 20, seed = 1234)
impList_ls2 <- mitmlComplete(imp_ls2, print = "all")

### Potential scale reduction factor is close to 1 (i.e., < 1.050) for all 
### parameters. Thus, we deem parameters to have converged.

sink("./Results/Imputation/impList2_linear_spline/mitmlSummary/summary.txt")
summary(imp_ls2)
sink()

setwd("./Results/Imputation/impList2_linear_spline")
plot(imp_ls2, trace = "all", export = "pdf")
setwd(wd_dir)

### Save imputation object and list of imputed datasets

save(imp_ls2, file = "./Data/Imputed/imp_ls2.RData")
save(impList_ls2, file = "./Data/Imputed/impList_ls2.RData")