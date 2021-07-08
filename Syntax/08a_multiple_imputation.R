# ---------------------------------------------------------------------------- #
# Multiple Imputation
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
# Multiply impute data ----
# ---------------------------------------------------------------------------- #

# Conduct multiple imputation with jomo given some missing data at Level 2 (in 
# age and educationGrpNew2). Treat age and educationGrpNew2 as numeric as the 
# model does not finish running after many hours when educationGrpNew2 is an
# ordered factor (have not tried as a factor).

final$age <- as.numeric(final$age)
final$educationGrpNew2 <- as.numeric(final$educationGrpNew2)

## Impute for an analysis model with 5 conditions. It may take a few hours 
## depending on computing power.

fml <- list(posExpBiasScale + negExpBiasScale + depressionScale + anxietyScale + selfEffScale + growthMindScale + optimismScale ~ condition + session_int + condition*session_int + (1 + session_int | participantId),
            age + educationGrpNew2 ~ 1)
imp <- jomoImpute(final, formula = fml, n.burn = 10000, n.iter = 250, m = 100, seed = 1234)
impList <- mitmlComplete(imp, print = "all")

### Potential scale reduction factor is close to 1 (i.e., < 1.050) for all 
### parameters. Trace plots show no sign of drifting or substantial change
### after burn-in phase, indicating 10,000 iterations were sufficient for
### paramters to reach respective target distributions. Autocorrelation
### dies out by lag 250, indicating imputations spread 250 iterations
### apart can be considered independent. Thus, parameters have converged
### and the imputed datasets constitute independent draws from posterior
### predictive distribution of the missing data.

sink("./Results/Imputation/impList/mitmlSummary/summary.txt")
summary(imp)
sink()

setwd("./Results/Imputation/impList")
plot(imp, trace = "all", export = "pdf")

### Save list of imputed datasets.

save(impList, file = "./Data/Imputed/impList.RData")

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

### Run the imputation model.

fml <- list(posExpBiasScale + negExpBiasScale + depressionScale + anxietyScale + selfEffScale + growthMindScale + optimismScale ~ condition + session_int + condition*session_int + (1 + session_int | participantId),
            age + educationGrpNew2 ~ 1)
imp2 <- jomoImpute(final2, formula = fml, n.burn = 10000, n.iter = 250, m = 100, seed = 1234)
impList2 <- mitmlComplete(imp2, print = "all")

### Potential scale reduction factor is close to 1 (i.e., < 1.050) for all 
### parameters. Trace plots show no sign of drifting or substantial change
### after burn-in phase, indicating 10,000 iterations were sufficient for
### parameters to reach respective target distributions. Autocorrelation
### dies out by lag 250, indicating imputations spread 250 iterations
### apart can be considered independent. Thus, parameters have converged
### and the imputed datasets constitute independent draws from posterior
### predictive distribution of the missing data.

sink("./Results/Imputation/impList2/mitmlSummary/summary.txt")
summary(imp2)
sink()

  setwd("./Results/Imputation/impList2/mitmlPlots")
plot(imp2, trace = "all", export = "pdf")

### Save list of imputed datasets.

save(impList2, file = "./Data/Imputed/impList2.RData")