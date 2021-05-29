#------------------------------------------------------------------------------#
# Baseline Internal Consistency
# Authors: Jianhui Sun and Jeremy W. Eberle
#------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------- #
# Check R version, load packages, and set seed ----
# ---------------------------------------------------------------------------- #

script_R_version <- "R version 4.0.2 (2020-06-22)"
current_R_version <- R.Version()$version.string

if(current_R_version != script_R_version) {
  warning(paste0("This script is based on ", script_R_version,
                 ". You are running ", current_R_version, "."))
}

library(groundhog)
groundhog_day <- "2020-06-03"

groundhog.library(psych, groundhog_day)
groundhog.library(MBESS, groundhog_day)
groundhog.library(OpenMx, groundhog_day)

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Define preparedata function ----
# ---------------------------------------------------------------------------- #

# Define a function for preparing data.

preparedata <- function(x) {
  
  # Combine Eligibility and preTest rows into a row called Baseline.
  
  x$session <- gsub('Eligibility', 'Baseline', x$session)
  x$session <- gsub('preTest', 'Baseline', x$session)
  x$session <- factor(x$session, levels = c("Baseline", "firstSession",
                                            "secondSession", "thirdSession",
                                            "fourthSession", "PostFollowUp"))
  
  x <- aggregate(
    x = x[c("nervous", "worry", "anxietyScale", "pleasure", "depressed", 
            "depressionScale", "shortRest", "settleIn", "consideredAdvancement", 
            "financiallySecure", "posExpBiasScale", "verySick", "offend", 
            "stuck", "ruining", "negExpBiasScale", "reruns", "bagel", "lunch", 
            "thermostat", "learn", "learnRev", "particularThinking", 
            "alwaysChangeThinking", "growthMindScale", "difficultTasks", 
            "performEffectively", "compared", "selfEffScale", "wrongWill", 
            "wrongWillRev", "hardlyEver", "hardlyEverRev", "optimismScale")],
    by = list(participantId = x$participantId,
              session = x$session,
              condition = x$condition),
    mean, na.rm = TRUE
  )
  
  # Replace NaN with NA.
  
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  x[is.nan(x)] <- NA
  
  # Sort by participantId and session.
  
  x <- x[order(x$participantId, x$session), ]
  
  # Compute session_int.
  
  x$session_int <- as.integer(x$session)
  x$session_int <- x$session_int - 1
  
  return(x)
  
}

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/jp5ws/).

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

# Import item data, remove the X column, use preparedata function above to 
# combine Eligibility and preTest into Baseline, restrict item data to baseline,
# and use sample data to restrict to ITT subjects and to completer subjects.

x <- read.csv("./Data/Clean/FTmainDataItemsScales.csv")
x$X <- NULL

x <- preparedata(x)

x.baseline <- x[x$session == 'Baseline', ]

p <- read.csv("./Data/Clean/FTmainAnalysisSamples.csv")
p$X <- NULL
ITT <- subset(p, ittSample == 1)
Completers <- subset(p, txCompSample == 1)

x.baseline.ITT <- 
  x.baseline[which(x.baseline$participantId %in% ITT$participantId), ]
x.baseline.Completers <- 
  x.baseline[which(x.baseline$participantId %in% Completers$participantId), ]

rownames(x.baseline.ITT) <- 1:nrow(x.baseline.ITT)
rownames(x.baseline.Completers) <- 1:nrow(x.baseline.Completers)

# ---------------------------------------------------------------------------- #
# Compute Cronbach's alpha ----
# ---------------------------------------------------------------------------- #

# Positive expectancybias

## Mean of "shortRest", "settleIn", "consideredAdvancement", "financiallySecure"

psych::alpha(x.baseline.ITT[, c("shortRest", 
                                "settleIn", 
                                "consideredAdvancement", 
                                "financiallySecure")])$total$std.alpha
psych::alpha(x.baseline.Completers[, c("shortRest", 
                                       "settleIn", 
                                       "consideredAdvancement", 
                                       "financiallySecure")])$total$std.alpha

# Negative expectancy bias

## Mean of "verySick", "offend", "stuck", "ruining"

psych::alpha(x.baseline.ITT[, c("verySick", 
                                "offend", 
                                "stuck", 
                                "ruining")])$total$std.alpha
psych::alpha(x.baseline.Completers[, c("verySick", 
                                       "offend", 
                                       "stuck", 
                                       "ruining")])$total$std.alpha

# Anxiety

## sum of "nervous", "worry"

psych::alpha(x.baseline.ITT[, c("nervous",
                                "worry")])$total$std.alpha
psych::alpha(x.baseline.Completers[, c("nervous",
                                       "worry")])$total$std.alpha

# Depression

## sum of "pleasure", "depressed"

psych::alpha(x.baseline.ITT[, c("pleasure",
                                "depressed")])$total$std.alpha
psych::alpha(x.baseline.Completers[, c("pleasure",
                                       "depressed")])$total$std.alpha

# Self-Efficacy

## Mean of "difficultTasks", "performEffectively", "compared"

psych::alpha(x.baseline.ITT[, c("difficultTasks", 
                                "performEffectively", 
                                "compared")])$total$std.alpha
psych::alpha(x.baseline.Completers[, c("difficultTasks", 
                                       "performEffectively", 
                                       "compared")])$total$std.alpha

# GrowthMindset

## Mean of "learnRev", "particularThinking", "alwaysChangeThinking"

psych::alpha(x.baseline.ITT[, c("learnRev", 
                                "particularThinking", 
                                "alwaysChangeThinking")])$total$std.alpha
psych::alpha(x.baseline.Completers[, c("learnRev", 
                                       "particularThinking", 
                                       "alwaysChangeThinking")])$total$std.alpha

# Optimism

## Mean of "wrongWillRev", "hardlyEverRev"

psych::alpha(x.baseline.ITT[, c("wrongWillRev", 
                                "hardlyEverRev")])$total$std.alpha
psych::alpha(x.baseline.Completers[, c("wrongWillRev", 
                                       "hardlyEverRev")])$total$std.alpha

# ---------------------------------------------------------------------------- #
# Compute McDonald's omega total ----
# ---------------------------------------------------------------------------- #

# Cronbach's alpha has many potential problems (Dunn et al., 2014). Use omega 
# total instead for the variables below and include 95% confidence intervals.

# ---------------------------------------------------------------------------- #
# Positive expectancy bias (incl. factor analysis) ----
# ---------------------------------------------------------------------------- #

# Mean of "shortRest", "settleIn", "consideredAdvancement", "financiallySecure"

# ITT sample

rawData <- x.baseline.ITT[, c("shortRest", 
                              "settleIn", 
                              "consideredAdvancement", 
                              "financiallySecure")]
sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 10000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 20000)

## Low internal consistency suggests unidimensionality assumption may be invalid.
## Do factor analysis to test this assumption.

### Exploratory factor analysis reveals that item "settleIn" has highest 
### factor loading. Fix this to 1 in confirmatory factor analysis.

faOut <- fa(completeData, nfactors = 1, n.obs = 958, rotate = "promax",
             fm = "ml", SMC = TRUE)
print(faOut, sort = TRUE)

### Confirmatory factor analysis.

#### Build a single-factor FIML model with fixed loading. Although this is a
#### FIML model, use complete data because this was used for omega total.

indicators <- names(completeData)
latents <- c("F1")
loadingLabels <- paste("b_", indicators, sep = "")
uniqueLabels <- paste("U_", indicators, sep = "")
meanLabels <- paste("M_", indicators, sep = "")
factorVarLabels <- paste("Var_", latents, sep = "")

oneFactorRaw <- mxModel("Single Factor FIML Model with Fixed Loading",
                        type = "RAM",
                        manifestVars = indicators,
                        latentVars = latents,
                        mxPath(from = latents, to = indicators, 
                               arrows = 1, connect = "unique.bivariate", 
                               free = TRUE, values = .2, 
                               labels = loadingLabels),
                        mxPath(from = indicators, 
                               arrows = 2, 
                               free = TRUE, values = .8, 
                               labels = uniqueLabels),
                        mxPath(from = latents,
                               arrows = 2,
                               free = TRUE, values = 1, 
                               labels = factorVarLabels),
                        mxPath(from = latents, to = c("settleIn"),
                               arrows = 1, 
                               free = FALSE, values = 1),
                        mxPath(from = "one", to = indicators, 
                               arrows = 1, free = TRUE, values = .1, 
                               labels = meanLabels),
                        mxData(observed = completeData, type = "raw")
)

#### Note: Warning indicates that Hessian in the latent space at the solution does 
#### not appear to be convex. Use mxTryHard function instead.

oneFactorRawOut <- mxRun(oneFactorRaw)

set.seed(1234)
oneFactorRawOut <- mxTryHard(oneFactorRaw)

#### Investigate Hessian warning. Model is identified local to its current
#### parameter values (not necessarily globally identified). No parameters
#### are not identified.

mxCheckIdentification(oneFactorRaw)

#### Create saturated (all paths) and independence (no paths) models based on
#### structure of oneFactorRaw. Warning states that "the condition has length > 
#### 1 and only the first element will be used."

tRefModel <- mxRefModels(oneFactorRawOut, run = TRUE)

#### Compare our model to saturated and independence models.

summary(oneFactorRawOut, refModels = tRefModel)

# Completer sample

rawData <- x.baseline.Completers[, c("shortRest", 
                                     "settleIn", 
                                     "consideredAdvancement", 
                                     "financiallySecure")]
sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 10000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 20000)

## Out-of-range omega total suggests unidimensionality assumption may be invalid.
## Do factor analysis to test this assumption.

### Exploratory factor analysis reveals that item "settleIn" has highest 
### factor loading. Fix this to 1 in confirmatory factor analysis.

faOut <- fa(completeData, nfactors = 1, n.obs = 289, rotate = "promax",
             fm = "ml", SMC = TRUE)
print(faOut, sort = TRUE)

### Confirmatory factor analysis.

#### Build a single-factor FIML model with fixed loading. Although this is a
#### FIML model, use complete data because this was used for omega total.

indicators <- names(completeData)
latents <- c("F1")
loadingLabels <- paste("b_", indicators, sep = "")
uniqueLabels <- paste("U_", indicators, sep = "")
meanLabels <- paste("M_", indicators, sep = "")
factorVarLabels <- paste("Var_", latents, sep = "")

oneFactorRaw <- mxModel("Single Factor FIML Model with Fixed Loading",
                        type = "RAM",
                        manifestVars = indicators,
                        latentVars = latents,
                        mxPath(from = latents, to = indicators, 
                               arrows = 1, connect = "unique.bivariate", 
                               free = TRUE, values = .2, 
                               labels = loadingLabels),
                        mxPath(from = indicators, 
                               arrows = 2, 
                               free = TRUE, values = .8, 
                               labels = uniqueLabels),
                        mxPath(from = latents,
                               arrows = 2,
                               free = TRUE, values = 1, 
                               labels = factorVarLabels),
                        mxPath(from = latents, to = c("settleIn"),
                               arrows = 1, 
                               free = FALSE, values = 1),
                        mxPath(from = "one", to = indicators, 
                               arrows = 1, free = TRUE, values = .1, 
                               labels = meanLabels),
                        mxData(observed = completeData, type = "raw")
)

#### Note: Warning indicates that Hessian in the latent space at the solution does 
#### not appear to be convex. Use mxTryHard function instead.

oneFactorRawOut <- mxRun(oneFactorRaw)

set.seed(1234)
oneFactorRawOut <- mxTryHard(oneFactorRaw)

#### Investigate Hessian warning. Model is identified local to its current
#### parameter values (not necessarily globally identified). No parameters
#### are not identified.

mxCheckIdentification(oneFactorRaw)

#### Create saturated (all paths) and independence (no paths) models based on
#### structure of oneFactorRaw. Warning states that "the condition has length > 
#### 1 and only the first element will be used."

tRefModel <- mxRefModels(oneFactorRawOut, run = TRUE) 

#### Compare our model to saturated and independence models.

summary(oneFactorRawOut, refModels = tRefModel)

# ---------------------------------------------------------------------------- #
# Negative expectancy bias (incl. factor analysis) ----
# ---------------------------------------------------------------------------- #

# Mean of "verySick", "offend", "stuck", "ruining"

# ITT sample

rawData <- x.baseline.ITT[, c("verySick", 
                              "offend", 
                              "stuck", 
                              "ruining")]
sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 10000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 20000)

## Low internal consistency suggests unidimensionality assumption may be invalid.
## Do factor analysis to test this assumption.

### Exploratory factor analysis reveals that item "offend" has highest 
### factor loading. Fix this to 1 in confirmatory factor analysis.

faOut <- fa(completeData, nfactors = 1, n.obs = 954, rotate = "promax",
             fm = "ml", SMC = TRUE)
print(faOut, sort = TRUE)

### Confirmatory factor analysis.

#### Build a single-factor FIML model with fixed loading. Although this is a
#### FIML model, use complete data because this was used for omega total.

indicators <- names(completeData)
latents <- c("F1")
loadingLabels <- paste("b_", indicators, sep = "")
uniqueLabels <- paste("U_", indicators, sep = "")
meanLabels <- paste("M_", indicators, sep = "")
factorVarLabels <- paste("Var_", latents, sep = "")

oneFactorRaw <- mxModel("Single Factor FIML Model with Fixed Loading",
                        type = "RAM",
                        manifestVars = indicators,
                        latentVars = latents,
                        mxPath(from = latents, to = indicators, 
                               arrows = 1, connect = "unique.bivariate", 
                               free = TRUE, values = .2, 
                               labels = loadingLabels),
                        mxPath(from = indicators, 
                               arrows = 2, 
                               free = TRUE, values = .8, 
                               labels = uniqueLabels),
                        mxPath(from = latents,
                               arrows = 2,
                               free = TRUE, values = 1, 
                               labels = factorVarLabels),
                        mxPath(from = latents, to = c("offend"),
                               arrows = 1, 
                               free = FALSE, values = 1),
                        mxPath(from = "one", to = indicators, 
                               arrows = 1, free = TRUE, values = .1, 
                               labels = meanLabels),
                        mxData(observed = completeData, type = "raw")
)

#### Note: Warning indicates that Hessian in the latent space at the solution does 
#### not appear to be convex. Try mxTryHard function instead.

oneFactorRawOut <- mxRun(oneFactorRaw)

set.seed(1234)
oneFactorRawOut <- mxTryHard(oneFactorRaw)

#### Investigate Hessian warning. Model is identified local to its current
#### parameter values (not necessarily globally identified). No parameters
#### are not identified.

mxCheckIdentification(oneFactorRaw)

#### Create saturated (all paths) and independence (no paths) models based on
#### structure of oneFactorRaw. Warning states that "the condition has length > 
#### 1 and only the first element will be used."

tRefModel <- mxRefModels(oneFactorRawOut, run = TRUE) 

#### Compare our model to saturated and independence models.

summary(oneFactorRawOut, refModels = tRefModel)

# Completer sample

rawData <- x.baseline.Completers[, c("verySick", 
                                     "offend", 
                                     "stuck", 
                                     "ruining")]
sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 10000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 20000)

## Low and unstable omega total suggests unidimensionality assumption may be invalid.
## Do factor analysis to test this assumption.

### Exploratory factor analysis reveals that item "offend" has highest 
### factor loading. Fix this to 1 in confirmatory factor analysis.

faOut <- fa(completeData, nfactors = 1, n.obs = 288, rotate = "promax",
             fm = "ml", SMC = TRUE)
print(faOut, sort = TRUE)

### Confirmatory factor analysis.

#### Build a single-factor FIML model with fixed loading. Although this is a
#### FIML model, use complete data because this was used for omega total.

indicators <- names(completeData)
latents <- c("F1")
loadingLabels <- paste("b_", indicators, sep = "")
uniqueLabels <- paste("U_", indicators, sep = "")
meanLabels <- paste("M_", indicators, sep = "")
factorVarLabels <- paste("Var_", latents, sep = "")

oneFactorRaw <- mxModel("Single Factor FIML Model with Fixed Loading",
                        type = "RAM",
                        manifestVars = indicators,
                        latentVars = latents,
                        mxPath(from = latents, to = indicators, 
                               arrows = 1, connect = "unique.bivariate", 
                               free = TRUE, values = .2, 
                               labels = loadingLabels),
                        mxPath(from = indicators, 
                               arrows = 2, 
                               free = TRUE, values = .8, 
                               labels = uniqueLabels),
                        mxPath(from = latents,
                               arrows = 2,
                               free = TRUE, values = 1, 
                               labels = factorVarLabels),
                        mxPath(from = latents, to = c("offend"),
                               arrows = 1, 
                               free = FALSE, values = 1),
                        mxPath(from = "one", to = indicators, 
                               arrows = 1, free = TRUE, values = .1, 
                               labels = meanLabels),
                        mxData(observed = completeData, type = "raw")
)

#### Note: Warning indicates that Hessian in the latent space at the solution does 
#### not appear to be convex. Use mxTryHard function instead.

oneFactorRawOut <- mxRun(oneFactorRaw)

set.seed(1234)
oneFactorRawOut <- mxTryHard(oneFactorRaw)

#### Investigate Hessian warning. Model is identified local to its current
#### parameter values (not necessarily globally identified). No parameters
#### are not identified.

mxCheckIdentification(oneFactorRaw)

#### Create saturated (all paths) and independence (no paths) models based on
#### structure of oneFactorRaw. Warning states that "the condition has length > 
#### 1 and only the first element will be used."

tRefModel <- mxRefModels(oneFactorRawOut, run = TRUE) 

#### Compare our model to saturated and independence models.

summary(oneFactorRawOut, refModels = tRefModel)

46.65/2
6.09/2
47.75/2
9.00/2

# ---------------------------------------------------------------------------- #
# Anxiety ----
# ---------------------------------------------------------------------------- #

# sum of "nervous", "worry"

# ITT sample

rawData <- x.baseline.ITT[, c("nervous",
                              "worry")]
sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 10000)

# Completer sample

rawData <- x.baseline.Completers[, c("nervous",
                                     "worry")]
sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 10000)

# ---------------------------------------------------------------------------- #
# Depression ----
# ---------------------------------------------------------------------------- #

# sum of "pleasure", "depressed"

# ITT sample

rawData <- x.baseline.ITT[, c("pleasure",
                              "depressed")]
sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 10000)

# Completer sample

rawData <- x.baseline.Completers[, c("pleasure",
                                     "depressed")]
sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 10000)

# ---------------------------------------------------------------------------- #
# Self-efficacy ----
# ---------------------------------------------------------------------------- #

# Mean of "difficultTasks", "performEffectively", "compared"

# ITT sample

rawData <- x.baseline.ITT[, c("difficultTasks", 
                              "performEffectively", 
                              "compared")]
sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 10000)

# Completer sample

rawData <- x.baseline.Completers[, c("difficultTasks", 
                                     "performEffectively", 
                                     "compared")]
sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 10000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 20000)

# ---------------------------------------------------------------------------- #
# Growth mindset ----
# ---------------------------------------------------------------------------- #

# Mean of "learnRev", "particularThinking", "alwaysChangeThinking"

# ITT sample

rawData <- x.baseline.ITT[, c("learnRev", 
                              "particularThinking", 
                              "alwaysChangeThinking")]
sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 10000)

# Completer sample

rawData <- x.baseline.Completers[, c("learnRev", 
                                     "particularThinking", 
                                     "alwaysChangeThinking")]
sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 10000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 20000)

# ---------------------------------------------------------------------------- #
# Optimism ----
# ---------------------------------------------------------------------------- #

# Mean of "wrongWillRev", "hardlyEverRev"

# ITT sample

rawData <- x.baseline.ITT[, c("wrongWillRev", 
                              "hardlyEverRev")]
sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 10000)

# Completer sample

rawData <- x.baseline.Completers[, c("wrongWillRev", 
                                     "hardlyEverRev")]
sum(is.na(rawData))
nrow(rawData)
completeData <- na.omit(rawData)
sum(is.na(completeData))
nrow(completeData)

ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 1000)
ci.reliability(data = completeData,
               type = "omega", conf.level = 0.95, interval.type = "bca", B = 10000)