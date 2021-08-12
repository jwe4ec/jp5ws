# ---------------------------------------------------------------------------- #
# Longitudinal Analyses - Exploratory
# Authors: Jeremy W. Eberle
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

groundhog.library(mitml, groundhog_day)
groundhog.library(nlme, groundhog_day)

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/jp5ws/).

# ---------------------------------------------------------------------------- #
# Load imputed datasets ----
# ---------------------------------------------------------------------------- #

# Read in imputed datasets, one list for analysis model with 5 conditions 
# (impList) and one list for analysis model with 3 conditions (impList2).

load("./Data/Imputed/impList.RData")
load("./Data/Imputed/impList2.RData")

# ---------------------------------------------------------------------------- #
# Compute PHQ-4 total score ----
# ---------------------------------------------------------------------------- #

# Compute PHQ-4 total score in each imputed dataset of impList and impList2 by 
# adding depressionScale to anxietyScale.

for (i in 1:length(impList)) {
  impList[[i]]$phq4Scale <- impList[[i]]$depressionScale + 
                            impList[[i]]$anxietyScale
}

for (i in 1:length(impList2)) {
  impList2[[i]]$phq4Scale <- impList2[[i]]$depressionScale + 
                             impList2[[i]]$anxietyScale
}

# ---------------------------------------------------------------------------- #
# Load sample data ----
# ---------------------------------------------------------------------------- #

# Load sample data and store ITT subjects and Completers.

p <- read.csv("./Data/Clean/FTmainAnalysisSamples.csv")
p$X <- NULL
ITT <- subset(p, ittSample == 1)
Completers <- subset(p, txCompSample == 1)

# ---------------------------------------------------------------------------- #
# Define writeresultsExpl function ----
# ---------------------------------------------------------------------------- #

# Define function for exploratory analysis models of phq4Scale.

writeresultsExpl <- function(data, path, phase, c.levels, sample, compare) {
  print(paste0("Current working directory: ", path))
  
  sink(file = path)
  
  print(paste("Sample:", deparse(substitute(sample))))
  print(paste("Study Phase:", phase))
  print(paste("Dataset:", deparse(substitute(data))))
  cat("\n")
  
  print("-------------------------------------------------------------------")
  cat("\n")
  
  for (outcome in list("phq4Scale")) {
    x <- data
    
    # Create an empty matrix to store the baseline SD in each imputed dataset
    # (computed below) and another empty matrix with the same dimensions to 
    # store the GMA d in each imputed dataset (computed below). Also create an 
    # empty vector to store the mean baseline SD across imputed datasets 
    # (computed below).
    
    if (compare) {
      sdMat <- matrix(0, nrow = 100, ncol = length(c.levels) - 1)
    } else {
      sdMat <- matrix(0, nrow = 100, ncol = 1)
    }
    
    dMat <- sdMat
    
    sdVec <- rep(0, ncol(sdMat))
    
    # Prepare imputed datasets for analysis.
    
    for (i in 1:length(x)) {
      
      # Restrict to the relevant sample.
      
      x[[i]] <- subset(x[[i]], participantId %in% sample$participantId)
      
      # Restrict to the relevant conditions.
      
      if (compare) {
        x[[i]]$condition <- factor(x[[i]]$condition, levels = c.levels)
      } else {
        x[[i]] <- subset(x[[i]], condition == c.levels[1])
      }
      
      # Restrict to the relevant assessment points based on study phase.
      
      if (phase == 'treatment') {
        x[[i]] <- subset(x[[i]], session_int != 5)
        if (outcome != "posExpBiasScale" & outcome != "negExpBiasScale") {
          x[[i]] <- subset(x[[i]], session_int != 1 & session_int != 3)
        }
      } else {
        x[[i]] <- subset(x[[i]], session_int == 4 | session_int == 5)
        x[[i]]$session_int <- x[[i]]$session_int - 4
      }
    }
    
    # Compute SD at baseline.
    
    z <- subset(x, session_int == 0)
    
    for (i in 1:length(z)) {
      if (compare) {
        for (j in 2:length(c.levels)) {
          temp <- z[[i]]
          temp <- subset(temp, condition == c.levels[1] |
                           condition == c.levels[j])
          
          sd1 <- sd(temp[temp$condition == c.levels[1], outcome])
          sd2 <- sd(temp[temp$condition == c.levels[j], outcome])
          n1 <- length(temp[temp$condition == c.levels[1], ]$participantId)
          n2 <- length(temp[temp$condition == c.levels[j], ]$participantId)
          sdPool <- sqrt(((n1 - 1)*sd1^2 + (n2 - 1)*sd2^2)/(n1 + n2 - 2))
          
          sdMat[i, j - 1] <- sdPool
        }
      } else {
        temp <- z[[i]]
        sdMat[i] <- sd(temp[, outcome])
      }
    }
    
    # Compute df as though data were complete.
    
    a <- length(c.levels)
    b <- nrow(x[[1]])
    c <- length(unique(x[[1]]$participantId))
    
    if (compare) {
      fml = as.formula(paste(outcome, "~ condition*session_int"))
      
      if (length(c.levels) == 5) {
        df = c(b - c - a,         # intercept
               rep(c - a, 4),     # condition effects
               b - c - a,         # session_int effect
               rep(b - c - a, 4)) # condition:session_int effects
      } else if (length(c.levels) == 3) {
        df = c(b - c - a,         # intercept
               rep(c - a, 2),     # condition effects
               b - c - a,         # session_int effect
               rep(b - c - a, 2)) # condition:session_int effects
      }
    } else {
      fml = as.formula(paste(outcome, "~ session_int"))
      
      df =     c(b - c - 1,         # intercept
                 b - c - 1)         # session_int effect
    }
    
    print(paste("Outcome:", outcome))
    cat("\n")
    
    print("Number of observations (after imputation) at each session by condition:")
    cat("\n")
    print(table(x[[1]]$condition, x[[1]]$session_int, dnn = c("condition", "session_int")))
    cat("\n")
    
    print("Baseline SD:")
    cat("\n")
    
    for (j in 1:ncol(sdMat)) {
      sdVec[j] <- mean(sdMat[, j])
      
      if (compare) {
        print(paste0(outcome, ": ", 
                     c.levels[j + 1], 
                     " vs. ", 
                     c.levels[1], 
                     " = ",
                     sdVec[j]))
      } else {
        print(paste0(outcome, ": ",
                     c.levels[1], 
                     " = ",
                     sdVec[j]))
      }
    }
    
    cat("\n")
    print("Mixed effects model:")
    cat("\n")
    
    print(paste0("fml = ", format(fml)))
    
    # Run mixed effects models. In treatment phase, use random intercepts and 
    # random slopes except where there are convergence errors, in which case use 
    # only random intercepts. In follow-up phase, use only random intercepts. 
    # Analyze each imputed dataset individually and then pool results using 
    # Rubin's rules (i.e., testEstimates function). Use adjusted df as though 
    # data were complete. Compute 97.5% confidence intervals based on
    # Bonferroni-corrected alpha level of .05/2 = .025.
    
    alpha = .05/2
    
    if (phase == "follow-up") {
      modelList <- with(x, lme(fml, 
                               random = ~ 1 | participantId,
                               control = lmeControl(opt = "optim"), 
                               method = "REML"))
      cat("\n")
      print(modelList[[1]]$call)
      pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df)
      print(pooled)
      ci <- confint(pooled, level = 1 - alpha)
      print(ci)
    } else {
      modelList <- with(x, lme(fml, 
                               random = ~ 1 + session_int | participantId,
                               control = lmeControl(opt = "optim"), 
                               method = "REML"))
      cat("\n")
      print(modelList[[1]]$call)
      pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df)
      print(pooled)
      ci <- confint(pooled, level = 1 - alpha)
      print(ci)
    }
    
    # Compute effect sizes and 97.5% confidence intervals for effect sizes 
    # based on alpha level of .025. Use Method 2, where effect size estimates 
    # are computed after analysis phase results were already pooled. Method 2
    # effect size estimates are similar to those obtained via Method 1, where 
    # effect size estimates are pooled from effect sizes computed separately in
    # each imputed dataset. Between-group effect size estimates obtained via
    # Method 2 have the benefit of confidence intervals that are based on 
    # pooled standard errors of the model coefficients; these pooled standard
    # errors appropriately take into account missing data. For consistency, 
    # use Method 2 for within-group effect size estimates as well.
    
    if (compare) {
      cat("\n")
      print("Between-groups growth modeling analysis d:")
      cat("\n")
      
      print("Method 1")
      
      for (i in 1:length(modelList)) {
        for (j in 1:ncol(dMat)) {
          interactionBeta <- 
            as.numeric(modelList[[i]]$coefficients$fixed[paste0("condition", 
                                                                c.levels[1 + j], 
                                                                ":session_int")])
          if (phase == 'treatment') {
            dMat[i, j] <- interactionBeta*4/sdMat[i, j]
          } else {
            dMat[i, j] <- interactionBeta*1/sdMat[i, j]
          }
        }
      }
      
      cat("\n")
      print("Estimates pooled from effect sizes computed in each imputed dataset:")
      cat("\n")
      
      for (j in 1:ncol(dMat)) {
        print(paste0("condition", c.levels[1 + j], ":session_int",
                     " = ",
                     mean(dMat[, j])))
      }
      
      cat("\n")
      print("Method 2")
      cat("\n")
      print("Estimates computed after analysis phase results were already pooled:")
      cat("\n")
      
      betaInteraction <- data.frame(pooled$estimates[grep(":", 
                                                          row.names(pooled$estimates), 
                                                          value = TRUE), 
                                                     "Estimate"])
      colnames(betaInteraction) <- "Estimate"
      
      if (phase == 'treatment') {
        betaInteractionES <- betaInteraction*4/sdVec
      } else {
        betaInteractionES <- betaInteraction*1/sdVec
      }
      
      colnames(betaInteractionES) <- "Effect Size"
      print(betaInteractionES)
      
      cat("\n")
      print(paste0((1 - alpha)*100, "% confidence intervals:"))
      cat("\n")
      
      ciInteraction <- ci[grep(":", row.names(ci), value = TRUE), ]
      
      if (phase == 'treatment') {
        ciInteractionES <- ciInteraction*4/rep(sdVec, 2)
      } else {
        ciInteractionES <- ciInteraction*1/rep(sdVec, 2)
      }
      
      print(ciInteractionES)
    } else {
      cat("\n")
      print("Within-group growth modeling analysis d:")
      cat("\n")
      
      print("Method 1")
      cat("\n")
      
      for (i in 1:length(modelList)) {
        sessionBeta <- as.numeric(modelList[[i]]$coefficients$fixed["session_int"])
        
        if (phase == 'treatment') {
          dMat[i] <- sessionBeta*4/sdMat[i]
        } else {
          dMat[i] <- sessionBeta*1/sdMat[i]
        }
      }
      
      print("Estimate pooled from effect size computed in each imputed dataset:")
      cat("\n")
      
      print(paste0("session_int",
                   " = ",
                   mean(dMat)))
      
      cat("\n")
      print("Method 2")
      cat("\n")
      
      print("Estimate computed after analysis phase results were already pooled:")
      cat("\n")
      
      betaSession <- pooled$estimates["session_int", "Estimate"]
      
      if (phase == 'treatment') {
        betaSessionES <- betaSession*4/sdVec
      } else {
        betaSessionES <- betaSession*1/sdVec
      }
      
      print(betaSessionES)
    }
    
    cat("\n")
    print("-------------------------------------------------------------------")
    cat("\n")
  }
  sink()
}

# ---------------------------------------------------------------------------- #
# Longitudinal analyses with 5 conditions ----
# ---------------------------------------------------------------------------- #

# ITT

## Treament phase

writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/ITT/4conditions_vs_POSITIVE.txt',
                 'treatment', c("POSITIVE", "POSITIVE_NEGATION", 
                                "FIFTY_FIFTY_BLOCKED", "FIFTY_FIFTY_RANDOM", "NEUTRAL"), 
                 ITT, TRUE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/ITT/4conditions_vs_FIFTY_FIFTY_RANDOM.txt',
                 'treatment', c("FIFTY_FIFTY_RANDOM", "FIFTY_FIFTY_BLOCKED", 
                                "POSITIVE_NEGATION", "POSITIVE", "NEUTRAL"), 
                 ITT, TRUE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/ITT/POSITIVE_NEGATION.txt', 
                 'treatment', c("POSITIVE_NEGATION"), 
                 ITT, FALSE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/ITT/POSITIVE.txt', 
                 'treatment', c("POSITIVE"), 
                 ITT, FALSE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/ITT/FIFTY_FIFTY_BLOCKED.txt', 
                 'treatment', c("FIFTY_FIFTY_BLOCKED"), 
                 ITT, FALSE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/ITT/FIFTY_FIFTY_RANDOM.txt', 
                 'treatment', c("FIFTY_FIFTY_RANDOM"), 
                 ITT, FALSE)

## Follow-up phase

writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/ITT/4conditions_vs_POSITIVE.txt', 
                 'follow-up', c("POSITIVE", "POSITIVE_NEGATION", 
                                "FIFTY_FIFTY_BLOCKED", "FIFTY_FIFTY_RANDOM", "NEUTRAL"), 
                 ITT, TRUE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/ITT/4conditions_vs_FIFTY_FIFTY_RANDOM.txt', 
                 'follow-up', c("FIFTY_FIFTY_RANDOM", "FIFTY_FIFTY_BLOCKED", 
                                "POSITIVE_NEGATION", "POSITIVE", "NEUTRAL"), 
                 ITT, TRUE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/ITT/POSITIVE_NEGATION.txt', 
                 'follow-up', c("POSITIVE_NEGATION"), 
                 ITT, FALSE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/ITT/POSITIVE.txt', 
                 'follow-up', c("POSITIVE"), 
                 ITT, FALSE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/ITT/FIFTY_FIFTY_BLOCKED.txt', 
                 'follow-up', c("FIFTY_FIFTY_BLOCKED"),
                 ITT, FALSE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/ITT/FIFTY_FIFTY_RANDOM.txt', 
                 'follow-up', c("FIFTY_FIFTY_RANDOM"), 
                 ITT, FALSE)

# Completers

## Treament phase

writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/Completers/4conditions_vs_POSITIVE.txt', 
                 'treatment', c("POSITIVE", "POSITIVE_NEGATION", 
                                "FIFTY_FIFTY_BLOCKED", "FIFTY_FIFTY_RANDOM", "NEUTRAL"), 
                 Completers, TRUE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/Completers/4conditions_vs_FIFTY_FIFTY_RANDOM.txt', 
                 'treatment', c("FIFTY_FIFTY_RANDOM", "FIFTY_FIFTY_BLOCKED", 
                                "POSITIVE_NEGATION", "POSITIVE", "NEUTRAL"), 
                 Completers, TRUE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/Completers/POSITIVE_NEGATION.txt', 
                 'treatment', c("POSITIVE_NEGATION"), 
                 Completers, FALSE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/Completers/POSITIVE.txt', 
                 'treatment', c("POSITIVE"), 
                 Completers, FALSE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/Completers/FIFTY_FIFTY_BLOCKED.txt', 
                 'treatment', c("FIFTY_FIFTY_BLOCKED"), 
                 Completers, FALSE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/Completers/FIFTY_FIFTY_RANDOM.txt', 
                 'treatment', c("FIFTY_FIFTY_RANDOM"), 
                 Completers, FALSE)

## Follow-up phase

writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/Completers/4conditions_vs_POSITIVE.txt', 
                 'follow-up', c("POSITIVE", "POSITIVE_NEGATION", 
                                "FIFTY_FIFTY_BLOCKED", "FIFTY_FIFTY_RANDOM", "NEUTRAL"), 
                 Completers, TRUE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/Completers/4conditions_vs_FIFTY_FIFTY_RANDOM.txt', 
                 'follow-up', c("FIFTY_FIFTY_RANDOM", "FIFTY_FIFTY_BLOCKED", 
                                "POSITIVE_NEGATION", "POSITIVE", "NEUTRAL"), 
                 Completers, TRUE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/Completers/POSITIVE_NEGATION.txt', 
                 'follow-up', c("POSITIVE_NEGATION"), 
                 Completers, FALSE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/Completers/POSITIVE.txt', 
                 'follow-up', c("POSITIVE"), 
                 Completers, FALSE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/Completers/FIFTY_FIFTY_BLOCKED.txt', 
                 'follow-up', c("FIFTY_FIFTY_BLOCKED"), 
                 Completers, FALSE)
writeresultsExpl(impList, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/Completers/FIFTY_FIFTY_RANDOM.txt', 
                 'follow-up', c("FIFTY_FIFTY_RANDOM"), 
                 Completers, FALSE)

# ---------------------------------------------------------------------------- #
# Longitudinal analyses with 3 conditions ----
# ---------------------------------------------------------------------------- #

# ITT

## Treament phase

writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/ITT/2conditions_vs_NEUTRAL.txt', 
                 'treatment', c("NEUTRAL", "BothPositive", "BothFiftyFifty"), 
                 ITT, TRUE)
writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/ITT/2conditions_vs_BothFiftyFifty.txt', 
                 'treatment', c("BothFiftyFifty", "BothPositive", "NEUTRAL"), 
                 ITT, TRUE)
writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/ITT/NEUTRAL.txt', 
                 'treatment', c("NEUTRAL"), 
                 ITT, FALSE)
writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/ITT/BothPositive.txt', 
                 'treatment', c("BothPositive"), 
                 ITT, FALSE)
writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/ITT/BothFiftyFifty.txt', 
                 'treatment', c("BothFiftyFifty"), 
                 ITT, FALSE)

## Follow-up phase

writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/ITT/2conditions_vs_NEUTRAL.txt', 
                 'follow-up', c("NEUTRAL", "BothPositive", "BothFiftyFifty"), 
                 ITT, TRUE)
writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/ITT/2conditions_vs_BothFiftyFifty.txt', 
                 'follow-up', c("BothFiftyFifty", "BothPositive", "NEUTRAL"), 
                 ITT, TRUE)
writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/ITT/NEUTRAL.txt', 
                 'follow-up', c("NEUTRAL"), 
                 ITT, FALSE)
writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/ITT/BothPositive.txt', 
                 'follow-up', c("BothPositive"), 
                 ITT, FALSE)
writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/ITT/BothFiftyFifty.txt', 
                 'follow-up', c("BothFiftyFifty"), 
                 ITT, FALSE)

# Completers

## Treament phase

writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/Completers/2conditions_vs_NEUTRAL.txt', 
                 'treatment', c("NEUTRAL", "BothPositive", "BothFiftyFifty"), 
                 Completers, TRUE)
writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/Completers/2conditions_vs_BothFiftyFifty.txt', 
                 'treatment', c("BothFiftyFifty", "BothPositive", "NEUTRAL"), 
                 Completers, TRUE)
writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/Completers/NEUTRAL.txt', 
                 'treatment', c("NEUTRAL"), 
                 Completers, FALSE)
writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/Completers/BothPositive.txt', 
                 'treatment', c("BothPositive"), 
                 Completers, FALSE)
writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Treatment Phase/Completers/BothFiftyFifty.txt', 
                 'treatment', c("BothFiftyFifty"), 
                 Completers, FALSE)

## Follow-up phase

writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/Completers/2conditions_vs_NEUTRAL.txt', 
                 'follow-up', c("NEUTRAL", "BothPositive", "BothFiftyFifty"), 
                 Completers, TRUE)
writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/Completers/2conditions_vs_BothFiftyFifty.txt', 
                 'follow-up', c("BothFiftyFifty", "BothPositive", "NEUTRAL"), 
                 Completers, TRUE)
writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/Completers/NEUTRAL.txt', 
                 'follow-up', c("NEUTRAL"), 
                 Completers, FALSE)
writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/Completers/BothPositive.txt', 
                 'follow-up', c("BothPositive"), 
                 Completers, FALSE)
writeresultsExpl(impList2, 
                 './Results/Longitudinal Outcome - Exploratory/Follow-Up Phase/Completers/BothFiftyFifty.txt', 
                 'follow-up', c("BothFiftyFifty"), 
                 Completers, FALSE)