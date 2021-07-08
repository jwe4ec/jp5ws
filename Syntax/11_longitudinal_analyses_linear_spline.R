# ---------------------------------------------------------------------------- #
# Longitudinal Analyses - Linear Spline
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
# (impList_ls) and one list for analysis model with 3 conditions (impList_ls2).

load("./Data/Imputed/impList_ls.RData")
load("./Data/Imputed/impList_ls2.RData")

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

# Define function for linear_spline analysis models of posExpBiasScale and negExpBiasScale

writeresults_ls <- function(data, path, c.levels, sample, compare) {
  print(paste0("Current working directory: ", path))
  
  sink(file = path)
  
  print(paste("Sample:", deparse(substitute(sample))))
  print(paste("Dataset:", deparse(substitute(data))))
  cat("\n")
  
  print("-------------------------------------------------------------------")
  cat("\n")
  
  for (outcome in list("posExpBiasScale", "negExpBiasScale")) {
    x <- data
    n_imp_datasets <- length(x)
    
    # Create an empty matrix to store the baseline SD in each imputed dataset
    # (computed below) and another empty matrix with the same dimensions to 
    # store the GMA d in each imputed dataset (computed below). Also create an 
    # empty vector to store the mean baseline SD across imputed datasets 
    # (computed below).
    
    if (compare) {
      sdMat <- matrix(0, nrow = n_imp_datasets, ncol = length(c.levels) - 1)
    } else {
      sdMat <- matrix(0, nrow = n_imp_datasets, ncol = 1)
    }
    
    dMat_at_session_4 <- sdMat
    dMat_at_follow_up <- sdMat
    
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
      
      # Restrict to the relevant assessment points based on when outcomes
      # were measured
      
      if (outcome != "posExpBiasScale" & outcome != "negExpBiasScale") {
        x[[i]] <- subset(x[[i]], session_int != 1 & session_int != 3)
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
    
    # Compute df as though data were complete
    
    a <- length(c.levels)
    b <- nrow(x[[1]])
    c <- length(unique(x[[1]]$participantId))
    
    n_time_effects <- length(c("time1", "time2"))
    n_cond_time_effects <- n_time_effects*(length(c.levels) - 1)
    
    d <- n_time_effects + n_cond_time_effects
    
    if (compare) {
      fml = as.formula(paste(outcome, "~ condition*time1 + condition*time2"))
      
      if (length(c.levels) == 5) {
        df = c(b - c - d,         # intercept
               rep(c - a, 4),     # condition effects
               b - c - d,         # time1 effect
               b - c - d,         # time2 effect
               rep(b - c - d, 4), # condition:time1 effects
               rep(b - c - d, 4)) # condition:time2 effects
      } else if (length(c.levels) == 3) {
        df = c(b - c - d,         # intercept
               rep(c - a, 2),     # condition effects
               b - c - d,         # time1 effect
               b - c - d,         # time2 effect
               rep(b - c - d, 2), # condition:time1 effects
               rep(b - c - d, 2)) # condition:time2 effects
      }
    } else {
      fml = as.formula(paste(outcome, "~ time1 + time2"))
      
      df =     c(b - c - d,         # intercept
                 b - c - d,         # time1 effect
                 b - c - d)         # time2 effect
    }
    
    print(paste("Outcome:", outcome))
    cat("\n")
    
    print("Number of observations (after imputation) at each session by condition:")
    cat("\n")
    obs <- as.data.frame.matrix(table(x[[1]]$session_int, x[[1]]$condition))
    print(data.frame(obs, 
                     "session_int" = row.names(obs), 
                     "time1" = c(0, 1, 2, 3, 4, 4), 
                     "time2" = c(0, 0, 0, 0, 0, 1)),
          row.names = FALSE)
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
    
    # Run mixed effects models. Include random slope for time1, except where 
    # there are convergence errors, in which case use only random intercept.
    # Analyze each imputed dataset individually and then pool results using 
    # Rubin's rules (i.e., testEstimates function). Use adjusted df as though 
    # data were complete. Compute 97.5% confidence intervals based on
    # Bonferroni-corrected alpha level of .05/2 = .025.
    
    alpha = .05/2
    
    modelList <- with(x, lme(fml, 
                             random = ~ 1 + time1 | participantId,
                             control = lmeControl(opt = "optim"), 
                             method = "REML"))
    cat("\n")
    print(modelList[[1]]$call)
    pooled <- testEstimates(modelList, extra.pars = TRUE, df.com = df)
    print(pooled)
    ci <- confint(pooled, level = 1 - alpha)
    print(ci)
    
    # Compute effect sizes for linear spline, adapting Equation 2 for quadratic
    # models from Feingold (2018). Use Method 2, where effect size estimates 
    # are computed after analysis phase results were already pooled. Method 2
    # effect size estimates are similar to those obtained via Method 1, where 
    # effect size estimates are pooled from effect sizes computed separately in
    # each imputed dataset. Confidence intervals for effect sizes are not 
    # computed because post hoc equations for computing standard errors of GMA 
    # d effect sizes based on slope differences between conditions have not been 
    # derived (Feingold, personal communication, 6/25/2021). For consistency, 
    # use Method 2 for within-group effect size estimates as well.
    
    if (compare) {
      cat("\n")
      print("Between-groups growth modeling analysis d:")
      cat("\n")
      
      print("Method 1")
      
      for (i in 1:length(modelList)) {
        for (j in 1:ncol(dMat_at_session_4)) {
          interactionBeta_time1 <- 
            as.numeric(modelList[[i]]$coefficients$fixed[paste0("condition", 
                                                                c.levels[1 + j], 
                                                                ":time1")])
          interactionBeta_time2 <- 
            as.numeric(modelList[[i]]$coefficients$fixed[paste0("condition", 
                                                                c.levels[1 + j], 
                                                                ":time2")])
          dMat_at_session_4[i, j] <- 
            (interactionBeta_time1*4 + interactionBeta_time2*0)/sdMat[i, j]
        }
        for (j in 1:ncol(dMat_at_follow_up)) {
          interactionBeta_time1 <- 
            as.numeric(modelList[[i]]$coefficients$fixed[paste0("condition", 
                                                                c.levels[1 + j], 
                                                                ":time1")])
          interactionBeta_time2 <- 
            as.numeric(modelList[[i]]$coefficients$fixed[paste0("condition", 
                                                                c.levels[1 + j], 
                                                                ":time2")])
          dMat_at_follow_up[i, j] <- 
            (interactionBeta_time1*4 + interactionBeta_time2*1)/sdMat[i, j]
        }
      }
      
      cat("\n")
      print("Estimates pooled from effect sizes computed in each imputed dataset:")
      cat("\n")
      
      print("At session 4:")
      for (j in 1:ncol(dMat_at_session_4)) {
        print(paste0("Based on condition", c.levels[1 + j], ":time1",
                     " and condition", c.levels[1 + j], ":time2", " = ",
                     mean(dMat_at_session_4[, j])))
      }
      cat("\n")
      print("At follow-up:")
      for (j in 1:ncol(dMat_at_follow_up)) {
        print(paste0("Based on condition", c.levels[1 + j], ":time1",
                     " and condition", c.levels[1 + j], ":time2", " = ",
                     mean(dMat_at_follow_up[, j])))
      }
      
      cat("\n")
      print("Method 2")
      cat("\n")
      print("Estimates computed after analysis phase results were already pooled:")
      cat("\n")
      print("Based on condition:time1 and condition:time2 for each condition")
      cat("\n")
      
      betaInteraction_time1 <- 
        data.frame(pooled$estimates[grep(":time1", 
                                         row.names(pooled$estimates), 
                                         value = TRUE), 
                                    "Estimate"])
      betaInteraction_time2 <- 
        data.frame(pooled$estimates[grep(":time2", 
                                         row.names(pooled$estimates), 
                                         value = TRUE), 
                                    "Estimate"])
      
      colnames(betaInteraction_time1) <- "Estimate"
      colnames(betaInteraction_time2) <- "Estimate"
      
      betaInteraction_session_4_ES <- 
        (betaInteraction_time1*4 + betaInteraction_time2*0)/sdVec
      betaInteraction_follow_up_ES <- 
        (betaInteraction_time1*4 + betaInteraction_time2*1)/sdVec
      
      colnames(betaInteraction_session_4_ES) <- "Effect Size at Session 4"
      colnames(betaInteraction_follow_up_ES) <- "Effect Size at Follow-Up"
      
      rownames(betaInteraction_session_4_ES) <- 
        gsub(":.*", "", rownames(betaInteraction_session_4_ES))
      rownames(betaInteraction_follow_up_ES) <-
        gsub(":.*", "", rownames(betaInteraction_follow_up_ES))
      
      print(betaInteraction_session_4_ES)
      cat("\n")
      print(betaInteraction_follow_up_ES)
    } else {
      cat("\n")
      print("Within-group growth modeling analysis d:")
      cat("\n")
      
      print("Method 1")
      cat("\n")
      
      for (i in 1:length(modelList)) {
        time1Beta <- as.numeric(modelList[[i]]$coefficients$fixed["time1"])
        time2Beta <- as.numeric(modelList[[i]]$coefficients$fixed["time2"])
        
        dMat_at_session_4[i] <- (time1Beta*4 + time2Beta*0)/sdMat[i]
        dMat_at_follow_up[i] <- (time1Beta*4 + time2Beta*1)/sdMat[i]
      }
      
      print("Estimate pooled from effect size computed in each imputed dataset:")
      cat("\n")
      
      print("At session 4:")
      print(paste0("Based on time1 and time2", " = ", mean(dMat_at_session_4)))
      cat("\n")
      print("At follow-up:")
      print(paste0("Based on time1 and time2", " = ", mean(dMat_at_follow_up)))
      
      cat("\n")
      print("Method 2")
      cat("\n")
      
      print("Estimate computed after analysis phase results were already pooled:")
      cat("\n")
      
      betatime1 <- pooled$estimates["time1", "Estimate"]
      betatime2 <- pooled$estimates["time2", "Estimate"]
      
      beta_session_4_ES <- (betatime1*4 + betatime2*0)/sdVec
      beta_follow_up_ES <- (betatime1*4 + betatime2*1)/sdVec
      
      print(paste0("At session 4: ", beta_session_4_ES))
      print(paste0("At follow-up: ", beta_follow_up_ES))
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

writeresults_ls(impList_ls, 
                './Results/Longitudinal Outcome - Linear Spline/ITT/4conditions_vs_POSITIVE.txt',
                c("POSITIVE", "POSITIVE_NEGATION",
                  "FIFTY_FIFTY_BLOCKED", "FIFTY_FIFTY_RANDOM", "NEUTRAL"), 
                ITT, TRUE)
writeresults_ls(impList_ls, 
                './Results/Longitudinal Outcome - Linear Spline/ITT/4conditions_vs_FIFTY_FIFTY_RANDOM.txt',
                c("FIFTY_FIFTY_RANDOM", "FIFTY_FIFTY_BLOCKED",
                  "POSITIVE_NEGATION", "POSITIVE", "NEUTRAL"), 
                ITT, TRUE)
writeresults_ls(impList_ls, 
                './Results/Longitudinal Outcome - Linear Spline/ITT/POSITIVE_NEGATION.txt', 
                c("POSITIVE_NEGATION"), 
                ITT, FALSE)
writeresults_ls(impList_ls, 
                './Results/Longitudinal Outcome - Linear Spline/ITT/POSITIVE.txt', 
                c("POSITIVE"), 
                ITT, FALSE)
writeresults_ls(impList_ls, 
                './Results/Longitudinal Outcome - Linear Spline/ITT/FIFTY_FIFTY_BLOCKED.txt', 
                c("FIFTY_FIFTY_BLOCKED"), 
                ITT, FALSE)
writeresults_ls(impList_ls, 
                './Results/Longitudinal Outcome - Linear Spline/ITT/FIFTY_FIFTY_RANDOM.txt', 
                c("FIFTY_FIFTY_RANDOM"), 
                ITT, FALSE)

# Completers

writeresults_ls(impList_ls, 
                './Results/Longitudinal Outcome - Linear Spline/Completers/4conditions_vs_POSITIVE.txt',
                c("POSITIVE", "POSITIVE_NEGATION",
                  "FIFTY_FIFTY_BLOCKED", "FIFTY_FIFTY_RANDOM", "NEUTRAL"), 
                Completers, TRUE)
writeresults_ls(impList_ls, 
                './Results/Longitudinal Outcome - Linear Spline/Completers/4conditions_vs_FIFTY_FIFTY_RANDOM.txt',
                c("FIFTY_FIFTY_RANDOM", "FIFTY_FIFTY_BLOCKED",
                  "POSITIVE_NEGATION", "POSITIVE", "NEUTRAL"), 
                Completers, TRUE)
writeresults_ls(impList_ls, 
                './Results/Longitudinal Outcome - Linear Spline/Completers/POSITIVE_NEGATION.txt', 
                c("POSITIVE_NEGATION"), 
                Completers, FALSE)
writeresults_ls(impList_ls, 
                './Results/Longitudinal Outcome - Linear Spline/Completers/POSITIVE.txt', 
                c("POSITIVE"), 
                Completers, FALSE)
writeresults_ls(impList_ls, 
                './Results/Longitudinal Outcome - Linear Spline/Completers/FIFTY_FIFTY_BLOCKED.txt', 
                c("FIFTY_FIFTY_BLOCKED"), 
                Completers, FALSE)
writeresults_ls(impList_ls, 
                './Results/Longitudinal Outcome - Linear Spline/Completers/FIFTY_FIFTY_RANDOM.txt', 
                c("FIFTY_FIFTY_RANDOM"), 
                Completers, FALSE)

# ---------------------------------------------------------------------------- #
# Longitudinal analyses with 3 conditions ----
# ---------------------------------------------------------------------------- #

# ITT

writeresults_ls(impList_ls2, 
                './Results/Longitudinal Outcome - Linear Spline/ITT/2conditions_vs_NEUTRAL.txt', 
                c("NEUTRAL", "BothPositive", "BothFiftyFifty"), 
                ITT, TRUE)
writeresults_ls(impList_ls2, 
                './Results/Longitudinal Outcome - Linear Spline/ITT/2conditions_vs_BothFiftyFifty.txt', 
                c("BothFiftyFifty", "BothPositive", "NEUTRAL"), 
                ITT, TRUE)
writeresults_ls(impList_ls2, 
                './Results/Longitudinal Outcome - Linear Spline/ITT/NEUTRAL.txt', 
                c("NEUTRAL"), 
                ITT, FALSE)
writeresults_ls(impList_ls2, 
                './Results/Longitudinal Outcome - Linear Spline/ITT/BothPositive.txt', 
                c("BothPositive"), 
                ITT, FALSE)
writeresults_ls(impList_ls2, 
                './Results/Longitudinal Outcome - Linear Spline/ITT/BothFiftyFifty.txt', 
                c("BothFiftyFifty"), 
                ITT, FALSE)

# Completers

writeresults_ls(impList_ls2, 
                './Results/Longitudinal Outcome - Linear Spline/Completers/2conditions_vs_NEUTRAL.txt', 
                c("NEUTRAL", "BothPositive", "BothFiftyFifty"), 
                Completers, TRUE)
writeresults_ls(impList_ls2, 
                './Results/Longitudinal Outcome - Linear Spline/Completers/2conditions_vs_BothFiftyFifty.txt', 
                c("BothFiftyFifty", "BothPositive", "NEUTRAL"), 
                Completers, TRUE)
writeresults_ls(impList_ls2, 
                './Results/Longitudinal Outcome - Linear Spline/Completers/NEUTRAL.txt', 
                c("NEUTRAL"), 
                Completers, FALSE)
writeresults_ls(impList_ls2, 
                './Results/Longitudinal Outcome - Linear Spline/Completers/BothPositive.txt', 
                c("BothPositive"), 
                Completers, FALSE)
writeresults_ls(impList_ls2, 
                './Results/Longitudinal Outcome - Linear Spline/Completers/BothFiftyFifty.txt', 
                c("BothFiftyFifty"), 
                Completers, FALSE)