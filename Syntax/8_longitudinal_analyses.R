# ---------------------------------------------------------------------------- #
# Longitudinal Analyses
# Authors: Mehdi Boukhechba and Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Load packages and set seed ----
# ---------------------------------------------------------------------------- #

library(mitml)
library(nlme)

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
# Define histbytime function ----
# ---------------------------------------------------------------------------- #

# Define function to investigate distributions of imputed data at each time 
# point and capture extent of imputed values beyond range of plausible values.

histbytime <- function(data, path, outcome, xmin, xmax, ymin, ymax, scalemin, scalemax) {
  sink(file = paste0(path, ".txt"))
  
  x <- data
  
  print(paste("Dataset:", deparse(substitute(data))), quote = FALSE)
  print(paste0("Outcome: ", outcome), quote = FALSE)
  print(paste0("Minimum plausible scale value: ", scalemin), quote = FALSE)
  print(paste0("Maximum plausible scale value: ", scalemax), quote = FALSE)
  cat("\n")
  
  pdf(paste0(path, ".pdf"), width = 9, height = 7)
  
  par(new = FALSE)
  par(mfrow = c(2, 3))
  
  for (k in unique(x[[1]]$session_int)) {
    n <- nrow(x[[1]][x[[1]]$session_int == k, ])
    
    iabove <- rep(0, length(x))
    ibelow <- rep(0, length(x))
    imax <- rep(0, length(x))
    imin <- rep(0, length(x))
    outrange <- data.frame(iabove, ibelow, imax, imin)
    
    for (i in 1:length(x)) {
      outrange[i, "ibelow"] <- sum(x[[i]][x[[i]]$session_int == k, outcome] < scalemin)
      outrange[i, "iabove"] <- sum(x[[i]][x[[i]]$session_int == k, outcome] > scalemax)
      outrange[i, "imin"] <- min(x[[i]][x[[i]]$session_int == k, outcome])
      outrange[i, "imax"] <- max(x[[i]][x[[i]]$session_int == k, outcome])
      
      hist(x[[i]][x[[i]]$session_int == k, outcome],
           main = paste0("session_int = ", k),
           breaks = c(xmin:xmax),
           right = FALSE,
           xlim = range(xmin, xmax),
           ylim = range(ymin, ymax),
           xlab = outcome)
      par(new = TRUE)
    }
    
    print(paste0("For session_int = ", k, ":"), quote = FALSE)
    cat("\n")
    print(paste0("Mean number of imputed values below plausible range = ", 
                 mean(outrange$ibelow)), quote = FALSE)
    print(paste0("Mean percent of imputed values below plausible range = ", 
                 round(mean((outrange$ibelow/n)*100), digits = 2), "%"), quote = FALSE)
    print(paste0("Mean minimum imputed value = ", 
                 round(mean(outrange$imin), digits = 2)), quote = FALSE)
    print(paste0("Absolute minimum imputed value = ", 
                 round(min(outrange$imin), digits = 2)), quote = FALSE)
    cat("\n")
    print(paste0("Mean number of imputed values above plausible range = ", 
                 mean(outrange$iabove)), quote = FALSE)
    print(paste0("Mean percent of imputed values above plausible range = ", 
                 round(mean((outrange$iabove/n)*100), digits = 2), "%"), quote = FALSE)
    print(paste0("Mean maximum imputed value = ", 
                 round(mean(outrange$imax), digits = 2)), quote = FALSE)
    print(paste0("Absolute maximum imputed value = ",
                 round(max(outrange$imax), digits = 2)), quote = FALSE)
    cat("\n")
    
    abline(v = scalemin)
    abline(v = scalemax)
    par(new = FALSE)
  }
  dev.off()
  par(mfrow = c(1,1))
  
  sink()
}

# ---------------------------------------------------------------------------- #
# Investigate imputed data distributions and implausible values ----
# ---------------------------------------------------------------------------- #

histbytime(impList, 
           './Results/Imputation/impList/histograms and extreme values/posExpBiasScale', 
           "posExpBiasScale", -5, 13, 0, 400, 1, 7)
histbytime(impList, 
           './Results/Imputation/impList/histograms and extreme values/negExpBiasScale', 
           "negExpBiasScale", -5, 13, 0, 400, 1, 7)
histbytime(impList, 
           './Results/Imputation/impList/histograms and extreme values/depressionScale', 
           "depressionScale", -9, 12, 0, 400, 0, 6)
histbytime(impList, 
           './Results/Imputation/impList/histograms and extreme values/anxietyScale', 
           "anxietyScale", -9, 12, 0, 400, 0, 6)
histbytime(impList, 
           './Results/Imputation/impList/histograms and extreme values/selfEffScale', 
           "selfEffScale", -4, 8, 0, 400, 0, 4)
histbytime(impList, 
           './Results/Imputation/impList/histograms and extreme values/growthMindScale', 
           "growthMindScale", -4, 8, 0, 400, 0, 4)
histbytime(impList, 
           './Results/Imputation/impList/histograms and extreme values/optimismScale', 
           "optimismScale", -4, 8, 0, 400, 0, 4)

histbytime(impList2, 
           './Results/Imputation/impList2/histograms and extreme values/posExpBiasScale', 
           "posExpBiasScale", -5, 13, 0, 400, 1, 7)
histbytime(impList2, 
           './Results/Imputation/impList2/histograms and extreme values/negExpBiasScale', 
           "negExpBiasScale", -5, 13, 0, 400, 1, 7)
histbytime(impList2, 
           './Results/Imputation/impList2/histograms and extreme values/depressionScale', 
           "depressionScale", -9, 12, 0, 400, 0, 6)
histbytime(impList2, 
           './Results/Imputation/impList2/histograms and extreme values/anxietyScale', 
           "anxietyScale", -9, 12, 0, 400, 0, 6)
histbytime(impList2, 
           './Results/Imputation/impList2/histograms and extreme values/selfEffScale', 
           "selfEffScale", -4, 8, 0, 400, 0, 4)
histbytime(impList2, 
           './Results/Imputation/impList2/histograms and extreme values/growthMindScale', 
           "growthMindScale", -4, 8, 0, 400, 0, 4)
histbytime(impList2, 
           './Results/Imputation/impList2/histograms and extreme values/optimismScale', 
           "optimismScale", -4, 8, 0, 400, 0, 4)

# ---------------------------------------------------------------------------- #
# Load sample data ----
# ---------------------------------------------------------------------------- #

# Load sample data and store ITT subjects and Completers.

p <- read.csv("./Data/Clean/FTmainAnalysisSamples.csv")
p$X <- NULL
ITT <- subset(p, ittSample == 1)
Completers <- subset(p, txCompSample == 1)

# ---------------------------------------------------------------------------- #
# Define writeresults function ----
# ---------------------------------------------------------------------------- #

# Define function for analysis models.

writeresults <- function(data, path, phase, c.levels, sample, compare) {
  print(paste0("Current working directory: ", path))
  
  sink(file = path)
  
  print(paste("Sample:", deparse(substitute(sample))))
  print(paste("Study Phase:", phase))
  print(paste("Dataset:", deparse(substitute(data))))
  cat("\n")
  
  print("-------------------------------------------------------------------")
  cat("\n")
  
  for (outcome in list("posExpBiasScale", "negExpBiasScale", "depressionScale",
                      "anxietyScale", "selfEffScale", "growthMindScale",
                      "optimismScale")) {
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
    
    if (phase == "follow-up" |
        
        (deparse(substitute(data)) == "impList" & phase == "treatment" &
         identical(c.levels, c("POSITIVE_NEGATION")) &
         deparse(substitute(sample)) == "Completers" &
         compare == FALSE & outcome == "selfEffScale") |
        
        (deparse(substitute(data)) == "impList" & phase == "treatment" &
         identical(c.levels, c("POSITIVE")) &
         deparse(substitute(sample)) == "Completers" &
         compare == FALSE & outcome == "selfEffScale") |
        
        (deparse(substitute(data)) == "impList" & phase == "treatment" &
         identical(c.levels, c("FIFTY_FIFTY_BLOCKED")) &
         deparse(substitute(sample)) == "Completers" &
         compare == FALSE & outcome == "growthMindScale")) {
      modelList <- with(x, lme(fml, 
                               random = ~ 1 | participantId,
                               control = lmeControl(opt = "optim"), 
                               method = "REML"))
      cat("\n")
      print(modelList[[1]]$call)
      pooled <- testEstimates(modelList, var.comp = TRUE, df.com = df)
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
      pooled <- testEstimates(modelList, var.comp = TRUE, df.com = df)
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

writeresults(impList, 
             './Results/Longitudinal Outcome/Treatment Phase/ITT/4conditions_vs_POSITIVE.txt',
             'treatment', c("POSITIVE", "POSITIVE_NEGATION", 
                            "FIFTY_FIFTY_BLOCKED", "FIFTY_FIFTY_RANDOM", "NEUTRAL"), 
             ITT, TRUE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Treatment Phase/ITT/4conditions_vs_FIFTY_FIFTY_RANDOM.txt',
             'treatment', c("FIFTY_FIFTY_RANDOM", "FIFTY_FIFTY_BLOCKED", 
                            "POSITIVE_NEGATION", "POSITIVE", "NEUTRAL"), 
             ITT, TRUE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Treatment Phase/ITT/POSITIVE_NEGATION.txt', 
             'treatment', c("POSITIVE_NEGATION"), 
             ITT, FALSE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Treatment Phase/ITT/POSITIVE.txt', 
             'treatment', c("POSITIVE"), 
             ITT, FALSE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Treatment Phase/ITT/FIFTY_FIFTY_BLOCKED.txt', 
             'treatment', c("FIFTY_FIFTY_BLOCKED"), 
             ITT, FALSE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Treatment Phase/ITT/FIFTY_FIFTY_RANDOM.txt', 
             'treatment', c("FIFTY_FIFTY_RANDOM"), 
             ITT, FALSE)

## Follow-up phase

writeresults(impList, 
             './Results/Longitudinal Outcome/Follow-Up Phase/ITT/4conditions_vs_POSITIVE.txt', 
             'follow-up', c("POSITIVE", "POSITIVE_NEGATION", 
                            "FIFTY_FIFTY_BLOCKED", "FIFTY_FIFTY_RANDOM", "NEUTRAL"), 
             ITT, TRUE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Follow-Up Phase/ITT/4conditions_vs_FIFTY_FIFTY_RANDOM.txt', 
             'follow-up', c("FIFTY_FIFTY_RANDOM", "FIFTY_FIFTY_BLOCKED", 
                            "POSITIVE_NEGATION", "POSITIVE", "NEUTRAL"), 
             ITT, TRUE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Follow-Up Phase/ITT/POSITIVE_NEGATION.txt', 
             'follow-up', c("POSITIVE_NEGATION"), 
             ITT, FALSE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Follow-Up Phase/ITT/POSITIVE.txt', 
             'follow-up', c("POSITIVE"), 
             ITT, FALSE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Follow-Up Phase/ITT/FIFTY_FIFTY_BLOCKED.txt', 
             'follow-up', c("FIFTY_FIFTY_BLOCKED"),
             ITT, FALSE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Follow-Up Phase/ITT/FIFTY_FIFTY_RANDOM.txt', 
             'follow-up', c("FIFTY_FIFTY_RANDOM"), 
             ITT, FALSE)

# Completers

## Treament phase

writeresults(impList, 
             './Results/Longitudinal Outcome/Treatment Phase/Completers/4conditions_vs_POSITIVE.txt', 
             'treatment', c("POSITIVE", "POSITIVE_NEGATION", 
                            "FIFTY_FIFTY_BLOCKED", "FIFTY_FIFTY_RANDOM", "NEUTRAL"), 
             Completers, TRUE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Treatment Phase/Completers/4conditions_vs_FIFTY_FIFTY_RANDOM.txt', 
             'treatment', c("FIFTY_FIFTY_RANDOM", "FIFTY_FIFTY_BLOCKED", 
                            "POSITIVE_NEGATION", "POSITIVE", "NEUTRAL"), 
             Completers, TRUE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Treatment Phase/Completers/POSITIVE_NEGATION.txt', 
             'treatment', c("POSITIVE_NEGATION"), 
             Completers, FALSE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Treatment Phase/Completers/POSITIVE.txt', 
             'treatment', c("POSITIVE"), 
             Completers, FALSE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Treatment Phase/Completers/FIFTY_FIFTY_BLOCKED.txt', 
             'treatment', c("FIFTY_FIFTY_BLOCKED"), 
             Completers, FALSE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Treatment Phase/Completers/FIFTY_FIFTY_RANDOM.txt', 
             'treatment', c("FIFTY_FIFTY_RANDOM"), 
             Completers, FALSE)

## Follow-up phase

writeresults(impList, 
             './Results/Longitudinal Outcome/Follow-Up Phase/Completers/4conditions_vs_POSITIVE.txt', 
             'follow-up', c("POSITIVE", "POSITIVE_NEGATION", 
                            "FIFTY_FIFTY_BLOCKED", "FIFTY_FIFTY_RANDOM", "NEUTRAL"), 
             Completers, TRUE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Follow-Up Phase/Completers/4conditions_vs_FIFTY_FIFTY_RANDOM.txt', 
             'follow-up', c("FIFTY_FIFTY_RANDOM", "FIFTY_FIFTY_BLOCKED", 
                            "POSITIVE_NEGATION", "POSITIVE", "NEUTRAL"), 
             Completers, TRUE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Follow-Up Phase/Completers/POSITIVE_NEGATION.txt', 
             'follow-up', c("POSITIVE_NEGATION"), 
             Completers, FALSE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Follow-Up Phase/Completers/POSITIVE.txt', 
             'follow-up', c("POSITIVE"), 
             Completers, FALSE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Follow-Up Phase/Completers/FIFTY_FIFTY_BLOCKED.txt', 
             'follow-up', c("FIFTY_FIFTY_BLOCKED"), 
             Completers, FALSE)
writeresults(impList, 
             './Results/Longitudinal Outcome/Follow-Up Phase/Completers/FIFTY_FIFTY_RANDOM.txt', 
             'follow-up', c("FIFTY_FIFTY_RANDOM"), 
             Completers, FALSE)

# ---------------------------------------------------------------------------- #
# Longitudinal analyses with 3 conditions ----
# ---------------------------------------------------------------------------- #

# ITT

## Treament phase

writeresults(impList2, 
             './Results/Longitudinal Outcome/Treatment Phase/ITT/2conditions_vs_NEUTRAL.txt', 
             'treatment', c("NEUTRAL", "BothPositive", "BothFiftyFifty"), 
             ITT, TRUE)
writeresults(impList2, 
             './Results/Longitudinal Outcome/Treatment Phase/ITT/2conditions_vs_BothFiftyFifty.txt', 
             'treatment', c("BothFiftyFifty", "BothPositive", "NEUTRAL"), 
             ITT, TRUE)
writeresults(impList2, 
             './Results/Longitudinal Outcome/Treatment Phase/ITT/NEUTRAL.txt', 
             'treatment', c("NEUTRAL"), 
             ITT, FALSE)
writeresults(impList2, 
             './Results/Longitudinal Outcome/Treatment Phase/ITT/BothPositive.txt', 
             'treatment', c("BothPositive"), 
             ITT, FALSE)
writeresults(impList2, 
             './Results/Longitudinal Outcome/Treatment Phase/ITT/BothFiftyFifty.txt', 
             'treatment', c("BothFiftyFifty"), 
             ITT, FALSE)

## Follow-up phase

writeresults(impList2, 
             './Results/Longitudinal Outcome/Follow-Up Phase/ITT/2conditions_vs_NEUTRAL.txt', 
             'follow-up', c("NEUTRAL", "BothPositive", "BothFiftyFifty"), 
             ITT, TRUE)
writeresults(impList2, 
             './Results/Longitudinal Outcome/Follow-Up Phase/ITT/2conditions_vs_BothFiftyFifty.txt', 
             'follow-up', c("BothFiftyFifty", "BothPositive", "NEUTRAL"), 
             ITT, TRUE)
writeresults(impList2, 
             './Results/Longitudinal Outcome/Follow-Up Phase/ITT/NEUTRAL.txt', 
             'follow-up', c("NEUTRAL"), 
             ITT, FALSE)
writeresults(impList2, 
             './Results/Longitudinal Outcome/Follow-Up Phase/ITT/BothPositive.txt', 
             'follow-up', c("BothPositive"), 
             ITT, FALSE)
writeresults(impList2, 
             './Results/Longitudinal Outcome/Follow-Up Phase/ITT/BothFiftyFifty.txt', 
             'follow-up', c("BothFiftyFifty"), 
             ITT, FALSE)

# Completers

## Treament phase

writeresults(impList2, 
             './Results/Longitudinal Outcome/Treatment Phase/Completers/2conditions_vs_NEUTRAL.txt', 
             'treatment', c("NEUTRAL", "BothPositive", "BothFiftyFifty"), 
             Completers, TRUE)
writeresults(impList2, 
             './Results/Longitudinal Outcome/Treatment Phase/Completers/2conditions_vs_BothFiftyFifty.txt', 
             'treatment', c("BothFiftyFifty", "BothPositive", "NEUTRAL"), 
             Completers, TRUE)
writeresults(impList2, 
             './Results/Longitudinal Outcome/Treatment Phase/Completers/NEUTRAL.txt', 
             'treatment', c("NEUTRAL"), 
             Completers, FALSE)
writeresults(impList2, 
             './Results/Longitudinal Outcome/Treatment Phase/Completers/BothPositive.txt', 
             'treatment', c("BothPositive"), 
             Completers, FALSE)
writeresults(impList2, 
             './Results/Longitudinal Outcome/Treatment Phase/Completers/BothFiftyFifty.txt', 
             'treatment', c("BothFiftyFifty"), 
             Completers, FALSE)

## Follow-up phase

writeresults(impList2, 
             './Results/Longitudinal Outcome/Follow-Up Phase/Completers/2conditions_vs_NEUTRAL.txt', 
             'follow-up', c("NEUTRAL", "BothPositive", "BothFiftyFifty"), 
             Completers, TRUE)
writeresults(impList2, 
             './Results/Longitudinal Outcome/Follow-Up Phase/Completers/2conditions_vs_BothFiftyFifty.txt', 
             'follow-up', c("BothFiftyFifty", "BothPositive", "NEUTRAL"), 
             Completers, TRUE)
writeresults(impList2, 
             './Results/Longitudinal Outcome/Follow-Up Phase/Completers/NEUTRAL.txt', 
             'follow-up', c("NEUTRAL"), 
             Completers, FALSE)
writeresults(impList2, 
             './Results/Longitudinal Outcome/Follow-Up Phase/Completers/BothPositive.txt', 
             'follow-up', c("BothPositive"), 
             Completers, FALSE)
writeresults(impList2, 
             './Results/Longitudinal Outcome/Follow-Up Phase/Completers/BothFiftyFifty.txt', 
             'follow-up', c("BothFiftyFifty"), 
             Completers, FALSE)