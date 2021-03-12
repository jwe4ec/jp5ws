# ---------------------------------------------------------------------------- #
# Item-Level Missingness
# Authors: Mehdi Boukhechba and Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Load packages ----
# ---------------------------------------------------------------------------- #

# None

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/jp5ws/).

# ---------------------------------------------------------------------------- #
# Import and prepare data ----
# ---------------------------------------------------------------------------- #

# Import item data and use sample data to restrict to ITT subjects.

x <- read.csv("./Data/Clean/FTmainDataItemsScales.csv")

p <- read.csv("./Data/Clean/FTmainAnalysisSamples.csv")
ITT <- subset(p, ittSample == 1)
Completers <- subset(p, txCompSample == 1)

ittx <- x[which(x$participantId %in% ITT$participantId), ]

# ---------------------------------------------------------------------------- #
# Compute proportions of item-level missing data ----
# ---------------------------------------------------------------------------- #

# posExpBiasScale

## mean of shortRest, settleIn, consideredAdvancement, and financiallySecure

scale_obs <- subset(ittx, !is.na(ittx$posExpBiasScale))
(num <- nrow(subset(scale_obs, is.na(scale_obs$shortRest) | 
                               is.na(scale_obs$settleIn) |
                               is.na(scale_obs$consideredAdvancement) |
                               is.na(scale_obs$financiallySecure))))
(denom <- nrow(scale_obs))
(prop <- num / denom)
(percent <- prop*100)

# negExpBiasScale

## mean of verySick, offend, stuck, and ruining

scale_obs <- subset(ittx, !is.na(ittx$negExpBiasScale))
(num <- nrow(subset(scale_obs, is.na(scale_obs$verySick) | 
                               is.na(scale_obs$offend) |
                               is.na(scale_obs$stuck) |
                               is.na(scale_obs$ruining))))
(denom <- nrow(scale_obs))
(prop <- num / denom)
(percent <- prop*100)

# anxietyScale

## sum of nervous and worry

scale_obs <- subset(ittx, !is.na(ittx$anxietyScale))
(num <- nrow(subset(scale_obs, is.na(scale_obs$nervous) | 
                               is.na(scale_obs$worry))))
(denom <- nrow(scale_obs))
(prop <- num / denom)
(percent <- prop*100)

# depressionScale

## sum of pleasure and depressed

scale_obs <- subset(ittx, !is.na(ittx$depressionScale))
(num <- nrow(subset(scale_obs, is.na(scale_obs$pleasure) | 
                               is.na(scale_obs$depressed))))
(denom <- nrow(scale_obs))
(prop <- num / denom)
(percent <- prop*100)

# selfEffScale

## mean of difficultTasks, performEffectively, and compared

scale_obs <- subset(ittx, !is.na(ittx$selfEffScale))
(num <- nrow(subset(scale_obs, is.na(scale_obs$difficultTasks) | 
                               is.na(scale_obs$performEffectively) |
                               is.na(scale_obs$compared))))
(denom <- nrow(scale_obs))
(prop <- num / denom)
(percent <- prop*100)

# growthMindScale

## mean of learnRev, particularThinking, and alwaysChangeThinking

scale_obs <- subset(ittx, !is.na(ittx$growthMindScale))
(num <- nrow(subset(scale_obs, is.na(scale_obs$learnRev) | 
                               is.na(scale_obs$particularThinking) |
                               is.na(scale_obs$alwaysChangeThinking))))
(denom <- nrow(scale_obs))
(prop <- num / denom)
(percent <- prop*100)

# optimismScale

## mean of wrongWillRev and hardlyEverRev

scale_obs <- subset(ittx, !is.na(ittx$optimismScale))
(num <- nrow(subset(scale_obs, is.na(scale_obs$wrongWillRev) | 
                               is.na(scale_obs$hardlyEverRev))))
(denom <- nrow(scale_obs))
(prop <- num / denom)
(percent <- prop*100)