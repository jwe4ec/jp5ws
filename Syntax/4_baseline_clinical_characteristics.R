# ---------------------------------------------------------------------------- #
# Baseline Clinical Characteristics
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Load packages ----
# ---------------------------------------------------------------------------- #

library(psych)

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/jp5ws/).

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

x <- read.csv("./Data/Clean/FTmainDataScales.csv")
p <- read.csv("./Data/Clean/FTmainAnalysisSamples.csv")

# Restrict to ITT subjects.

ITT <- subset(p, ittSample == 1)
length(ITT$participantId)
x <- x[which(x$participantId %in% ITT$participantId), ]

# Restrict to preTest.

x <- subset(x, session == "preTest")

# ---------------------------------------------------------------------------- #
# Compute descriptives ----
# ---------------------------------------------------------------------------- #

# There are no missing values at baseline.

describe(x$anxietyScale)
hist(x$anxietyScale)
describe(x$depressionScale)
hist(x$depressionScale)

sum(is.na(x$anxietyScale))
sum(is.na(x$depressionScale))

# Subjects with likely anxiety

table(x$anxietyScale)
sum(122, 155, 104, 222)/958

# Subjects with likely depression

table(x$depressionScale)
sum(136, 122, 85, 136)/958

# Subjects with likely anxiety or depression

for (i in 1:nrow(x)) {
  if (x$anxietyScale[i] >= 3 | x$depressionScale[i] >= 3) {
    x$clinicalAnxOrDep[i] <- 1
  } else {
    x$clinicalAnxOrDep[i] <- 0
  }
}

table(x$clinicalAnxOrDep)
table(x$clinicalAnxOrDep)/958

# Subjects with likely anxiety and depression

for (i in 1:nrow(x)) {
  if (x$anxietyScale[i] >= 3 & x$depressionScale[i] >= 3) {
    x$clinicalAnxAndDep[i] <- 1
  } else {
    x$clinicalAnxAndDep[i] <- 0
  }
}

table(x$clinicalAnxAndDep)
table(x$clinicalAnxAndDep)/958
