# ---------------------------------------------------------------------------- #
# Item Selection
# Author: M. Joseph Meyer
#
# Revision History:
#
# Dec 5 2016 1:37:54 AM:   TempletonData.R was created.
# Oct 28 2019 7:16:30 AM:  Jeremy W. Eberle renamed file to "0_item_selection.R"
#                          and reformatted the code.
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Check R version and load packages ----
# ---------------------------------------------------------------------------- #

# Note: Exact R version used for this script is unknown

library(groundhog)
groundhog_day <- "2019-11-02"

groundhog.library(psych, groundhog_day)
groundhog.library(mirt, groundhog_day)
groundhog.library(mokken, groundhog_day)

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/jp5ws/).

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Goal: Pick 2-3 items for each scale.

# Info about scales:

# For the NGSES, the composite score (all the composites are labeled "total") is 
# just the sum of the items. Higher scores indicate more self-efficacy.

# For the LOT-R, items 3, 7, and 9 are reverse scored. Items 2, 5, 6, and 8 are 
# fillers and not included in composite. Higher scores indicate more optimism.

# For the MPBS items 1, 4, 6, and 8 are reverse scored. Lower scores indicate 
# more growth mindset.

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

# Note: These data are from the Good Prospection Study, not the Future Thinking
# study, which is the focus of the present manuscript.

scaleData <- read.csv("./Data/Temp/templeton_long_subset.csv")

summary(scaleData$session)
describe(scaleData)

# Separating scales

ngsesData <- scaleData[, c(1:2, 29, 3:10)]
lotrData <- scaleData[, c(1:2, 30, 11:20)]
mpbsData <- scaleData[, c(1:2, 31, 21:28)]

# Number of items

ncol(ngsesData[, -1:-3])
ncol(lotrData[, -1:-3])
ncol(mpbsData[, -1:-3])

# Reversed items

# Note: No items are reversed for the ngses.
lotrReversed <- c(3, 7, 9)
mpbsReversed <- c(1, 4, 6, 8)

# Ratio of missing rows

nrow(na.omit(ngsesData[, -1:-3]))/nrow(ngsesData[, -1:-3])
nrow(na.omit(lotrData[, -1:-3]))/nrow(lotrData[, -1:-3])
nrow(na.omit(mpbsData[, -1:-3]))/nrow(mpbsData[, -1:-3])

# Looking at skew of each dataset

describe(ngsesData[, -1:-3]) # Item 3 is more skewed (although all are skewed).
describe(lotrData[, -1:-3])  # Item 5 is definitely skewed, and item 6 probably
                             # is as well.
describe(mpbsData[, -1:-3])  # Item 8 is a bit skewed.

# Reverse-scoring relevant items (used only for initial principal components 
# analyses on full dataset below; Cronbach's alpha analyses below also account
# for reverse scoring, but other analyses below do not)

lotrRevData <- lotrData[, -1:-3]
mpbsRevData <- mpbsData[, -1:-3]
lotrRevData[, lotrReversed] <- 6 - lotrRevData[, lotrReversed]
mpbsRevData[, mpbsReversed] <- 5 - mpbsRevData[, mpbsReversed]

# ---------------------------------------------------------------------------- #
# Analyze full dataset (Part 1) ----
# ---------------------------------------------------------------------------- #

# Conduct principal components analyses on all nonmissing rows with reverse-
# scoring implemented for relevant items

princomp(na.omit(ngsesData[, -1:-3]), cor = TRUE) # 1
princomp(na.omit(lotrRevData), cor = TRUE)        # 3
princomp(na.omit(mpbsRevData), cor = TRUE)        # 2

# ---------------------------------------------------------------------------- #
# Separating into training and testing datasets ----
# ---------------------------------------------------------------------------- #

set.seed(1234)

ngsesTrainRows <- sample(1:nrow(ngsesData), nrow(ngsesData)*.6)
lotrTrainRows <- sample(1:nrow(lotrData), nrow(lotrData)*.6)
mpbsTrainRows <- sample(1:nrow(mpbsData), nrow(mpbsData)*.6)

ngsesTrainData <- ngsesData[ngsesTrainRows, -1:-3]
lotrTrainData <- lotrData[lotrTrainRows, -1:-3]
mpbsTrainData <- mpbsData[mpbsTrainRows, -1:-3]

ngsesTestData <- ngsesData[-ngsesTrainRows, -1:-3]
lotrTestData <- lotrData[-lotrTrainRows, -1:-3]
mpbsTestData <- mpbsData[-mpbsTrainRows, -1:-3]

# ---------------------------------------------------------------------------- #
# Analyze training datasets ----
# ---------------------------------------------------------------------------- #

# Computing singular value decomposition on nonmissing training rows without
# implementation of any reverse scoring

svd(na.omit(ngsesTrainData))$d
svd(na.omit(lotrTrainData))$d
svd(na.omit(mpbsTrainData))$d

# Conduct principal components analyses on nonmissing training rows without
# implementation of any reverse scoring
# (from http://www.statmethods.net/advstats/factor.html)

princomp(na.omit(ngsesTrainData), cor = TRUE) # 1
princomp(na.omit(lotrTrainData), cor = TRUE)  # 3
princomp(na.omit(mpbsTrainData), cor = TRUE)  # 2

# Conduct exploratory factor analyses using pairwise deletion on training rows 
# without implementation of any reverse scoring

print(fa.sort(fa(ngsesTrainData, 
                 nfactor = 1)), cut = .3) # Maybe 1?
print(fa.sort(fa(lotrTrainData, 
                 nfactor = 3)), cut = .3) # Remove 5, 9, and maybe 1
print(fa.sort(fa(lotrTrainData[, c(-2, -5, -6, -8)], 
                 nfactor = 1)), cut = .2) # Remove 9 and 10
print(fa.sort(fa(mpbsTrainData[, -8], 
                 nfactor = 2)), cut = .3) # Remove 6?

# Compute Cronbach's alpha using pairwise deletion on training rows with
# reverse-scoring implemented for relevant items

alpha(ngsesTrainData)
alpha(lotrTrainData, key = lotrReversed)
alpha(lotrTrainData[, c(2, 5, 6, 8)], 
      check.keys = TRUE)                  # Remove 6, maybe 5?
alpha(lotrTrainData[, c(3, 7, 9)])        # Remove 9
alpha(lotrTrainData[, c(1, 4, 10)])       # Remove 10
alpha(mpbsTrainData, key = mpbsReversed)
alpha(mpbsTrainData[, c(2, 5, 6, 7)], 
      check.keys = TRUE)                  # Maybe 5, 6, and 7?
alpha(mpbsTrainData[, c(1, 3, 4, 5, 7)], 
      check.keys = TRUE)                  # Maybe (4 and) 5?

# Full-information item factor analysis on nonmissing training rows without
# implementation of any reverse scoring

ngsesTrainIRT <- mirt(ngsesTrainData, 1, verbose = FALSE, 
                      technical = list("removeEmptyRows" = TRUE, "NCYCLES" = 1000))
summary(ngsesTrainIRT)
lotrTrainIRT <- mirt(lotrTrainData, 1, verbose = FALSE,
                     technical = list("removeEmptyRows" = TRUE, "NCYCLES" = 1000))
summary(lotrTrainIRT) # Remove 6, 9, and 10 and maybe 3, 4, and 5
mpbsTrainIRT <- mirt(mpbsTrainData, 1, verbose = FALSE, 
                     technical = list("removeEmptyRows" = TRUE, "NCYCLES" = 1000))
summary(mpbsTrainIRT) # Maybe 6?

# Correlations among self-efficacy items using nonmissing training rows

corr.test(na.omit(ngsesTrainData)) # cor >= .6:     7/8, 2/4; 
                                   # .5 < cor < .6: 1/2, 1/4, 2/3, 2/5, 2/7, 2/8, 
                                   #                3/4, 3/5, 4/5, 5/6, 6/7

# Recommendation: Remove 1, 2/4, 5, 7/8

# ---------------------------------------------------------------------------- #
# Analyze testing datasets (Part 1) ----
# ---------------------------------------------------------------------------- #

# Conduct exploratory factor analyses using pairwise deletion on testing rows
# without implementation of any reverse scoring

print(fa.sort(fa(ngsesTestData, 
                 nfactor = 1)), cut = .3)
print(fa.sort(fa(lotrTestData, 
                 nfactor = 1)), cut = .3) # Remove 5, 9, and maybe 1
print(fa.sort(fa(lotrTestData[, c(-2, -5, -6, -8, -9, -10)],
                 nfactor = 2)), cut = .2)
print(fa.sort(fa(mpbsTestData[, -8],
                 nfactor = 2)), cut = .3) # Remove 6?

# Compute Cronbach's alpha using pairwise deletion on testing rows with
# reverse-scoring implemented for relevant items

alpha(ngsesTestData)
alpha(lotrTestData, key = lotrReversed)
alpha(lotrTestData[, c(2, 5, 6, 8)], 
      check.keys = TRUE)           # Remove 6, maybe 5?
alpha(lotrTestData[, c(3, 7, 9)])  # Remove 9
alpha(lotrTestData[, c(1, 4, 10)]) # Remove 10
alpha(mpbsTestData, key = mpbsReversed)
alpha(mpbsTestData[, c(2, 5, 6, 7)], 
      check.keys = TRUE)           # Maybe 5, 6, and 7?
alpha(mpbsTestData[, c(1, 3, 4, 5, 7)], 
      check.keys = TRUE)           # Maybe (4 and) 5?

# Full-information item factor analysis on nonmissing testing rows without
# implementation of any reverse scoring

ngsesTestIRT <- mirt(ngsesTestData, 1, verbose = FALSE, 
                     technical = list("removeEmptyRows" = TRUE, "NCYCLES" = 1000))
summary(ngsesTestIRT)
lotrTestIRT <- mirt(lotrTestData, 1, verbose = FALSE, 
                    technical = list("removeEmptyRows" = TRUE, "NCYCLES" = 1000))
summary(lotrTestIRT) # Remove 6, 9, 10, maybe 3, 4, and 5
mpbsTestIRT <- mirt(mpbsTestData, 1, verbose = FALSE,
                    technical = list("removeEmptyRows" = TRUE, "NCYCLES" = 1000))
summary(mpbsTestIRT) # Maybe 6?

# ---------------------------------------------------------------------------- #
# Analyze full dataset (Part 2) ----
# ---------------------------------------------------------------------------- #

# Automated item selection procedure on all nonmissing rows without
# implementation of any reverse scoring

aisp(na.omit(ngsesData[, -1:-3][, c(-1, -3, -4, -5, -8)]))
aisp(na.omit(lotrData[, -1:-3][, c(-2, -5, -6, -8, -9, -10)]))
aisp(na.omit(mpbsData[, -1:-3][, c(-1, -2, -4, -5, -6, -8)]))
# aisp(na.omit(ngsesData[, -1:-3][, c(-1, -3, -4, -5, -8)]),
#     search = "ga", verbose = TRUE)
# aisp(na.omit(lotrData[, -1:-3][, c(-2, -5, -6, -8, -9, -10)]),
#     search = "ga", verbose = TRUE)
# aisp(na.omit(mpbsData[, -1:-3][, c(-2, -4, -5, -6, -8)]),
#     search = "ga", verbose = TRUE)

# ---------------------------------------------------------------------------- #
# Analyze testing datasets (Part 2) ----
# ---------------------------------------------------------------------------- #

# Correlations among self-efficacy items using nonmissing testing rows

corr.test(na.omit(ngsesTestData)) # cor > .6:      none; 
                                  # .5 < cor < .6: 1/2, 1/3, 1/4, 2/3, 2/5, 2/7, 
                                  #                2/8, 3/4, 3/5, 4/5, 5/6, 6/7

# Recommendation: Remove 1, 2/4, 5, 7/8

# ---------------------------------------------------------------------------- #
# Analyze full dataset (Part 3) ----
# ---------------------------------------------------------------------------- #

# Conclusions

# Cronbach's alpha using pairwise deletion on all rows with reverse-scoring
# implemented for relevant items

alpha(ngsesData[, -1:-3][, c(-1, -3, -4, -5, -8)])
alpha(lotrData[, -1:-3][, c(-2, -5, -6, -8, -9, -10)], check.keys = TRUE)
alpha(mpbsData[, -1:-3][, c(-2, -4, -5, -6, -8)], check.keys = TRUE)

# NGSES - Remove 1, 3, 4, 5, 8 (Cronbach's alpha of .74 after; .88 before)
#         Keep   2, 6, 7

# LOTR -  Remove 5, 6, 9, 10   (Cronbach's alpha of .63 after; .65 before)
#         Keep   1, 3, 4, 7

# MPBS -  Remove 2, 4, 5, 6, 8 (Cronbach's alpha of .65 after; .76 before)
#         Keep   1, 3, 7