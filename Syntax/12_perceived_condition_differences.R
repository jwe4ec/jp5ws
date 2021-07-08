# ---------------------------------------------------------------------------- #
# Perceived Condition Differences
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Check R version and load packages ----
# ---------------------------------------------------------------------------- #

script_R_version <- "R version 4.1.0 (2021-05-18)"
current_R_version <- R.Version()$version.string

if(current_R_version != script_R_version) {
  warning(paste0("This script is based on ", script_R_version,
                 ". You are running ", current_R_version, "."))
}

library(groundhog)
groundhog_day <- "2021-05-20"

# No packages loaded

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/jp5ws/).

# ---------------------------------------------------------------------------- #
# Load data ----
# ---------------------------------------------------------------------------- #

evalTbl3 <- read.csv("./Data/Temp/evalTbl3.csv")
reasonEndTbl3 <- read.csv("./Data/Temp/reasonEndTbl3.csv")

# ---------------------------------------------------------------------------- #
# Analyze descriptives on separate variables ----
# ---------------------------------------------------------------------------- #

# 32 participants completed Evaluation (13 at Session 4, 19 at follow-up), and
# 21 participants completed Reasons for Ending (4 at Session 1, 4 at Session 2,
# 3 at Session 3, 10 at Session 4). These two participant sets are distinct.

length(unique(evalTbl3$participantId))
sum(table(evalTbl3$perceived_cond))
table(evalTbl3$session)
table(evalTbl3$session, evalTbl3$condition)

length(unique(reasonEndTbl3$participantId))
sum(table(reasonEndTbl3$thoughtInControl))
table(reasonEndTbl3$session)
table(reasonEndTbl3$session, reasonEndTbl3$condition)

intersect(evalTbl3$participantId, reasonEndTbl3$participantId)

# ---------------------------------------------------------------------------- #
# Analyze descriptives on combined variable ----
# ---------------------------------------------------------------------------- #

# 53 participants completed Evaluation or Reasons for Ending (4 at Session 1, 4
# at Session 2, 3 at Session 3, 23 at Session 4, 19 at follow-up).

eval_temp <- 
  evalTbl3[, c("participantId", "condition", "session", "perceived_cond")]
reason_temp <- 
  reasonEndTbl3[, c("participantId", "condition", "session", "thoughtInControl")]

combinedTbl <- merge(eval_temp, reason_temp,
                     by = c("participantId", "condition", "session"),
                     all = TRUE)

combinedTbl$combined <- NA
combinedTbl[!is.na(combinedTbl$perceived_cond) &
              combinedTbl$perceived_cond == 0, "combined"] <- 0
combinedTbl[!is.na(combinedTbl$perceived_cond) &
              combinedTbl$perceived_cond == 1, "combined"] <- 1
combinedTbl[!is.na(combinedTbl$thoughtInControl) &
              combinedTbl$thoughtInControl == "False", "combined"] <- 0
combinedTbl[!is.na(combinedTbl$thoughtInControl) &
              combinedTbl$thoughtInControl == "True", "combined"] <- 1

length(unique(combinedTbl$participantId))
sum(table(combinedTbl$combined))
table(combinedTbl$session)
table(combinedTbl$session, combinedTbl$condition)

# ---------------------------------------------------------------------------- #
# Test condition differences on separate variables ----
# ---------------------------------------------------------------------------- #

# Use Fisher's exact test instead of chi-square given expected values of less
# than 5 in many cells. Using many simulations yields p-values consistent to 
# two decimal places. Note: Analysis may take up to 1 min to run.

perceived_cond_table <- table(evalTbl3$perceived_cond, evalTbl3$condition)

perceived_cond_chi <- chisq.test(perceived_cond_table)
perceived_cond_chi$expected

fisher.test(perceived_cond_table, simulate.p.value = TRUE, B = 10000000)

# Use Fisher's exact test instead of chi-square given expected values of less
# than 5 in many cells. Using many simulations yields p-values consistent to 
# two decimal places. Note: Analysis may take up to 1 min to run.

thoughtInControl_table <- 
  table(reasonEndTbl3$thoughtInControl, reasonEndTbl3$condition)

thoughtInControl_chi <- chisq.test(thoughtInControl_table)
thoughtInControl_chi$expected

fisher.test(thoughtInControl_table, simulate.p.value = TRUE, B = 10000000)

# ---------------------------------------------------------------------------- #
# Test condition differences on combined variable ----
# ---------------------------------------------------------------------------- #

# Use Fisher's exact test instead of chi-square given expected values of less
# than 5 in many cells. Using many simulations yields p-values consistent to 
# two decimal places. Note: Analysis may take up to 1 min to run.

combined_table <- table(combinedTbl$combined, combinedTbl$condition)

combined_chi <- chisq.test(combined_table)
combined_chi$expected

fisher.test(combined_table, simulate.p.value = TRUE, B = 10000000)