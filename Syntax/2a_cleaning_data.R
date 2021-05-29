# ---------------------------------------------------------------------------- #
# Cleaning Data
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

groundhog.library(dplyr, groundhog_day)

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/jp5ws/).

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Scope of data = Data collected through 9/9/2018 for subjects who created an
# account through 3/27/2018.

# Test accounts need to be removed from each table, and each table needs to be
# checked for multiple entries per subject per time point. See the companion
# script "1b_cleaning_data_decisions.R" for decisions made in cleaning multiple 
# entries and the status of implementing those decisions in the present script.

# ---------------------------------------------------------------------------- #
# Import participant table ----
# ---------------------------------------------------------------------------- #

# Import data from T.participant_Mar_27_2018_fixed.csv, obtained from Diheng
# Zhang. Note: This table is sufficient to identify subjects who created accounts 
# through 3/27/18.

participantTbl <- read.csv("./Data/Raw/T.participant_Mar_27_2018_fixed.csv", 
                           header = TRUE)

# View(participantTbl)
# summary(participantTbl)
# str(participantTbl)

# Rearrange variable columns.

participantTbl <- participantTbl[ , c("study_id", "test_account", "admin", 
                                      "last_login_date", "random_token", 
                                      "reference", "theme", "timezone", 
                                      "password_token_id", "campaign", 
                                      "active", "email_reminders", "over18", 
                                      "phone_reminders", "receive_gift_cards", 
                                      "id")]
# View(participantTbl)

# On 6/26/19, Claudia Calicho-Mamani identified additional testing accounts not 
# indicated as such in the test_account variable. For example, these accounts 
# had "test" in the first name, the names of research assistants or staff, etc. 
# We manually reviewed the first and last names of all participants to identify 
# all these cases. 9 additional subjects need to be marked as testing accounts. 
# Only 6 of these are in this participantTbl. Update the values of test_account.

moreTest <- c(949, 1028, 1129, 1200, 1215, 1287, 1361, 1392, 1393)
intersect(participantTbl$study_id, moreTest)
length(intersect(participantTbl$study_id, moreTest))

participantTbl[participantTbl$study_id %in% moreTest, ]$test_account <- TRUE

# Sort by test_account (descending) and study_id.

participantTbl <- participantTbl[order(-participantTbl$test_account, 
                                       participantTbl$study_id), ]
# View(participantTbl)

# Identify test accounts.

# summary(participantTbl$test_account)
participantTblTestAccts <- subset(participantTbl, 
                                  participantTbl$test_account == TRUE)
# participantTblTestAccts$study_id
# length(participantTblTestAccts$study_id)

# Remove 85 test accounts.

participantTbl2 <- subset(participantTbl, participantTbl$test_account == FALSE)
# View(participantTbl2)

# Confirm that no admin accounts remain.

# sum(participantTbl2$admin == TRUE)

# Rename rows.

# str(participantTbl2)
rownames(participantTbl2) <- 1:nrow(participantTbl2)
# View(participantTbl2)

# Check for duplicate entries.

## There are 1221 subjects and no duplicate rows.

length(unique(participantTbl2$study_id))

# Deem these subjects those that created an account through 3/27/18.

IDsEnrolled <- unique(participantTbl2$study_id)

# The last subject ID is 1307. This person is not in expectancyBiasTbl 
# and perhaps not in other tables.

tail(IDsEnrolled)

# ---------------------------------------------------------------------------- #
# Import randomization table ----
# ---------------------------------------------------------------------------- #

# Import data from report_Aug_19_2018.csv, obtained from Dan Funk. Note: This 
# table is sufficient to identify condition for subjects with accounts through 
# 3/27/18.

randomizeTbl <- read.csv("./Data/Raw/report_Aug_19_2018.csv")
# head(randomizeTbl)

# Rename columns.

colnames(randomizeTbl)[grep("..id", colnames(randomizeTbl))] <- "id"

colnames(randomizeTbl)[colnames(randomizeTbl) == 
                         "cast.p.admin.as.UNSIGNED."] <- "admin"

colnames(randomizeTbl)[colnames(randomizeTbl) == 
                         "CAST.p.test_account.as.UNSIGNED."] <- "test_account"

colnames(randomizeTbl)[colnames(randomizeTbl) == 
                         "conditioning"] <- "condition"

# Recode condition as a factor.

# str(randomizeTbl)
# head(randomizeTbl$condition)
randomizeTbl$condition <- as.factor(randomizeTbl$condition)
randomizeTbl$condition <- recode_factor(randomizeTbl$condition, 
                                        "0" = "POSITIVE", 
                                        "1" = "POSITIVE_NEGATION", 
                                        "2" = "FIFTY_FIFTY_RANDOM", 
                                        "3" = "FIFTY_FIFTY_BLOCKED", 
                                        "4" = "NEUTRAL")
# levels(randomizeTbl$condition)

randomizeTbl$condition <- factor(randomizeTbl$condition, levels =
                                   c("POSITIVE",
                                     "POSITIVE_NEGATION",
                                     "FIFTY_FIFTY_BLOCKED",
                                     "FIFTY_FIFTY_RANDOM",
                                     "NEUTRAL"))
# levels(randomizeTbl$condition)

# As stated above for participantTbl, on 6/26/19, Claudia Calicho-Mamani 
# identified additional testing accounts not indicated as such in the 
# test_account variable. For example, these accounts had "test" in the first 
# name, the names of research assistants or staff, etc. We manually reviewed 
# the first and last names of all participants to identify all these cases. 
# 9 additional subjects need to be marked as testing accounts. Only 6 of these 
# are in this randomizeTbl. Update the values of test_account.

intersect(randomizeTbl$id, moreTest)
length(intersect(randomizeTbl$id, moreTest))

randomizeTbl[randomizeTbl$id %in% moreTest, ]$test_account <- 1

# Identify test accounts.

# summary(as.factor(randomizeTbl$test_account))
randomizeTblTestAccts <- subset(randomizeTbl, randomizeTbl$test_account == TRUE)
# randomizeTblTestAccts$id
# length(randomizeTblTestAccts$id)
# length(unique(randomizeTblTestAccts$id))

# Remove 95 test account rows for 91 test accounts (there are some duplicates).

randomizeTbl2 <- subset(randomizeTbl, randomizeTbl$test_account == FALSE)
# View(randomizeTbl2)

# Confirm that no admin accounts remain.

# sum(randomizeTbl2$admin == TRUE)

# The table is already sorted by date. Remove rows beyond 3/27/18 (i.e., IDs
# beyond 1307).

randomizeTbl2 <- randomizeTbl2[randomizeTbl2$id <= 1307, ]
# View(randomizeTbl2)

# Rename rows.

rownames(randomizeTbl2) <- 1:nrow(randomizeTbl2)

# Check for duplicate entries.

# length(randomizeTbl2$id)
# length(unique(randomizeTbl2$id))

# Investigate multiple entries.

# View(randomizeTbl2 %>% group_by(id) %>% summarise(count=n()))
# View(unique(randomizeTbl2) %>% group_by(id) %>% summarise(count=n()))

randomizeTbl2 <- unique(randomizeTbl2)

# After removing duplicate rows, there are 25 subjects with multiple entries.

summary <- randomizeTbl2 %>% group_by(id) %>% summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
# unique(summarySubset$id)

length(unique(summarySubset$id))

randomizeTbl2multipleIDs <- unique(summarySubset$id)

# Use this to search for participants.

# View(randomizeTbl2[which(randomizeTbl2$id == 1307), ])

# See whether people with multiple entries were assigned consistent conditions.
# They were always assigned the same condition.

# View(randomizeTbl2[randomizeTbl2$id %in% summarySubset$id, ])

# Collapse table to only ID and condition, remove duplicate rows, and rename
# rows.

# colnames(randomizeTbl2)
randomizeTbl2$admin <- NULL
randomizeTbl2$test_account <- NULL
randomizeTbl2$date_completed <- NULL

randomizeTbl2 <- unique(randomizeTbl2)
rownames(randomizeTbl2) <- 1:nrow(randomizeTbl2)

# ---------------------------------------------------------------------------- #
# Analyze participant flow (Part 1) ----
# ---------------------------------------------------------------------------- #

# Note: Information used in the participant flowchart is marked *USED IN FLOWCHART*.

# 1221 subjects were randomized. These subjects are the same as the 1221 
# subjects in participantTbl2.

length(randomizeTbl2$id) # *USED IN FLOWCHART*

# setdiff(randomizeTbl2$id, participantTbl2$study_id)
# setdiff(participantTbl2$study_id, randomizeTbl2$id)

# Compute number randomized per condition.

table(randomizeTbl2$condition) # *USED IN FLOWCHART*

# ---------------------------------------------------------------------------- #
# Import tasklog table ----
# ---------------------------------------------------------------------------- #

# Import data from TaskLog_recovered_corrected_Sep_10_2018, obtained from Claudia
# Calicho-Mamani.

taskLogTbl <- read.csv("./Data/Raw/TaskLog_recovered_corrected_Sep_10_2018.csv", 
                       header = TRUE)

# View(taskLogTbl)
# summary(taskLogTbl)
# str(taskLogTbl)

# Identify and remove 64 test accounts.

# sort(unique(subset(taskLogTbl, 
#                    taskLogTbl$participantId %in% 
#                      participantTblTestAccts$study_id)$participantId))
# length(unique(subset(taskLogTbl, 
#                      taskLogTbl$participantId %in% 
#                        participantTblTestAccts$study_id)$participantId))
taskLogTbl2 <- subset(taskLogTbl, !(taskLogTbl$participantId %in% 
                                      participantTblTestAccts$study_id))
# length(unique(taskLogTbl2$participantId))
# View(taskLogTbl2)

# Remove subjects who enrolled after 3/27/18, leaving 1221 subjects.

taskLogTbl2 <- taskLogTbl2[taskLogTbl2$participantId %in% IDsEnrolled, ]

# Rearrange variable columns.

taskLogTbl2 <- taskLogTbl2[ , c("participantId", "sessionName", "tag", 
                                "taskName", "timeOnTask", "dateTime", 
                                "TimeDate", "study", "id")]
# View(taskLogTbl2)

# Reorder the levels of session and make it an ordered factor.

# str(taskLogTbl2$sessionName)
# levels(taskLogTbl2$sessionName) # Nobody has COMPLETE, so remove it.
# table(taskLogTbl2$sessionName)

taskLogTbl2$sessionName <- ordered(taskLogTbl2$sessionName, 
                                   levels = c("Eligibility", "preTest", 
                                              "firstSession", "secondSession", 
                                              "thirdSession", "fourthSession", 
                                              "PostFollowUp"))

# head(taskLogTbl2$sessionName)
# str(taskLogTbl2$sessionName)
# levels(taskLogTbl2$sessionName)
# table(taskLogTbl2$sessionName)

# Sort by participantId, sessionName, and id.

taskLogTbl2 <- taskLogTbl2[order(taskLogTbl2$participantId, 
                                 taskLogTbl2$sessionName, 
                                 taskLogTbl2$id), ]

# Rename rows.

# str(taskLogTbl2)
rownames(taskLogTbl2) <- 1:nrow(taskLogTbl2)
# View(taskLogTbl2)

# Add condition.

# length(unique(taskLogTbl2$participantId))
# length(unique(randomizeTbl2$id))
# setequal(unique(taskLogTbl2$participantId), unique(randomizeTbl2$id))

tempRandomize <- randomizeTbl2
tempRandomize$participantId <- tempRandomize$id
tempRandomize$id <- NULL
tempRandomize <- tempRandomize[ , c(2, 1)]

# View(taskLogTbl2)
# View(tempRandomize)

taskLogTbl2 <- merge(taskLogTbl2, tempRandomize, by = "participantId", 
                     all.x = TRUE)

taskLogTbl2 <- taskLogTbl2[order(taskLogTbl2$participantId, 
                                 taskLogTbl2$sessionName, 
                                 taskLogTbl2$id), ]

# View(taskLogTbl2)

# There are 1221 subjects. They are the same as those in participantTbl2.

# length(unique(taskLogTbl2$participantId))
# setequal(unique(taskLogTbl2$participantId), unique(participantTbl2$study_id))

# Check number of entries for each task by session. NOTE: Multiple entries will
# not be removed from the present table; rather, when using it to identify the
# number of subjects who completed various tasks, be sure not to count the same
# participantID more than once (i.e., count only unique IDs).

# There are no simply duplicated entries.

# nrow(taskLogTbl2) == nrow(unique(taskLogTbl2))

# View(unique(taskLogTbl2) %>% 
#        group_by(participantId, sessionName, taskName, tag) %>% 
#        summarise(count=n()))

# There are 99 subjects with multiple entries.

summary <- unique(taskLogTbl2) %>%
  group_by(participantId, sessionName, taskName, tag) %>% 
  summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
unique(summarySubset$participantId)

length(unique(summarySubset$participantId))

taskLogTbl2multipleIDs <- unique(summarySubset$participantId)

# View(summary[summary$participantId %in%
#                unique(summarySubset$participantId), ])

# View(taskLogTbl2[taskLogTbl2$participantId %in% 
#                    unique(summarySubset$participantId), ])

# Use this to search for participants.

# View(taskLogTbl2[which(taskLogTbl2$participantId == 137), ])

#------------------------------------------------------------------------------#
# Analyze participant flow (Part 2) ----
#------------------------------------------------------------------------------#

# Determine number of subjects at each session. In the taskLog table, starting
# treatment is defined as having an entry for pre-affect at the first session. 
# Per Dan Funk, such an entry appears only when the subject presses submit for 
# the pre-affect items; the very next thing that happens is the subject sees the 
# introduction screen for the training scenarios.

# Recall that taskLog table has multiple entries. When using it to identify the
# number of subjects who completed various tasks, be sure not to count the same
# participantID more than once (i.e., count only unique IDs).

# View(taskLogTbl2)

# Pretreatment

## 1158 started

length(taskLogTbl2[taskLogTbl2$sessionName == "preTest" & 
                      taskLogTbl2$taskName == "Credibility", ]$participantId)

IDsStartedPTX <- unique(taskLogTbl2[taskLogTbl2$sessionName == "preTest" & 
                                       taskLogTbl2$taskName == "Credibility", 
                                     ]$participantId)

length(IDsStartedPTX)

## By condition

taskLogTbl2Brief <- taskLogTbl2[taskLogTbl2$sessionName == "preTest" & 
                                    taskLogTbl2$taskName == "Credibility", 
                                  c("participantId", "sessionName", "taskName", 
                                    "condition")]
taskLogTbl2Brief <- unique(taskLogTbl2Brief)
table(taskLogTbl2Brief$condition)

# Randomized (see Analyze Subject Flow [Part 1] above) - Started S1 (see below)
# = Didn't start S1 *USED IN FLOWCHART*S

222 - 177 # POSITIVE
188 - 149 # POSITIVE_NEGATION
194 - 151 # FIFTY_FIFTY_BLOCKED
217 - 176 # FIFTY_FIFTY_RANDOM
400 - 318 # NEUTRAL

# Randomized (see Analyze Subject Flow [Part 1] above) - Started PTX (see above) 
# = Drop by PTX *USED IN FLOWCHART*

222 - 206 # POSITIVE
188 - 181 # POSITIVE_NEGATION
194 - 187 # FIFTY_FIFTY_BLOCKED
217 - 206 # FIFTY_FIFTY_RANDOM
400 - 378 # NEUTRAL

# Didn't start S1 (see above) - Drop by PTX (see above)
# = Drop by S1 *USED IN FLOWCHART*

45 - 16   # POSITIVE
39 - 7    # POSITIVE_NEGATION
43 - 7    # FIFTY_FIFTY_BLOCKED
41 - 11   # FIFTY_FIFTY_RANDOM
82 - 22   # NEUTRAL

## We do not presently investigate multiple taskLog entries for pretreatment.

# Session 1

## 971 started

length(taskLogTbl2[taskLogTbl2$sessionName == "firstSession" & 
                            taskLogTbl2$taskName == "Affect" &
                            taskLogTbl2$tag == "pre", ]$participantId)

IDsStartedS1 <- unique(taskLogTbl2[taskLogTbl2$sessionName == "firstSession" & 
                                     taskLogTbl2$taskName == "Affect" &
                                     taskLogTbl2$tag == "pre", ]$participantId)

length(IDsStartedS1)

## By condition

taskLogTbl2Brief <- taskLogTbl2[taskLogTbl2$sessionName == "firstSession" & 
                                    taskLogTbl2$taskName == "Affect" &
                                    taskLogTbl2$tag == "pre", 
                                  c("participantId", "sessionName", "tag", 
                                    "taskName", "condition")]
taskLogTbl2Brief <- unique(taskLogTbl2Brief)
table(taskLogTbl2Brief$condition)

## 850 finished

length(taskLogTbl2[taskLogTbl2$sessionName == "firstSession" & 
                   taskLogTbl2$taskName == "JsPsychTrial", 
                   ]$participantId)

IDsFinishedS1 <- 
  unique(taskLogTbl2[taskLogTbl2$sessionName == "firstSession" & 
                     taskLogTbl2$taskName == "JsPsychTrial", 
                     ]$participantId)

length(IDsFinishedS1)

setdiff(IDsFinishedS1, IDsStartedS1)

## Investigate multiple entries for session 1.

# View(taskLogTbl2[taskLogTbl2$sessionName == "firstSession" &
#                  taskLogTbl2$taskName == "Affect" &
#                  taskLogTbl2$tag == "pre", ] %>%
#      group_by(sessionName, taskName, tag, participantId) %>%
#      summarise(count=n()))
# 
# View(taskLogTbl2[taskLogTbl2$sessionName == "firstSession" & 
#                  taskLogTbl2$taskName == "JsPsychTrial", ] %>%
#        group_by(sessionName, taskName, tag, participantId) %>%
#        summarise(count=n()))

### 29 people attempted session 1 more than once. 6 people completed it twice.

# View(taskLogTbl2[taskLogTbl2$participantId == 1160 & 
#                    taskLogTbl2$sessionName == "firstSession" &
#                    taskLogTbl2$taskName == "Affect" &
#                    taskLogTbl2$tag == "pre", ])

summary <- taskLogTbl2[taskLogTbl2$sessionName == "firstSession" &
                         taskLogTbl2$taskName == "Affect" &
                         taskLogTbl2$tag == "pre", ] %>%
           group_by(sessionName, taskName, tag, participantId) %>%
           summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
IDsReattemptS1 <- summarySubset$participantId
length(IDsReattemptS1)

summary <-
  taskLogTbl2[taskLogTbl2$sessionName == "firstSession" & 
              taskLogTbl2$taskName == "JsPsychTrial", ] %>%
    group_by(sessionName, taskName, participantId) %>%
    summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
IDsRecompleteS1 <- summarySubset$participantId
length(IDsRecompleteS1)

setdiff(IDsRecompleteS1, IDsReattemptS1)

# Session 2

## 535 started

length(taskLogTbl2[taskLogTbl2$sessionName == "secondSession" & 
                     taskLogTbl2$taskName == "Affect" &
                     taskLogTbl2$tag == "pre", ]$participantId)

IDsStartedS2 <- unique(taskLogTbl2[taskLogTbl2$sessionName == "secondSession" & 
                                     taskLogTbl2$taskName == "Affect" &
                                     taskLogTbl2$tag == "pre", ]$participantId)

length(IDsStartedS2)

## By condition

taskLogTbl2Brief <- taskLogTbl2[taskLogTbl2$sessionName == "secondSession" & 
                                    taskLogTbl2$taskName == "Affect" &
                                    taskLogTbl2$tag == "pre", 
                                  c("participantId", "sessionName", "taskName", 
                                    "condition")]
taskLogTbl2Brief <- unique(taskLogTbl2Brief)
table(taskLogTbl2Brief$condition)

# Started S1 (see above) - Started S2 (see above)
# = Drop by S2 *USED IN FLOWCHART*

177 - 93  # POSITIVE
149 - 80  # POSITIVE_NEGATION
151 - 76  # FIFTY_FIFTY_BLOCKED
176 - 98  # FIFTY_FIFTY_RANDOM
318 - 188 # NEUTRAL

## 485 finished

length(taskLogTbl2[taskLogTbl2$sessionName == "secondSession" & 
                     taskLogTbl2$taskName == "JsPsychTrial", 
                   ]$participantId)

IDsFinishedS2 <- 
  unique(taskLogTbl2[taskLogTbl2$sessionName == "secondSession" & 
                       taskLogTbl2$taskName == "JsPsychTrial", 
                     ]$participantId)

length(IDsFinishedS2)

setdiff(IDsFinishedS2, IDsStartedS2)

# Everyone who started session 2 started session 1. 436 subjects who started
# S1 did not start S2.

setdiff(IDsStartedS2, IDsStartedS1)
length(setdiff(IDsStartedS1, IDsStartedS2))

## Investigate multiple entries for session 2.

# View(taskLogTbl2[taskLogTbl2$sessionName == "secondSession" &
#                    taskLogTbl2$taskName == "Affect" &
#                    taskLogTbl2$tag == "pre", ] %>%
#        group_by(sessionName, taskName, tag, participantId) %>%
#        summarise(count=n()))
# 
# View(taskLogTbl2[taskLogTbl2$sessionName == "secondSession" & 
#                  taskLogTbl2$taskName == "JsPsychTrial", ] %>%
#        group_by(sessionName, taskName, tag, participantId) %>%
#        summarise(count=n()))

### 11 people attempted session 2 twice. 3 people completed it twice.

summary <- taskLogTbl2[taskLogTbl2$sessionName == "secondSession" &
                         taskLogTbl2$taskName == "Affect" &
                         taskLogTbl2$tag == "pre", ] %>%
  group_by(sessionName, taskName, tag, participantId) %>%
  summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
IDsReattemptS2 <- summarySubset$participantId
length(IDsReattemptS2)

summary <-
  taskLogTbl2[taskLogTbl2$sessionName == "secondSession" & 
                taskLogTbl2$taskName == "JsPsychTrial", ] %>%
  group_by(sessionName, taskName, participantId) %>%
  summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
IDsRecompleteS2 <- summarySubset$participantId
length(IDsRecompleteS2)

setdiff(IDsRecompleteS2, IDsReattemptS2)

# See if people who reattempted S2 are among those who reattempted S1. 1025 is 
# the same person; the other 10 are different.

length(setdiff(IDsReattemptS2, IDsReattemptS1))

# Session 3

## 392 started

length(taskLogTbl2[taskLogTbl2$sessionName == "thirdSession" & 
                     taskLogTbl2$taskName == "Affect" &
                     taskLogTbl2$tag == "pre", ]$participantId)

IDsStartedS3 <- unique(taskLogTbl2[taskLogTbl2$sessionName == "thirdSession" & 
                                     taskLogTbl2$taskName == "Affect" &
                                     taskLogTbl2$tag == "pre", ]$participantId)

length(IDsStartedS3)

## By condition

taskLogTbl2Brief <- taskLogTbl2[taskLogTbl2$sessionName == "thirdSession" & 
                                    taskLogTbl2$taskName == "Affect" &
                                    taskLogTbl2$tag == "pre", 
                                  c("participantId", "sessionName", "taskName", 
                                    "condition")]
taskLogTbl2Brief <- unique(taskLogTbl2Brief)
table(taskLogTbl2Brief$condition)

# Started S2 (see above) - Started S3 (see above)
# = Drop by S3 *USED IN FLOWCHART*

93 - 62   # POSITIVE
80 - 52   # POSITIVE_NEGATION
76 - 58   # FIFTY_FIFTY_BLOCKED
98 - 73   # FIFTY_FIFTY_RANDOM
188 - 147 # NEUTRAL

## 366 finished

length(taskLogTbl2[taskLogTbl2$sessionName == "thirdSession" & 
                   taskLogTbl2$taskName == "JsPsychTrial", 
                   ]$participantId)

IDsFinishedS3 <- 
  unique(taskLogTbl2[taskLogTbl2$sessionName == "thirdSession" & 
                     taskLogTbl2$taskName == "JsPsychTrial", 
                     ]$participantId)

length(IDsFinishedS3)

setdiff(IDsFinishedS3, IDsStartedS3)

# Everyone who started session 3 started sessions 1 and 2. 143 subjects who 
# started S2 did not start S3.

setdiff(IDsStartedS3, IDsStartedS1)
setdiff(IDsStartedS3, IDsStartedS2)
length(setdiff(IDsStartedS2, IDsStartedS3))

## Investigate multiple entries for session 3.

# View(taskLogTbl2[taskLogTbl2$sessionName == "thirdSession" &
#                    taskLogTbl2$taskName == "Affect" &
#                    taskLogTbl2$tag == "pre", ] %>%
#        group_by(sessionName, taskName, tag, participantId) %>%
#        summarise(count=n()))
# 
# View(taskLogTbl2[taskLogTbl2$sessionName == "thirdSession" & 
#                    taskLogTbl2$taskName == "JsPsychTrial", ] %>%
#        group_by(sessionName, taskName, tag, participantId) %>%
#        summarise(count=n()))

# 5 people attempted third session twice. 2 people completed it more than once.

summary <- taskLogTbl2[taskLogTbl2$sessionName == "thirdSession" &
                         taskLogTbl2$taskName == "Affect" &
                         taskLogTbl2$tag == "pre", ] %>%
  group_by(sessionName, taskName, tag, participantId) %>%
  summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
IDsReattemptS3 <- summarySubset$participantId
length(IDsReattemptS3)

summary <-
  taskLogTbl2[taskLogTbl2$sessionName == "thirdSession" & 
                taskLogTbl2$taskName == "JsPsychTrial", ] %>%
  group_by(sessionName, taskName, participantId) %>%
  summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
IDsRecompleteS3 <- summarySubset$participantId
length(IDsRecompleteS3)

setdiff(IDsRecompleteS3, IDsReattemptS3)

# See if people who reattempted S3 are among those who reattempted S2 or S1.
# All 5 are different from those who reattempted S2. 916 also reattmepted S1;
# the other 4 are different.

length(setdiff(IDsReattemptS3, IDsReattemptS2))
length(setdiff(IDsReattemptS3, IDsReattemptS1))

# Session 4

## 305 started

length(taskLogTbl2[taskLogTbl2$sessionName == "fourthSession" & 
                     taskLogTbl2$taskName == "Affect" &
                     taskLogTbl2$tag == "pre", ]$participantId)

IDsStartedS4 <- unique(taskLogTbl2[taskLogTbl2$sessionName == "fourthSession" & 
                                     taskLogTbl2$taskName == "Affect" &
                                     taskLogTbl2$tag == "pre", ]$participantId)

length(IDsStartedS4)

## By condition

taskLogTbl2Brief <- taskLogTbl2[taskLogTbl2$sessionName == "fourthSession" & 
                                    taskLogTbl2$taskName == "Affect" &
                                    taskLogTbl2$tag == "pre", 
                                  c("participantId", "sessionName", "taskName", 
                                    "condition")]
taskLogTbl2Brief <- unique(taskLogTbl2Brief)
table(taskLogTbl2Brief$condition)

# Started S3 (see above) - Started S4 (see above)
# = Drop by S4 *USED IN FLOWCHART*

62 - 48   # POSITIVE
52 - 38   # POSITIVE_NEGATION
58 - 45   # FIFTY_FIFTY_BLOCKED
73 - 54   # FIFTY_FIFTY_RANDOM
147 - 120 # NEUTRAL

## 293 finished

length(taskLogTbl2[taskLogTbl2$sessionName == "fourthSession" & 
                   taskLogTbl2$taskName == "JsPsychTrial", 
                   ]$participantId)

IDsFinishedS4 <- 
  unique(taskLogTbl2[taskLogTbl2$sessionName == "fourthSession" & 
                     taskLogTbl2$taskName == "JsPsychTrial", 
                     ]$participantId)

length(IDsFinishedS4)

setdiff(IDsFinishedS4, IDsStartedS4)

# Everyone who started session 4 started sessions 1, 2, and 3. 87 subjects who 
# started S3 did not start S4.

setdiff(IDsStartedS4, IDsStartedS1)
setdiff(IDsStartedS4, IDsStartedS2)
setdiff(IDsStartedS4, IDsStartedS3)
length(setdiff(IDsStartedS3, IDsStartedS4))

## Investigate multiple entries for session 4.

# View(taskLogTbl2[taskLogTbl2$sessionName == "fourthSession" &
#                    taskLogTbl2$taskName == "Affect" &
#                    taskLogTbl2$tag == "pre", ] %>%
#        group_by(sessionName, taskName, tag, participantId) %>%
#        summarise(count=n()))
# 
# View(taskLogTbl2[taskLogTbl2$sessionName == "fourthSession" & 
#                    taskLogTbl2$taskName == "JsPsychTrial", ] %>%
#        group_by(sessionName, taskName, tag, participantId) %>%
#        summarise(count=n()))

# View(taskLogTbl2[taskLogTbl2$participantId == 782, ])
# View(taskLogTbl2[taskLogTbl2$participantId == 1162, ])

### 6 people attempted session 4 more than once. 4 people completed it twice.

summary <- taskLogTbl2[taskLogTbl2$sessionName == "fourthSession" &
                         taskLogTbl2$taskName == "Affect" &
                         taskLogTbl2$tag == "pre", ] %>%
  group_by(sessionName, taskName, tag, participantId) %>%
  summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
IDsReattemptS4 <- summarySubset$participantId
length(IDsReattemptS4)

summary <-
  taskLogTbl2[taskLogTbl2$sessionName == "fourthSession" & 
              taskLogTbl2$taskName == "JsPsychTrial", ] %>%
    group_by(sessionName, taskName, participantId) %>%
    summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
IDsRecompleteS4 <- summarySubset$participantId
length(IDsRecompleteS4)

setdiff(IDsRecompleteS4, IDsReattemptS4)

# See if people who reattempted S4 are among those who reattempted S3, S2, or
# S1. All 6 are different.

length(setdiff(IDsReattemptS4, IDsReattemptS3))
length(setdiff(IDsReattemptS4, IDsReattemptS2))
length(setdiff(IDsReattemptS4, IDsReattemptS1))

# Determine number who dropped treatment (Drop by S2 + Drop by S3 + Drop by S4)

436 + 143 + 87

# Determine number who reattempted and number who recompleted any treatment
# session.

union(union(union(IDsReattemptS1, 
                  IDsReattemptS2), 
            IDsReattemptS3), 
      IDsReattemptS4) %>% length()

union(union(union(IDsRecompleteS1, 
                  IDsRecompleteS2), 
            IDsRecompleteS3), 
      IDsRecompleteS4) %>% length()

# Follow-up

## 150 started

length(taskLogTbl2[taskLogTbl2$sessionName == 
                      "PostFollowUp" & 
                    taskLogTbl2$taskName == 
                      "ExpectancyBias", ]$participantId)

IDsStartedFU <- unique(taskLogTbl2[taskLogTbl2$sessionName == 
                                      "PostFollowUp" & 
                                    taskLogTbl2$taskName == 
                                      "ExpectancyBias", ]$participantId)

length(IDsStartedFU)

## By condition

taskLogTbl2Brief <- taskLogTbl2[taskLogTbl2$sessionName == "PostFollowUp" & 
                                    taskLogTbl2$taskName == "ExpectancyBias", 
                                  c("participantId", "sessionName", "taskName", 
                                    "condition")]
taskLogTbl2Brief <- unique(taskLogTbl2Brief)
table(taskLogTbl2Brief$condition)

# Started S4 (see above) - Started FU (see above)
# = Drop by FU *USED IN FLOWCHART*

48 - 23   # POSITIVE
38 - 13   # POSITIVE_NEGATION
45 - 25   # FIFTY_FIFTY_BLOCKED
54 - 29   # FIFTY_FIFTY_RANDOM
120 - 60  # NEUTRAL

# Everyone who started follow-up started sessions 1-4 and completed training
# at S4. 155 subjects who started S4 (and 143 subjects who completed S4, see 
# Analyze Subject Flow [Part 3] below) did not start FU.

setdiff(IDsStartedFU, IDsStartedS1)
setdiff(IDsStartedFU, IDsStartedS2)
setdiff(IDsStartedFU, IDsStartedS3)
setdiff(IDsStartedFU, IDsStartedS4)
length(setdiff(IDsStartedS4, IDsStartedFU))

setdiff(IDsStartedFU, taskLogTbl2completerIDs)
# length(setdiff(taskLogTbl2completerIDs, IDsStartedFU))

# We do not presently investigate multiple taskLog entries for follow-up.

#------------------------------------------------------------------------------#
# Import credibility table ----
#------------------------------------------------------------------------------#

# Import data from Credibility_recovered_Sep_10_2018.csv, obtained from Claudia
# Calicho-Mamani.

credibilityTbl <- read.csv("./Data/Raw/Credibility_recovered_Sep_10_2018.csv", 
                           header = TRUE)

# View(credibilityTbl)
# summary(credibilityTbl)
# str(credibilityTbl)

# Remove 32 test accounts, leaving 1208 subjects.

length(unique(credibilityTbl$participantRSA))

credibilityTbl2 <- subset(credibilityTbl, !(credibilityTbl$participantRSA %in% 
                                participantTblTestAccts$study_id))

# sort(unique(subset(credibilityTbl,
#                    (credibilityTbl$participantRSA %in%
#                       participantTblTestAccts$study_id))$participantRSA))
# length(unique(subset(credibilityTbl,
#                      (credibilityTbl$participantRSA %in%
#                         participantTblTestAccts$study_id))$participantRSA))

# length(unique(credibilityTbl2$participantRSA))
# View(credibilityTbl2)

# Remove subjects who enrolled after 3/27/18, leaving 1158 subjects.

credibilityTbl2 <- 
  credibilityTbl2[credibilityTbl2$participantRSA %in% IDsEnrolled, ]

# Rearrange variable columns.

credibilityTbl2 <- credibilityTbl2[ , c("participantRSA", "session", 
                                        "confident_design", "confident_online", 
                                        "important", "id", "date", "timeOnPage", 
                                        "tag")]

# Make session an ordered factor.

# head(credibilityTbl2$session)
# str(credibilityTbl2$session)
# levels(credibilityTbl2$session)
# table(credibilityTbl2$session)

credibilityTbl2$session <- 
  ordered(credibilityTbl2$session, levels = c("preTest"))

# Sort by participantRSA and id.

credibilityTbl2 <- credibilityTbl2[order(credibilityTbl2$participantRSA, 
                                         credibilityTbl2$id), ]

# View(credibilityTbl2)

# Rename rows.

# str(credibilityTbl2)
rownames(credibilityTbl2) <- 1:nrow(credibilityTbl2)
# View(credibilityTbl2)

# There are 1158 subjects, all of which are part of the 1221 in participantTbl2.

length(unique(credibilityTbl2$participantRSA))

sum(!(unique(credibilityTbl2$participantRSA) %in% unique(participantTbl2$study_id)))

# Check number of entries per session.

# View(credibilityTbl2 %>%
#        group_by(session, participantRSA) %>% summarise(count=n()))
# View(unique(credibilityTbl2) %>%
#        group_by(session, participantRSA) %>% summarise(count=n()))

# There are 37 subjects with multiple entries.

summary <- unique(credibilityTbl2) %>% 
  group_by(participantRSA, session) %>% summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
unique(summarySubset$participantRSA)

length(unique(summarySubset$participantRSA))

credibilityTbl2multipleIDs <- unique(summarySubset$participantRSA)

#------------------------------------------------------------------------------#
# Resolve multiple entries in credibility table ----
#------------------------------------------------------------------------------#

# Implement decisions documented in separate cleaning script.

# Implement DECISION 1.

# View(credibilityTbl2[credibilityTbl2$participantRSA %in%
#                                   c(200, 392, 412, 495, 496, 577, 582, 627,
#                                     634, 961, 453, 788, 138, 942), ])

credibilityTbl3 <- 
  credibilityTbl2[!(credibilityTbl2$participantRSA %in% 
               c(200, 392, 412, 495, 496, 577, 582, 627, 
                 634, 961, 453, 788, 138, 942)), ]

# Implement DECISION 4.

# View(credibilityTbl3[(credibilityTbl3$participantRSA %in%
#                          c(352, 971, 834, 862, 868)), ])

## Remove rows for ids (note: not participant IDs) identified manually.

credibilityTbl3 <- 
  credibilityTbl3[!(credibilityTbl3$id %in% 
                      c(318, 768, 771, 772, 789, 1035, 1012, 1021)), ]

# Implement DECISION 5.

# View(credibilityTbl3[(credibilityTbl3$participantRSA %in%
#                         c(734)), ])

## Remove rows for ids (note: not participant IDs) identified manually.

credibilityTbl3 <- 
  credibilityTbl3[!(credibilityTbl3$id %in% 
                      c(676)), ]

# Implement DECISION 6.

# View(credibilityTbl3[(credibilityTbl3$participantRSA %in%
#                         c(969, 469, 99, 186, 194, 831)), ])

## Remove rows for ids (note: not participant IDs) identified manually.

credibilityTbl3 <- 
  credibilityTbl3[!(credibilityTbl3$id %in% 
                      c(426, 913, 86, 170, 179, 871)), ]

# Implement DECISION 7.

# View(credibilityTbl3[(credibilityTbl3$participantRSA %in%
#                         c(883, 584, 91, 1112, 1130, 261, 223, 814, 1031)), ])

## Remove rows for ids (note: not participant IDs) identified manually.

credibilityTbl3 <- 
  credibilityTbl3[!(credibilityTbl3$id %in% 
                      c(76, 886, 243, 533, 902, 818, 970, 972, 1075, 1094)), ]

# Implement DECISION 8.

# View(credibilityTbl3[(credibilityTbl3$participantRSA %in%
#                         c(130, 137, 760, 950, 711, 533, 947, 895)), ])

## Remove rows for ids (note: not participant IDs) identified manually.

credibilityTbl3 <- 
  credibilityTbl3[!(credibilityTbl3$id %in% 
                      c(116, 127, 784, 652, 704, 827, 1064, 885)), ]

# Implement DECISION 9.

# View(credibilityTbl3[(credibilityTbl3$participantRSA %in%
#                         c(916)), ])

## Remove rows for ids (note: not participant IDs) identified manually.

credibilityTbl3 <- 
  credibilityTbl3[!(credibilityTbl3$id %in% 
                      c(1041)), ]

# Implement DECISION 10.

# View(credibilityTbl3[(credibilityTbl3$participantRSA %in%
#                         c(943, 565, 1066, 49, 1160)), ])

## Remove rows for ids (note: not participant IDs) identified manually.

credibilityTbl3 <- 
  credibilityTbl3[!(credibilityTbl3$id %in% 
                      c(1063, 1128, 906, 1088, 1125)), ]

# Confirm that there are no longer multiple entries.

summary <- unique(credibilityTbl3) %>% 
  group_by(participantRSA, session) %>% summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
unique(summarySubset$participantRSA)

length(unique(summarySubset$participantRSA))

#------------------------------------------------------------------------------#
# Import ph4 table ----
#------------------------------------------------------------------------------#

# Import data from Phq4_recovered_Sep_10_2018.csv, obtained from Claudia
# Calicho-Mamani.

phq4Tbl <- read.csv("./Data/Raw/Phq4_recovered_Sep_10_2018.csv", header = TRUE)

# View(phq4Tbl)
# summary(phq4Tbl)
# str(phq4Tbl)

# Remove 46 test accounts, leaving 1185 subjects.

# length(unique(phq4Tbl$participantRSA))

phq4Tbl2 <- subset(phq4Tbl, !(phq4Tbl$participantRSA %in% 
                                participantTblTestAccts$study_id))

# sort(unique(subset(phq4Tbl, 
#                    (phq4Tbl$participantRSA %in% 
#                       participantTblTestAccts$study_id))$participantRSA))
# length(unique(subset(phq4Tbl, 
#                      (phq4Tbl$participantRSA %in% 
#                         participantTblTestAccts$study_id))$participantRSA))

# length(unique(phq4Tbl2$participantRSA))
# View(phq4Tbl2)

# Remove subjects who enrolled after 3/27/18, leaving 1136 subjects.

phq4Tbl2 <- phq4Tbl2[phq4Tbl2$participantRSA %in% IDsEnrolled, ]

# Rearrange variable columns.

phq4Tbl2 <- phq4Tbl2[ , c("participantRSA", "session", "nervous", "worry", 
                          "pleasure", "depressed", "id", "date", "timeOnPage", 
                          "tag")]

# Reorder the levels of session and make it an ordered factor.

# head(phq4Tbl2$session)
# str(phq4Tbl2$session)
# levels(phq4Tbl2$session)
# table(phq4Tbl2$session)

phq4Tbl2$session <- ordered(phq4Tbl2$session, levels = c("preTest", 
                                                         "secondSession", 
                                                         "fourthSession", 
                                                         "PostFollowUp"))

# Sort by participantRSA, session, and id.

phq4Tbl2 <- phq4Tbl2[order(phq4Tbl2$participantRSA, 
                           phq4Tbl2$session,
                           phq4Tbl2$id), ]

# View(phq4Tbl2)

# Rename rows.

# str(phq4Tbl2)
rownames(phq4Tbl2) <- 1:nrow(phq4Tbl2)
# View(phq4Tbl2)

# There are 1136 subjects, all of which are part of the 1221 in participantTbl2.

length(unique(phq4Tbl2$participantRSA))

sum(!(unique(phq4Tbl2$participantRSA) %in% unique(participantTbl2$study_id)))

# Check number of entries per session.

# View(phq4Tbl2 %>% 
#        group_by(session, participantRSA) %>% summarise(count=n()))
# View(unique(phq4Tbl2) %>% 
#        group_by(session, participantRSA) %>% summarise(count=n()))

# There are 30 subjects with multiple entries.

summary <- unique(phq4Tbl2) %>% 
  group_by(participantRSA, session) %>% summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
unique(summarySubset$participantRSA)

length(unique(summarySubset$participantRSA))

phq4Tbl2multipleIDs <- unique(summarySubset$participantRSA)

#------------------------------------------------------------------------------#
# Resolve multiple entries in ph4 table ----
#------------------------------------------------------------------------------#

# Implement decisions documented in separate cleaning script.

# Implement DECISION 1.

# View(phq4Tbl2[phq4Tbl2$participantRSA %in% 
#                                   c(200, 392, 412, 495, 496, 577, 582, 627, 
#                                     634, 961, 453, 788, 138, 942), ])

phq4Tbl3 <- 
  phq4Tbl2[!(phq4Tbl2$participantRSA %in% 
                                 c(200, 392, 412, 495, 496, 577, 582, 627, 
                                   634, 961, 453, 788, 138, 942)), ]

# Implement DECISION 7.

# View(phq4Tbl3[(phq4Tbl3$participantRSA %in% 
#                 c(883, 584, 91, 1112, 1130, 261, 223, 814, 1031)) &
#               (phq4Tbl3$session == "preTest"), 
#               c(1, 7, 2, 8)])

## Remove rows for ids (note: not participant IDs) identified manually.

phq4Tbl3 <- 
  phq4Tbl3[!(phq4Tbl3$id %in% 
               c(81, 997, 238, 523, 1023, 852, 1149, 1151, 1552, 1709)), ]

# Implement DECISION 8.

# View(phq4Tbl3[(phq4Tbl3$participantRSA %in% 
#                  c(130, 137, 760, 950, 711, 533, 947, 895)) &
#                 (phq4Tbl3$session == "preTest"), 
#               c(1, 7, 2, 8)])

## Remove rows for ids (note: not participant IDs) identified manually.

phq4Tbl3 <- 
  phq4Tbl3[!(phq4Tbl3$id %in% 
               c(120, 129, 779, 638, 688, 881, 1504, 993)), ]

## Implement DECISION 9.

# View(phq4Tbl3[phq4Tbl3$participantRSA == 916, 
#               c(1, 7, 2, 8)])

## Remove row for id (note: not participant IDs) identified manually.

phq4Tbl3 <- 
  phq4Tbl3[!(phq4Tbl3$id %in% 
               c(1378)), ]

## Implement DECISION 10.

# View(phq4Tbl3[(phq4Tbl3$participantRSA %in% 
#                  c(943, 565, 1066, 49, 1160)) &
#               (phq4Tbl3$session == "preTest"), 
#               c(1, 7, 2, 8)])

# Remove row for ids (note: not participant IDs) identified manually.

phq4Tbl3 <- 
  phq4Tbl3[!(phq4Tbl3$id %in% 
               c(1500, 1812, 1035, 1668, 1817)), ]

## Implement DECISION 12.

# View(phq4Tbl3[(phq4Tbl3$participantRSA %in% 
#                  c(125, 879, 782)), 
#               c(1, 7, 2, 8)])

# Remove row for ids (note: not participant IDs) identified manually.

phq4Tbl3 <- 
  phq4Tbl3[!(phq4Tbl3$id %in% 
               c(1304, 1715)), ]

## Implement DECISION 13.

# View(phq4Tbl3[(phq4Tbl3$participantRSA %in% 
#                  c(408, 732)), 
#               c(1, 7, 2, 8)])

## Remove row for ids (note: not participant IDs) identified manually.

phq4Tbl3 <- 
  phq4Tbl3[!(phq4Tbl3$id %in% 
               c(1664, 1461)), ]

## Implement DECISION 14.

# View(phq4Tbl3[(phq4Tbl3$participantRSA %in% 
#                  c(662)), 
#               c(1, 7, 2, 8)])

## Remove row for ids (note: not participant IDs) identified manually.

phq4Tbl3 <- 
  phq4Tbl3[!(phq4Tbl3$id %in% 
               c(2047)), ]

# Confirm that there are no longer multiple entries.

summary <- unique(phq4Tbl3) %>% 
  group_by(participantRSA, session) %>% summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
unique(summarySubset$participantRSA)

length(unique(summarySubset$participantRSA))

#------------------------------------------------------------------------------#
# Import expectancybias table ----
#------------------------------------------------------------------------------#

# Import data from ExpectancyBias_recovered_Sep_10_2018.csv, obtained from
# Claudia Calicho-Mamani.

expectancyBiasTbl <- read.csv("./Data/Raw/ExpectancyBias_recovered_Sep_10_2018.csv", 
                              header = TRUE)

# View(expectancyBiasTbl)
# summary(expectancyBiasTbl)
# str(expectancyBiasTbl)

# Reorder session variable.

# levels(expectancyBiasTbl$session)
# table(expectancyBiasTbl$session)

expectancyBiasTbl$session <- ordered(expectancyBiasTbl$session, 
                                     levels = c("", "ELIGIBLE", 
                                                "firstSession", "secondSession", 
                                                "thirdSession", "fourthSession", 
                                                "PostFollowUp"))

# Rearrange variable columns.

# View(expectancyBiasTbl)

expectancyBiasTbl <- expectancyBiasTbl[ , c("participant", "session", 
                                              "shortRest", "reruns", 
                                              "verySick", "offend", 
                                              "settleIn", "bagel", "lunch", 
                                              "consideredAdvancement", "stuck", 
                                              "financiallySecure", "thermostat", 
                                              "ruining", "date", "id", 
                                              "sessionId", "timeOnPage", "tag")]

# Create dataset for analysis by selecting only subjects who enrolled after 
# 3/27/18. This will exclude test accounts and subjects whose participant IDs
# are blank. A different dataset based on expectancyBiasTbl will be created 
# for analyzing subject flow below (see Analyze Subject Flow [Part 3]).

expectancyBiasTbl2analysis <- 
  expectancyBiasTbl[expectancyBiasTbl$participant %in% 
                                           IDsEnrolled, ]

# View(expectancyBiasTbl2analysis)
# length(unique(expectancyBiasTbl2analysis$participant))

# Drop the unused level of session, recode Eligibility, reorder the levels, 
# and make session an ordered factor.

# str(expectancyBiasTbl2analysis$session)
# levels(expectancyBiasTbl2analysis$session)
# table(expectancyBiasTbl2analysis$session)

expectancyBiasTbl2analysis$session <- 
  droplevels(expectancyBiasTbl2analysis$session)

expectancyBiasTbl2analysis$session <- 
  recode_factor(expectancyBiasTbl2analysis$session, ELIGIBLE = "Eligibility")

expectancyBiasTbl2analysis$session <- ordered(expectancyBiasTbl2analysis$session, 
                                      levels = c("Eligibility", 
                                                 "firstSession", 
                                                 "secondSession", 
                                                 "thirdSession", 
                                                 "fourthSession", 
                                                 "PostFollowUp"))

# Sort by participant, session, and id.

expectancyBiasTbl2analysis <- 
  expectancyBiasTbl2analysis[order(expectancyBiasTbl2analysis$participant, 
                                   expectancyBiasTbl2analysis$session,
                                   expectancyBiasTbl2analysis$id), ]

# View(expectancyBiasTbl2analysis)

# Rename rows.

# str(expectancyBiasTbl2analysis)
rownames(expectancyBiasTbl2analysis) <- 1:nrow(expectancyBiasTbl2analysis)
# View(expectancyBiasTbl2analysis)

# Check number of entries per session.

# View(expectancyBiasTbl2analysis %>% 
#        group_by(session, participant) %>% summarise(count=n()))
# View(unique(expectancyBiasTbl2analysis) %>% 
#        group_by(session, participant) %>% summarise(count=n()))

# There are 44 subjects with multiple entries.

summary <- unique(expectancyBiasTbl2analysis) %>% 
  group_by(participant, session) %>% summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
unique(summarySubset$participant)

length(unique(summarySubset$participant))

expectancyBiasTbl2multipleIDs <- unique(summarySubset$participant)

#------------------------------------------------------------------------------#
# Resolve multiple entries in expectancybias table ----
#------------------------------------------------------------------------------#

# Implement decisions documented in separate cleaning script.

# Implement DECISION 1.

# View(expectancyBiasTbl2analysis[expectancyBiasTbl2analysis$participant %in% 
#                                   c(200, 392, 412, 495, 496, 577, 582, 627, 
#                                     634, 961, 453, 788, 138, 942), ])

expectancyBiasTbl3analysis <- 
  expectancyBiasTbl2analysis[!(expectancyBiasTbl2analysis$participant %in% 
                                 c(200, 392, 412, 495, 496, 577, 582, 627, 
                                   634, 961, 453, 788, 138, 942)), ]

# Implement DECISION 2.

# View(expectancyBiasTbl3analysis[(expectancyBiasTbl3analysis$participant %in% 
#                                   c(178, 434, 517, 594, 758, 874, 1055, 1061, 
#                                     1068, 1094, 1138, 1141, 1162, 1165, 1186, 
#                                     1245, 1271)) & 
#                                  (expectancyBiasTbl3analysis$session == 
#                                   "Eligibility"), c(1, 16, 2, 15)])

## Remove rows for ids (note: not participant IDs) identified manually.

expectancyBiasTbl3analysis <- 
  expectancyBiasTbl3analysis[!(expectancyBiasTbl3analysis$id %in%
                                 c(718, 1743, 2072, 2510, 3322, 3931, 5136,
                                   5218, 5271, 5603, 6297, 6328, 6420, 6432, 
                                   6596, 6926, 7056, 7119)), ]

# Implement DECISION 3.

# View(expectancyBiasTbl3analysis[(expectancyBiasTbl3analysis$participant ==
#                                    992) & 
#                                 (expectancyBiasTbl3analysis$session == 
#                                      "Eligibility"), c(1, 16, 2, 15)])

## Remove row for id (note: not participant ID) identified manually.

expectancyBiasTbl3analysis <- 
  expectancyBiasTbl3analysis[!(expectancyBiasTbl3analysis$id == 4642), ]

# Implement DECISION 5.

# View(expectancyBiasTbl3analysis[(expectancyBiasTbl3analysis$participant ==
#                                    734) & 
#                                   (expectancyBiasTbl3analysis$session == 
#                                      "thirdSession"), ])

## Remove row for id (note: not participant ID) identified manually.

expectancyBiasTbl3analysis <- 
  expectancyBiasTbl3analysis[!(expectancyBiasTbl3analysis$id == 6120), ]

# Implement DECISION 9.

# View(expectancyBiasTbl3analysis[(expectancyBiasTbl3analysis$participant ==
#                                    916) & 
#                                   (expectancyBiasTbl3analysis$session == 
#                                      "thirdSession"), ])

## Remove rows for ids (note: not participant IDs) identified manually.

expectancyBiasTbl3analysis <- 
  expectancyBiasTbl3analysis[!(expectancyBiasTbl3analysis$id %in%
                                 c(5921, 6053)), ]

# Implement DECISION 10

# View(expectancyBiasTbl3analysis[(expectancyBiasTbl3analysis$participant %in%
#                                    c(943, 565, 1066, 49, 1160)) & 
#                                   (expectancyBiasTbl3analysis$session == 
#                                      "firstSession"), ])

## Remove rows for ids (note: not participant IDs) identified manually.

expectancyBiasTbl3analysis <- 
  expectancyBiasTbl3analysis[!(expectancyBiasTbl3analysis$id %in%
                                 c(5768, 6451)), ]

# Implement DECISION 11

# View(expectancyBiasTbl3analysis[(expectancyBiasTbl3analysis$participant %in%
#                                    c(1184, 1135)) & 
#                                   (expectancyBiasTbl3analysis$session == 
#                                      "firstSession"), ])

## Remove rows for ids (note: not participant IDs) identified manually.

expectancyBiasTbl3analysis <- 
  expectancyBiasTbl3analysis[!(expectancyBiasTbl3analysis$id %in%
                                 c(6406, 6666)), ]

# Implement DECISION 12

# View(expectancyBiasTbl3analysis[(expectancyBiasTbl3analysis$participant %in%
#                                    c(125, 879, 782)) & 
#                                   (expectancyBiasTbl3analysis$session %in% 
#                                      c("secondSession", "fourthSession")), ])

## Remove rows for ids (note: not participant IDs) identified manually.

expectancyBiasTbl3analysis <- 
  expectancyBiasTbl3analysis[!(expectancyBiasTbl3analysis$id %in%
                                 c(5354, 4562, 6224, 6374)), ]

# Implement DECISION 13

# View(expectancyBiasTbl3analysis[(expectancyBiasTbl3analysis$participant %in%
#                                    c(408, 732)) & 
#                                   (expectancyBiasTbl3analysis$session %in% 
#                                      c("fourthSession")), ])

## Remove rows for ids (note: not participant IDs) identified manually.

expectancyBiasTbl3analysis <- 
  expectancyBiasTbl3analysis[!(expectancyBiasTbl3analysis$id %in%
                                 c(6133, 5679)), ]

# Implement DECISION 14

# View(expectancyBiasTbl3analysis[(expectancyBiasTbl3analysis$participant %in%
#                                    c(662)) & 
#                                   (expectancyBiasTbl3analysis$session %in% 
#                                      c("PostFollowUp")), ])

## Remove rows for ids (note: not participant IDs) identified manually.

expectancyBiasTbl3analysis <- 
  expectancyBiasTbl3analysis[!(expectancyBiasTbl3analysis$id %in%
                                 c(6882)), ]

# Confirm that there are no longer multiple entries.

summary <- unique(expectancyBiasTbl3analysis) %>% 
  group_by(participant, session) %>% summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
unique(summarySubset$participant)

length(unique(summarySubset$participant))

#------------------------------------------------------------------------------#
# Analyze participant flow (Part 3) ----
#------------------------------------------------------------------------------#

# Create dataset for subject flow based on expectancyBiasTbl by sorting by id 
# and removing entries (note: not subjects this time) after 3/27/18 (i.e., 
# ids greater than 7216).

expectancyBiasTblflow <- expectancyBiasTbl[order(expectancyBiasTbl$id), ]

expectancyBiasTblflow <- expectancyBiasTblflow[expectancyBiasTblflow$id <= 
                                                 7216, ]

rownames(expectancyBiasTblflow) <- 1:nrow(expectancyBiasTblflow)

# View(expectancyBiasTblflow)

# Restrict expectancyBiasTblflow to screening.

# str(expectancyBiasTblflow$session)
# levels(expectancyBiasTblflow$session)
expectancyBiasTblscreening <- 
  expectancyBiasTblflow[expectancyBiasTblflow$session == "" |
                           expectancyBiasTblflow$session == "ELIGIBLE", ]
# View(expectancyBiasTblscreening)

# Remove 33 test accounts.

expectancyBiasTbl2screening <- subset(expectancyBiasTblscreening, 
                             !(expectancyBiasTblscreening$participant %in% 
                                 participantTblTestAccts$study_id))

sort(unique(subset(expectancyBiasTblscreening, 
                   (expectancyBiasTblscreening$participant %in% 
                      participantTblTestAccts$study_id))$participant))
length(unique(subset(expectancyBiasTblscreening, 
                     (expectancyBiasTblscreening$participant %in% 
                        participantTblTestAccts$study_id))$participant))

# There are 1221 participant IDs, which match the 1221 in participantTbl.
# However, there are also rows where participantID is NA. ParticipantIDs are
# created only when the subject enrolls in the study by creating an account.

length(unique(expectancyBiasTbl2screening$participant)) # Includes 1 level of NA.
setdiff(unique(expectancyBiasTbl2screening$participant), IDsEnrolled)

# View(expectancyBiasTbl2screening)

# Date that study is considered launched (i.e., first expectancyBias submission)

sum(is.na(expectancyBiasTbl2screening$id))
expectancyBiasTbl2screening[expectancyBiasTbl2screening$id == 
                              sort(expectancyBiasTbl2screening$id)[1], ]$date

# Rename rows.

# str(expectancyBiasTbl2screening)
rownames(expectancyBiasTbl2screening) <- 1:nrow(expectancyBiasTbl2screening)

# Replace 161 responses of 555 (Prefer not to answer) with NA.

sum(expectancyBiasTbl2screening[ , 3:14] == 555, na.rm = TRUE)

expectancyBiasTbl2screening[ , 3:14][expectancyBiasTbl2screening[ , 3:14] == 
                                     555] <- NA

# Compute posExpBiasScale by taking the mean of shortRest, settleIn, 
# consideredAdvancement, and financiallySecure. Use mean across available 
# items for item-level missing data.

# sum(is.na(expectancyBiasTbl2screening$shortRest))
# sum(is.na(expectancyBiasTbl2screening$settleIn))
# sum(is.na(expectancyBiasTbl2screening$consideredAdvancement))
# sum(is.na(expectancyBiasTbl2screening$financiallySecure))
# 
# sum(!is.na(expectancyBiasTbl2screening$shortRest))
# sum(!is.na(expectancyBiasTbl2screening$settleIn))
# sum(!is.na(expectancyBiasTbl2screening$consideredAdvancement))
# sum(!is.na(expectancyBiasTbl2screening$financiallySecure))

expectancyBiasTbl2screening$posExpBiasScale <- 
  rowMeans(expectancyBiasTbl2screening[ , c("shortRest", "settleIn", 
                                            "consideredAdvancement", 
                                            "financiallySecure")], 
                                            na.rm = TRUE)
# sum(!is.na(expectancyBiasTbl2screening$posExpBiasScale))
# sum(is.nan(expectancyBiasTbl2screening$posExpBiasScale))
# which(is.nan(expectancyBiasTbl2screening$posExpBiasScale))

# Compute negExpBiasScale by taking the mean of verySick, offend, stuck, 
# and ruining. Use mean across available items for item-level missing data.

# sum(is.na(expectancyBiasTbl2screening$verySick))
# sum(is.na(expectancyBiasTbl2screening$offend))
# sum(is.na(expectancyBiasTbl2screening$stuck))
# sum(is.na(expectancyBiasTbl2screening$ruining))
# 
# sum(!is.na(expectancyBiasTbl2screening$verySick))
# sum(!is.na(expectancyBiasTbl2screening$offend))
# sum(!is.na(expectancyBiasTbl2screening$stuck))
# sum(!is.na(expectancyBiasTbl2screening$ruining))

expectancyBiasTbl2screening$negExpBiasScale <- 
  rowMeans(expectancyBiasTbl2screening[ , c("verySick", "offend", "stuck", 
                                            "ruining")], na.rm = TRUE)
# sum(!is.na(expectancyBiasTbl2screening$negExpBiasScale))
# sum(is.nan(expectancyBiasTbl2screening$negExpBiasScale))
# which(is.nan(expectancyBiasTbl2screening$negExpBiasScale))

# Compute netExpBiasScale by subtracting negExpBiasScale from posExpBiasScale.

expectancyBiasTbl2screening$netExpBiasScale <- 
  expectancyBiasTbl2screening$posExpBiasScale -
  expectancyBiasTbl2screening$negExpBiasScale
# sum(!is.na(expectancyBiasTbl2screening$netExpBiasScale))
# sum(is.nan(expectancyBiasTbl2screening$netExpBiasScale))
# which(is.nan(expectancyBiasTbl2screening$netExpBiasScale))

# Compute biasEligible as 1 if netExpBiasScale < 1.1111 and as 0 otherwise.

for(i in 1:nrow(expectancyBiasTbl2screening)){
  if(is.nan(expectancyBiasTbl2screening$netExpBiasScale[i])){
    expectancyBiasTbl2screening$biasEligible[i] <- 0
  } else if(expectancyBiasTbl2screening$netExpBiasScale[i] < 1.1111){
    expectancyBiasTbl2screening$biasEligible[i] <- 1
  } else if(expectancyBiasTbl2screening$netExpBiasScale[i] >= 1.1111){
    expectancyBiasTbl2screening$biasEligible[i] <- 0
  }
}

# Use sessionId to identify subjects because participantID is NA until subject 
# enrolls by creating an account, at which point participantID is created and 
# session, previously empty, is marked "ELIGIBLE," as shown below. Note that
# here, ELIGIBLE means that on at least one screening attempt, the subject (a)
# met the expectancy bias criterion, (b) checked a box that the subject is at
# least 18 years of age, and (c) enrolled in the study by creating an account.
# When this happens, a participantID is created and session is marked ELIGIBLE
# for every screening attempt corresponding to the subject's session Id.

all(which(is.na(expectancyBiasTbl2screening$participant)) == 
      which(expectancyBiasTbl2screening$session == ""))
all(which(!is.na(expectancyBiasTbl2screening$participant)) == 
      which(expectancyBiasTbl2screening$session == "ELIGIBLE"))

# Order by session and then sessionId.

expectancyBiasTbl2screening <- 
  expectancyBiasTbl2screening[order(expectancyBiasTbl2screening$session, 
                                    expectancyBiasTbl2screening$sessionId), ]

# 2440 subjects were ineligible based on computed bias score.

sum(is.na(expectancyBiasTbl2screening$participant) & 
      expectancyBiasTbl2screening$biasEligible == 0)
biasIneligibleRows <- 
  expectancyBiasTbl2screening[is.na(expectancyBiasTbl2screening$participant) & 
                              expectancyBiasTbl2screening$biasEligible == 0, ]
length(biasIneligibleRows$sessionId)

length(unique(biasIneligibleRows$sessionId)) # *USED IN FLOWCHART*

## Check number of rows per sessionId.

## View(biasIneligibleRows %>% group_by(sessionId) %>% summarise(count=n()))

## 81 subjects attempted more than once (209 extra attempts).

summary <- biasIneligibleRows %>% group_by(sessionId) %>% summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
length(summarySubset$sessionId)

sum(summarySubset$count - 1)

# 1090 subjects were eligible based on computed bias score but did not enroll
# either because they were ineligible based on age or declined (website did not
# differentiate between these, so impossible to tease them apart; need to ask
# for age before inviting to enroll in future studies rather than asking the
# subject to confirm age on the invitation screen used to create an account).

sum(is.na(expectancyBiasTbl2screening$participant) & 
      expectancyBiasTbl2screening$biasEligible == 1)
biasEligibleRows <- 
  expectancyBiasTbl2screening[is.na(expectancyBiasTbl2screening$participant) & 
                                expectancyBiasTbl2screening$biasEligible == 1, ]
length(biasEligibleRows$sessionId)

length(unique(biasEligibleRows$sessionId)) # *USED IN FLOWCHART*

## Check number of rows per sessionId.

# View(biasEligibleRows %>% group_by(sessionId) %>% summarise(count=n()))

## 31 people attempted more than once (31 extra attempts).

summary <- biasEligibleRows %>% group_by(sessionId) %>% summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
length(summarySubset$sessionId)

sum(summarySubset$count - 1)

# Excluded = Ineligible based on computed bias score (see above) + Ineligible
# based on age or declined (see above)

2440 + 1090 # *USED IN FLOWCHART*

# Assessed for eligibility = Randomized (see Analyze Subject Flow [Part 1]) + 
# Excluded (see above)

1221 + 3530 # *USED IN FLOWCHART*

# Determine exclusions from the ITT and PP analysis samples.

## 15 subjects (16 entries) were deemed eligible but didn't meet bias criterion
## on at least one screening attempt.

discEntries <- setdiff(which(expectancyBiasTbl2screening$session == "ELIGIBLE"), 
                       which(expectancyBiasTbl2screening$biasEligible == 1))
discEntries
length(discEntries)

discSubjects <- unique(expectancyBiasTbl2screening[discEntries, ]$participant)
sort(discSubjects)
length(discSubjects)

## Use this to search per participant:

## View(expectancyBiasTbl2screening[which(expectancyBiasTbl2screening$participant 
##                                        == 1303), ])

## 1 subject had a blank screen. *USED IN FLOWCHART*

## Subject 1303: NA for all expectancyBias items.

## 14 subjects had repeat screens. *USED IN FLOWCHART*

## Subject 138: Ineligible on attempt 1; eligible on attempt 2 (3 min later)
## Subject 200: Ineligible on attempt 1; eligible on attempt 2 (2 min later)
## Subject 392: Ineligible on attempt 1; eligible on attempt 2 (2 min later)
## Subject 412: Ineligible on attempt 1; eligible on attempt 2 (21 min later)
## Subject 453: Ineligible on attempt 1; 
##                  eligible on attempts 2-3, which are duplicated (7 min later)
## Subject 495: Ineligible on attempt 1; eligible on attempt 2 (4 min later)
## Subject 496: Ineligible on attempt 1; eligible on attempt 2 (2 min later)
## Subject 577: Ineligible on attempt 1; eligible on attempt 2 (1 min later)
## Subject 582: Ineligible on attempt 1; eligible on attempt 2 (1 min later)
## Subject 627: Ineligible on attempt 1; eligible on attempt 2 (1 min later)
## Subject 634: Ineligible on attempt 1; eligible on attempt 2 (2 min later)
## Subject 788: Ineligible on attempts 1-2, which are duplicated;
##                                           eligible on attempt 3 (8 min later)
## Subject 942: Ineligible on attempt 1; eligible on attempt 2 (2 min later)
## Subject 961: Ineligible on attempt 1; eligible on attempt 2 (6 min later)

#------------------------------------------------------------------------------#
# Import whatibelieve table ----
#------------------------------------------------------------------------------#

# Import data from WhatIBelieve_recovered_Sep_10_2018.csv, obtained from
# Claudia Calicho-Mamani.

whatIBelieveTbl <- read.csv("./Data/Raw/WhatIBelieve_recovered_Sep_10_2018.csv", 
                            header = TRUE)

# View(whatIBelieveTbl)
# summary(whatIBelieveTbl)
# str(whatIBelieveTbl)

# Remove 46 test accounts.

# length(unique(whatIBelieveTbl$participantRSA))

whatIBelieveTbl2 <- subset(whatIBelieveTbl, 
                           !(whatIBelieveTbl$participantRSA %in% 
                               participantTblTestAccts$study_id))

# sort(unique(subset(whatIBelieveTbl, 
#                    (whatIBelieveTbl$participantRSA %in% 
#                       participantTblTestAccts$study_id))$participantRSA))
# length(unique(subset(whatIBelieveTbl, 
#                      (whatIBelieveTbl$participantRSA %in% 
#                         participantTblTestAccts$study_id))$participantRSA))

# length(unique(whatIBelieveTbl2$participantRSA))
# View(whatIBelieveTbl2)

# Remove subjects who enrolled after 3/27/18, leaving 1138 subjects.

whatIBelieveTbl2 <- whatIBelieveTbl2[whatIBelieveTbl2$participantRSA %in% 
                                       IDsEnrolled, ]

# Rearrange variable columns.

whatIBelieveTbl2 <- 
  whatIBelieveTbl2[ , c("participantRSA", "session", "learn", 
                        "particularThinking", "alwaysChangeThinking", 
                        "difficultTasks", "performEffectively", "compared", 
                        "wrongWill", "hardlyEver", "date", "id", "timeOnPage", 
                        "tag")]

# Reorder the levels of session and it an ordered factor.

# head(whatIBelieveTbl2$session)
# str(whatIBelieveTbl2$session)
# levels(whatIBelieveTbl2$session)
# table(whatIBelieveTbl2$session)

whatIBelieveTbl2$session <- 
  ordered(whatIBelieveTbl2$session, levels = c("preTest", 
                                               "secondSession", 
                                               "fourthSession", 
                                               "PostFollowUp"))

# Sort by participantRSA, session, and id.

whatIBelieveTbl2 <- whatIBelieveTbl2[order(whatIBelieveTbl2$participantRSA, 
                                           whatIBelieveTbl2$session,
                                           whatIBelieveTbl2$id), ]

# View(whatIBelieveTbl2)

# Rename rows.

# str(whatIBelieveTbl2)
rownames(whatIBelieveTbl2) <- 1:nrow(whatIBelieveTbl2)
# View(whatIBelieveTbl2)

# Check number of entries per session.

# View(whatIBelieveTbl2 %>% 
#        group_by(session, participantRSA) %>% summarise(count=n()))
# View(unique(whatIBelieveTbl2) %>% 
#        group_by(session, participantRSA) %>% summarise(count=n()))

# There are 30 subjects with multiple entries.

summary <- unique(whatIBelieveTbl2) %>% 
  group_by(participantRSA, session) %>% summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
unique(summarySubset$participantRSA)

length(unique(summarySubset$participantRSA))

whatIBelieveTbl2multipleIDs <- unique(summarySubset$participantRSA)

#------------------------------------------------------------------------------#
# Resolve multiple entries in whatibelieve table ----
#------------------------------------------------------------------------------#

# Implement decisions documented in separate cleaning script.

# Implement DECISION 1.

# View(whatIBelieveTbl2[whatIBelieveTbl2$participantRSA %in% 
#                 c(200, 392, 412, 495, 496, 577, 582, 627, 
#                   634, 961, 453, 788, 138, 942), ])

whatIBelieveTbl3 <- 
  whatIBelieveTbl2[!(whatIBelieveTbl2$participantRSA %in% 
               c(200, 392, 412, 495, 496, 577, 582, 627, 
                 634, 961, 453, 788, 138, 942)), ]

# Implement DECISION 7.

# View(whatIBelieveTbl3[(whatIBelieveTbl3$participantRSA %in% 
#                       c(883, 584, 91, 1112, 1130, 261, 223, 814, 1031)) &
#                       (whatIBelieveTbl3$session == "preTest"), 
#                       c(1, 12, 2, 11) ])

## Remove rows for ids (note: not participant IDs) identified manually.

whatIBelieveTbl3 <- 
  whatIBelieveTbl3[!(whatIBelieveTbl3$id %in% 
               c(81, 997, 238, 522, 1023, 852, 1150, 1152, 1553, 1710)), ]

# Implement DECISION 8.

# View(whatIBelieveTbl3[(whatIBelieveTbl3$participantRSA %in% 
#                          c(130, 137, 760, 950, 711, 533, 947, 895)) &
#                         (whatIBelieveTbl3$session == "preTest"), 
#                       c(1, 12, 2, 11) ])

## Remove rows for ids (note: not participant IDs) identified manually.

whatIBelieveTbl3 <- 
  whatIBelieveTbl3[!(whatIBelieveTbl3$id %in% 
                       c(119, 128, 779, 637, 688, 881, 1505, 993)), ]

# Implement DECISION 9.

# View(whatIBelieveTbl3[(whatIBelieveTbl3$participantRSA %in% 
#                          c(916)), 
#                       c(1, 12, 2, 11) ])

## Remove rows for ids (note: not participant IDs) identified manually.

whatIBelieveTbl3 <- 
  whatIBelieveTbl3[!(whatIBelieveTbl3$id %in% 
                       c(1379)), ]

# Implement DECISION 10.

# View(whatIBelieveTbl3[(whatIBelieveTbl3$participantRSA %in% 
#                          c(943, 565, 1066, 49, 1160)), 
#                       c(1, 12, 2, 11) ])

## Remove rows for ids (note: not participant IDs) identified manually.

whatIBelieveTbl3 <- 
  whatIBelieveTbl3[!(whatIBelieveTbl3$id %in% 
                       c(1501, 1813, 1035, 1669, 1818)), ]

# Implement DECISION 12.

# View(whatIBelieveTbl3[(whatIBelieveTbl3$participantRSA %in% 
#                          c(125, 879, 782)), 
#                       c(1, 12, 2, 11) ])

## Remove rows for ids (note: not participant IDs) identified manually.

whatIBelieveTbl3 <- 
  whatIBelieveTbl3[!(whatIBelieveTbl3$id %in% 
                       c(1305, 1716)), ]

# Implement DECISION 13.

# View(whatIBelieveTbl3[(whatIBelieveTbl3$participantRSA %in% 
#                          c(408, 732)), 
#                       c(1, 12, 2, 11) ])

## Remove rows for ids (note: not participant IDs) identified manually.

whatIBelieveTbl3 <- 
  whatIBelieveTbl3[!(whatIBelieveTbl3$id %in% 
                       c(1665, 1462)), ]

# Implement DECISION 14.

# View(whatIBelieveTbl3[(whatIBelieveTbl3$participantRSA %in% 
#                          c(662)), 
#                       c(1, 12, 2, 11) ])

## Remove rows for ids (note: not participant IDs) identified manually.

whatIBelieveTbl3 <- 
  whatIBelieveTbl3[!(whatIBelieveTbl3$id %in% 
                       c(2048)), ]

# Confirm that there are no longer multiple entries.

summary <- unique(whatIBelieveTbl3) %>% 
  group_by(participantRSA, session) %>% summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
unique(summarySubset$participantRSA)

length(unique(summarySubset$participantRSA))

#------------------------------------------------------------------------------#
# Import demographics table ----
#------------------------------------------------------------------------------#

# Starting with "./Data/Raw/Demographics_recovered_Sep_10_2018.csv", obtained 
# from Claudia Calicho-Mamani, identify rows where participantRSA is non-numeric. 
# The correct participantRSA values for these rows are in maritalStatus. For 
# such participantRSA values (1, 2, 7, 8, 9, and 10), manually shift the data 
# one column to the right starting with the devices column to create
# "./Data/Temp/Demographics_recovered_Sep_10_2018_manuallyCorrected.csv".

# Import data from Demographics_recovered_Sep_10_2018_manuallyCorrected.csv.

demographicTbl <- 
  read.csv("./Data/Temp/Demographics_recovered_Sep_10_2018_manuallyCorrected.csv", 
           header = TRUE)

# View(demographicTbl)
# summary(demographicTbl)
# str(demographicTbl)

# Remove 27 test accounts.

# length(unique(demographicTbl$participantRSA))

demographicTbl2 <- subset(demographicTbl, !(demographicTbl$participantRSA %in% 
                                              participantTblTestAccts$study_id))

# sort(unique(subset(demographicTbl, 
#                    (demographicTbl$participantRSA %in% 
#                       participantTblTestAccts$study_id))$participantRSA))
# length(unique(subset(demographicTbl, 
#                      (demographicTbl$participantRSA %in% 
#                         participantTblTestAccts$study_id))$participantRSA))

# length(unique(demographicTbl2$participantRSA))
# View(demographicTbl2)

# Remove subjects who enrolled after 3/27/18, leaving 1154 subjects.

demographicTbl2 <- demographicTbl2[demographicTbl2$participantRSA %in% 
                                       IDsEnrolled, ]

# Rearrange variable columns.

demographicTbl2 <- demographicTbl2[ , c("participantRSA", "session", 
                                        "birthYear", "genderId", "race", 
                                        "ethnicity", "maritalStat", "education", 
                                        "employmentStat", "income", "country", 
                                        "devices", "ptpReason", 
                                        "ptpReasonOther", "date", "id", 
                                        "timeOnPage", "tag")]

# Make session an ordered factor.

# head(demographicTbl2$session)
# str(demographicTbl2$session)
# levels(demographicTbl2$session)
# table(demographicTbl2$session)

demographicTbl2$session <- 
  ordered(demographicTbl2$session, levels = c("preTest"))

# Sort by participantRSA, session, and id.

# View(demographicTbl2)

demographicTbl2 <- demographicTbl2[order(demographicTbl2$participantRSA, 
                                         demographicTbl2$session,
                                         demographicTbl2$id), ]

# View(demographicTbl2)

# Rename rows.

# str(demographicTbl2)
rownames(demographicTbl2) <- 1:nrow(demographicTbl2)
# View(demographicTbl2)

# Check number of entries per session.

# View(demographicTbl2 %>% 
#        group_by(session, participantRSA) %>% summarise(count=n()))
# View(unique(demographicTbl2) %>% 
#        group_by(session, participantRSA) %>% summarise(count=n()))

# There are 33 subjects with multiple entries.

summary <- unique(demographicTbl2) %>% group_by(participantRSA, session) %>% 
  summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
unique(summarySubset$participantRSA)

length(unique(summarySubset$participantRSA))

demographicTbl2multipleIDs <- unique(summarySubset$participantRSA)

#------------------------------------------------------------------------------#
# Resolve multiple entries in demographics table ----
#------------------------------------------------------------------------------#

# Implement decisions documented in separate cleaning script.

# Implement DECISION 1.

# View(demographicTbl2[demographicTbl2$participantRSA %in% 
#                         c(200, 392, 412, 495, 496, 577, 582, 627, 
#                           634, 961, 453, 788, 138, 942), ])

demographicTbl3 <- 
  demographicTbl2[!(demographicTbl2$participantRSA %in% 
                       c(200, 392, 412, 495, 496, 577, 582, 627, 
                         634, 961, 453, 788, 138, 942)), ]

# Implement DECISION 4.

# View(demographicTbl3[(demographicTbl3$participantRSA %in% 
#                          c(239, 352, 971, 834, 862)), 
#                      c(1, 16, 2, 15, 3:14, 17:18)])

## Remove rows for ids (note: not participant IDs) identified manually.

demographicTbl3 <- 
  demographicTbl3[!(demographicTbl3$id %in% 
                       c(217, 309, 759, 775, 1019, 1005)), ]

# Implement DECISION 5.

# View(demographicTbl3[(demographicTbl3$participantRSA %in% 
#                         c(734)), 
#                      c(1, 16, 2, 15, 3:14, 17:18)])

## Remove rows for ids (note: not participant IDs) identified manually.

demographicTbl3 <- 
  demographicTbl3[!(demographicTbl3$id %in% 
                      c(665)), ]

# Implement DECISION 6.

# View(demographicTbl3[(demographicTbl3$participantRSA %in% 
#                         c(969, 469)), 
#                      c(1, 16, 2, 15, 3:14, 17:18)])

## Remove rows for ids (note: not participant IDs) identified manually.

demographicTbl3 <- 
  demographicTbl3[!(demographicTbl3$id %in% 
                      c(417, 898)), ]

# Implement DECISION 7.

# View(demographicTbl3[(demographicTbl3$participantRSA %in% 
#                         c(883, 584, 91, 1112, 1130, 261, 223, 814, 1031)), 
#                      c(1, 16, 2, 15, 3:14, 17:18)])

## Remove rows for ids (note: not participant IDs) identified manually.

demographicTbl3 <- 
  demographicTbl3[!(demographicTbl3$id %in% 
                      c(73, 872, 234, 524, 887, 805, 955, 957, 1059, 1078)), ]

# Implement DECISION 8.

# View(demographicTbl3[(demographicTbl3$participantRSA %in% 
#                         c(130, 137, 760, 950, 711, 533, 947, 895)), 
#                      c(1, 16, 2, 15, 3:14, 17:18)])

## Remove rows for ids (note: not participant IDs) identified manually.

demographicTbl3 <- 
  demographicTbl3[!(demographicTbl3$id %in% 
                      c(111, 122, 771, 642, 694, 814, 1048, 871)), ]

# Implement DECISION 9.

# View(demographicTbl3[(demographicTbl3$participantRSA %in% 
#                         c(916)), 
#                      c(1, 16, 2, 15, 3:14, 17:18)])

## Remove rows for ids (note: not participant IDs) identified manually.

demographicTbl3 <- 
  demographicTbl3[!(demographicTbl3$id %in% 
                      c(1025)), ]

# Implement DECISION 10.

# View(demographicTbl3[(demographicTbl3$participantRSA %in% 
#                         c(943, 565, 1066, 49, 1160)), 
#                      c(1, 16, 2, 15, 3:14, 17:18)])

## Remove rows for ids (note: not participant IDs) identified manually.

demographicTbl3 <- 
  demographicTbl3[!(demographicTbl3$id %in% 
                      c(1047, 1111, 891, 1072, 1113)), ]

# Confirm that there are no longer multiple entries.

summary <- unique(demographicTbl3) %>% group_by(participantRSA, session) %>% 
  summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
unique(summarySubset$participantRSA)

length(unique(summarySubset$participantRSA))

# Additional data cleaning for ITT subjects is conducted in the separate script
# "1c_further_cleaning_demographics.R." Demographic data have not been cleaned 
# for all randomized subjects.

#------------------------------------------------------------------------------#
# Import mentalhealthhistory table ----
#------------------------------------------------------------------------------#

# Import data from MentalHealthHistory_recovered_Sep_10_2018.csv.

mentalHealthHxTbl <- 
  read.csv("./Data/Raw/MentalHealthHistory_recovered_Sep_10_2018.csv", 
           header = TRUE)

# Remove 29 test accounts.

# length(unique(mentalHealthHxTbl$participantRSA))

mentalHealthHxTbl2 <- subset(mentalHealthHxTbl, 
                             !(mentalHealthHxTbl$participantRSA %in% 
                                 participantTblTestAccts$study_id))

# sort(unique(subset(mentalHealthHxTbl,
#                    (mentalHealthHxTbl$participantRSA %in%
#                       participantTblTestAccts$study_id))$participantRSA))
# length(unique(subset(mentalHealthHxTbl,
#                      (mentalHealthHxTbl$participantRSA %in%
#                         participantTblTestAccts$study_id))$participantRSA))

# length(unique(mentalHealthHxTbl2$participantRSA))

# Remove subjects who enrolled after 3/27/2018, leaving 1142 subjects.

mentalHealthHxTbl2 <- mentalHealthHxTbl2[mentalHealthHxTbl2$participantRSA %in% 
                                          IDsEnrolled, ]

# Rearrange variable columns.

mentalHealthHxTbl2 <- 
  mentalHealthHxTbl2[ , c("participantRSA", "session", "app", "app_past", 
                          "book", "book_past", "changeHelp", "coach", 
                          "coach_past", "disorders", "family", "family_past", 
                          "friend", "friend_past", "general_practitioner", 
                          "general_practitioner_past", "help", "lmhc", 
                          "lmhc_past", "medicine", "medicine_past", 
                          "noHelp_Reason", "online", "online_past", "other", 
                          "other_Desc", "other_DescNo", "other_HelpChange", 
                          "other_HelpCurrent", "other_HelpPast", 
                          "other_NoHelpReason", "other_past", "pastDisorders", 
                          "pastHelp", "psychiatrist", "psychiatrist_past", 
                          "psychologist", "psychologist_past", 
                          "religious_leader", "religious_leader_past", 
                          "school_counselor", "school_counselor_past", 
                          "support_group", "support_group_past", "teacher", 
                          "teacher_past", "date", "id", "timeOnPage", "tag")]

# All observations are at preTest

table(mentalHealthHxTbl2$session)

# Sort by participantRSA and id.

mentalHealthHxTbl2 <- mentalHealthHxTbl2[order(mentalHealthHxTbl2$participantRSA, 
                                                 mentalHealthHxTbl2$id), ]

# Rename rows.

rownames(mentalHealthHxTbl2) <- 1:nrow(mentalHealthHxTbl2)

# Check number of entries per session.

# View(mentalHealthHxTbl2 %>%
#        group_by(participantRSA) %>% summarise(count=n()))
# View(unique(mentalHealthHxTbl2) %>%
#        group_by(participantRSA) %>% summarise(count=n()))

# There are 27 subjects with multiple entries.

summary <- unique(mentalHealthHxTbl2) %>% group_by(participantRSA) %>% 
  summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
unique(summarySubset$participantRSA)

length(unique(summarySubset$participantRSA))

mentalHealthHxTbl2multipleIDs <- unique(summarySubset$participantRSA)

#------------------------------------------------------------------------------#
# Resolve multiple entries in mentalhealthhistory table ----
#------------------------------------------------------------------------------#

# Implement decisions documented in separate cleaning script.

# Implement DECISION 1.

# View(mentalHealthHxTbl2[mentalHealthHxTbl2$participantRSA %in%
#                           c(200, 392, 412, 495, 496, 577, 582, 627,
#                             634, 961, 453, 788, 138, 942), ])

mentalHealthHxTbl3 <- 
  mentalHealthHxTbl2[!(mentalHealthHxTbl2$participantRSA %in% 
                         c(200, 392, 412, 495, 496, 577, 582, 627, 
                           634, 961, 453, 788, 138, 942)), ]

# Implement DECISION 4.

# View(mentalHealthHxTbl2[(mentalHealthHxTbl2$participantRSA %in%
#                          c(862)),
#                      c(1, 48)])

## Remove rows for ids (note: not participant IDs) identified manually.

mentalHealthHxTbl3 <- 
  mentalHealthHxTbl3[!(mentalHealthHxTbl3$id %in% 
                      c(1002)), ]

# Implement DECISION 5.

# View(mentalHealthHxTbl2[(mentalHealthHxTbl2$participantRSA %in% c(734)), ])

## Remove rows for ids (note: not participant IDs) identified manually.

mentalHealthHxTbl3 <- 
  mentalHealthHxTbl3[!(mentalHealthHxTbl3$id %in% c(654)), ]

# Implement DECISION 7.

# View(mentalHealthHxTbl2[(mentalHealthHxTbl2$participantRSA %in%
#                            c(883, 584, 91, 1112, 1130, 261, 223, 814, 1031)), 
#                         c(1, 48)])

## Remove rows for ids (note: not participant IDs) identified manually.

mentalHealthHxTbl3 <- 
  mentalHealthHxTbl3[!(mentalHealthHxTbl3$id %in% 
                      c(74, 858, 232, 515, 872, 792, c(939, 941), 1043, 1062)), ]

# Implement DECISION 8.

# View(mentalHealthHxTbl2[(mentalHealthHxTbl2$participantRSA %in%
#                            c(130, 137, 760, 950, 711, 533, 947, 895)),
#                          c(1, 48)])

## Remove rows for ids (note: not participant IDs) identified manually.

mentalHealthHxTbl3 <- 
  mentalHealthHxTbl3[!(mentalHealthHxTbl3$id %in% 
                         c(112, 121, 758, 631, 683, 801, 1031, 857)), ]

# Implement DECISION 9.

# View(mentalHealthHxTbl2[(mentalHealthHxTbl2$participantRSA %in%
#                            c(916)),
#                          c(1, 48)])

## Remove rows for ids (note: not participant IDs) identified manually.

mentalHealthHxTbl3 <- 
  mentalHealthHxTbl3[!(mentalHealthHxTbl3$id %in% 
                      c(1008)), ]

# Implement DECISION 10.

# View(mentalHealthHxTbl2[(mentalHealthHxTbl2$participantRSA %in%
#                            c(943, 565, 1066, 49, 1160)),
#                          c(1, 48)])

## Remove rows for ids (note: not participant IDs) identified manually.

mentalHealthHxTbl3 <- 
  mentalHealthHxTbl3[!(mentalHealthHxTbl3$id %in% 
                         c(1030, 1096, 876, 1056, 1098)), ]

# Confirm that there are no longer multiple entries.

summary <- unique(mentalHealthHxTbl3) %>% group_by(participantRSA) %>% 
  summarise(count=n())
summarySubset <- subset(summary, summary$count > 1)
unique(summarySubset$participantRSA)

length(unique(summarySubset$participantRSA))

#------------------------------------------------------------------------------#
# Export partially cleaned mentalhealthhistory table ----
#------------------------------------------------------------------------------#

# Rename participant ID column and remove tag column.

names(mentalHealthHxTbl3)[names(mentalHealthHxTbl3) == "participantRSA"] <-
  "participantId"
mentalHealthHxTbl3$tag <- NULL

# Add condition

tempRandomize <- randomizeTbl2
tempRandomize$participantId <- tempRandomize$id
tempRandomize$id <- NULL
tempRandomize <- tempRandomize[ , c(2, 1)]

mentalHealthHxTbl3 <- merge(mentalHealthHxTbl3, tempRandomize, 
                            by = "participantId",
                            all.x = TRUE)

# Save table

write.csv(mentalHealthHxTbl3, file = "./Data/Temp/mentalHealthHxTbl3.csv")

# Additional data cleaning for ITT subjects is conducted in separate script 
# "1d_further_cleaning_mentalHealthHx.R." Mental health history data have 
# not been cleaned for all randomized subjects.

#------------------------------------------------------------------------------#
# Build data file ----
#------------------------------------------------------------------------------#

# Subject 1303 (blank screen) remains in tables, whereas 14 subjects with repeat 
# screens have already been excluded from tables except randomizeTbl2.

# 1303 %in% randomizeTbl2$id
# 1303 %in% credibilityTbl3$participantRSA
# 1303 %in% expectancyBiasTbl3analysis$participant
# 1303 %in% demographicTbl3$participantRSA
# 1303 %in% phq4Tbl3$participantRSA
# 1303 %in% whatIBelieveTbl3$participantRSA
# 
# length(unique(randomizeTbl2$id))
# length(unique(credibilityTbl3$participantRSA))
# length(unique(expectancyBiasTbl3analysis$participant))
# length(unique(demographicTbl3$participantRSA))
# length(unique(phq4Tbl3$participantRSA))
# length(unique(whatIBelieveTbl3$participantRSA))
# 
# length(setdiff(IDsEnrolled, unique(expectancyBiasTbl3analysis$participant)))

# Build a complete set of rows to fill in for 1221 randomized subjects. You
# will need to exclude 14 subjects with repeat screens (rows will be blank)
# and 1 subject with NA screen (rows will have data) later.

tempSessionVector <- c("Eligibility", "preTest", "firstSession", 
                       "secondSession", "thirdSession", "fourthSession", 
                       "PostFollowUp")

mainData <- 
  data.frame("participantId" = rep(unique(randomizeTbl2$id), 
                                   each = length(tempSessionVector)),
             "session" = rep(tempSessionVector,
                             times = length(unique(randomizeTbl2$id))))

# Add condition.

# length(unique(mainData$participantId))
# length(unique(randomizeTbl2$id))
# setequal(unique(mainData$participantId), unique(randomizeTbl2$id))

tempRandomize <- randomizeTbl2
tempRandomize$participantId <- tempRandomize$id
tempRandomize$id <- NULL
tempRandomize <- tempRandomize[ , c(2, 1)]

mainData <- merge(mainData, tempRandomize, by = c("participantId"), 
                  all.x = TRUE)

# View(mainData)

# Reorder the levels of session and make it an ordered factor.

# str(mainData$session)
# table(mainData$session)

mainData$session <- 
  ordered(mainData$session, levels = c("Eligibility", "preTest", "firstSession", 
                                       "secondSession", "thirdSession", 
                                       "fourthSession", "PostFollowUp"))

# Order by participantId, session, and scenario.

mainData <- mainData[order(mainData$participantId, mainData$session), ]

# Add items from measures.

FTdata <- mainData

# View(FTdata)

# Add credibility items.

# View(credibilityTbl3)
# length(unique(credibilityTbl3$participantRSA))
# length(unique(FTdata$participantId))

tempCred <- (credibilityTbl3[ , c("participantRSA", "session", 
                                  "confident_design", "confident_online", 
                                  "important")])
tempCred$participantId <- tempCred$participantRSA
tempCred$participantRSA <- NULL
tempCred <- tempCred[ , c(5, 1:4)]
# View(tempCred)

FTdata <- merge(FTdata, tempCred, by = c("participantId", "session"), 
                all.x = TRUE)

FTdata <- FTdata[order(FTdata$participantId, FTdata$session), ]

# View(FTdata)

# Add PHQ-4 items.

# View(phq4Tbl3)
# length(unique(phq4Tbl3$participantRSA))
# length(unique(FTdata$participantId))

tempPHQ4 <- (phq4Tbl3[ , c("participantRSA", "session", "nervous", "worry", 
                           "pleasure", "depressed")])
tempPHQ4$participantId <- tempPHQ4$participantRSA
tempPHQ4$participantRSA <- NULL
tempPHQ4 <- tempPHQ4[ , c(6, 1:5)]
# View(tempPHQ4)

FTdata <- merge(FTdata, tempPHQ4, by = c("participantId", "session"), 
                all.x = TRUE)

FTdata <- FTdata[order(FTdata$participantId, FTdata$session), ]

# View(FTdata)

# Add expectancyBias items.

# View(expectancyBiasTbl3analysis)
# length(unique(expectancyBiasTbl3analysis$participant))
# length(unique(FTdata$participantId))

tempExpectancyBias <- 
  (expectancyBiasTbl3analysis[ , c("participant", "session", "shortRest", 
                                   "settleIn", "consideredAdvancement", 
                                   "financiallySecure", "verySick", "offend", 
                                   "stuck", "ruining", "reruns", "bagel", 
                                   "lunch", "thermostat")])
tempExpectancyBias$participantId <- tempExpectancyBias$participant
tempExpectancyBias$participant <- NULL
tempExpectancyBias <- tempExpectancyBias[ , c(14, 1:13)]
# View(tempExpectancyBias)

FTdata <- merge(FTdata, tempExpectancyBias, by = c("participantId", "session"), 
                all.x = TRUE)

FTdata <- FTdata[order(FTdata$participantId, FTdata$session), ]

# View(FTdata)

# Add growth mindset, self-efficacy, and optimism items.

# View(whatIBelieveTbl3)
# length(unique(whatIBelieveTbl3$participantRSA))
# length(unique(FTdata$participantId))

tempWhatIBelieve <- (whatIBelieveTbl3[ , c("participantRSA", "session", 
                                           "learn", "particularThinking", 
                                           "alwaysChangeThinking", 
                                           "difficultTasks", 
                                           "performEffectively", "compared", 
                                           "wrongWill", "hardlyEver")])
tempWhatIBelieve$participantId <- tempWhatIBelieve$participantRSA
tempWhatIBelieve$participantRSA <- NULL
tempWhatIBelieve <- tempWhatIBelieve[ , c(10, 1:9)]
# View(tempWhatIBelieve)

FTdata <- merge(FTdata, tempWhatIBelieve, by = c("participantId", "session"), 
                all.x = TRUE)

FTdata <- FTdata[order(FTdata$participantId, FTdata$session), ]

# Add demographics items.

# View(demographicTbl3)
# length(unique(demographicTbl3$participantRSA))
# length(unique(FTdata$participantId))

tempDemographic <- (demographicTbl3[ , c("participantRSA", "session", 
                                         "birthYear", "date", "genderId", 
                                         "race", "ethnicity", "maritalStat", 
                                         "education", "employmentStat", 
                                         "income", "country")])
tempDemographic$participantId <- tempDemographic$participantRSA
tempDemographic$participantRSA <- NULL
tempDemographic$demographicDateStamp <- tempDemographic$date
tempDemographic$date <- NULL
tempDemographic <- tempDemographic[ , c(11, 1:2, 12, 3:10)]
# View(tempDemographic)

FTdata <- merge(FTdata, tempDemographic, by = c("participantId", 
                                                "session"), all.x = TRUE)

FTdata <- FTdata[order(FTdata$participantId, FTdata$session), ]

# View(FTdata)

# Store as FTmainData.

FTmainData <- FTdata

# Write to a CSV file.

write.csv(FTmainData, file = "./Data/Temp/FTmainData.csv")

# Import FTmainData.

FTmainData <- read.csv("./Data/Temp/FTmainData.csv")

# Remove the X column, which are just the row names at time of export to 
# CSV and are redundant.

# View(FTmainData)

FTmainData$X <- NULL

# Reorder the levels of session and make it an ordered factor.

FTmainData$session <- ordered(FTmainData$session, 
                              levels = c("Eligibility", "preTest", 
                                         "firstSession", "secondSession", 
                                         "thirdSession", "fourthSession", 
                                         "PostFollowUp"))

FTmainData <- FTmainData[order(FTmainData$participantId, FTmainData$session), ]

# View(FTmainData)

#------------------------------------------------------------------------------#
# Compute variables ----
#------------------------------------------------------------------------------#

# Remove 69 responses of 555 (Prefer not to answer) from non-demographic
# items.

# View(FTmainData)
# str(FTmainData)

sum(FTmainData[ , 4:30] == 555, na.rm = TRUE)

FTmainData[ , 4:30][FTdata[ , 4:30] == 555] <- NA

# Compute anxietyScale by summing nervous and worry. There are no item-level 
# missing data.

# sum(!is.na(FTmainData$nervous))
# sum(!is.na(FTmainData$worry))

FTmainData$anxietyScale <- rowSums(FTmainData[ , c("nervous", "worry")], 
                                   na.rm = FALSE)
# sum(!is.na(FTmainData$anxietyScale))

FTmainData <- FTmainData[ , c(1:8, 41, 9:40)]
# View(FTmainData)

##### Compute depressionScale by summing pleasure and depressed. There are no 
# item-level missing data.

# sum(!is.na(FTmainData$pleasure))
# sum(!is.na(FTmainData$depressed))

FTmainData$depressionScale <- rowSums(FTmainData[ , c("pleasure", "depressed")], 
                                      na.rm = FALSE)
# sum(!is.na(FTmainData$depressionScale))

FTmainData <- FTmainData[ , c(1:11, 42, 12:41)]
# View(FTmainData)

# Compute posExpBiasScale by taking the mean of shortRest, settleIn, 
# consideredAdvancement, and financiallySecure. Use mean across available 
# items for item-level missing data.

# sum(!is.na(FTmainData$shortRest))
# sum(!is.na(FTmainData$settleIn))
# sum(!is.na(FTmainData$consideredAdvancement))
# sum(!is.na(FTmainData$financiallySecure))

FTmainData$posExpBiasScale <- rowMeans(FTmainData[ , c("shortRest", "settleIn", 
                                                       "consideredAdvancement", 
                                                       "financiallySecure")], 
                                       na.rm = TRUE)
# sum(!is.na(FTmainData$posExpBiasScale))

FTmainData$posExpBiasScale[is.nan(FTmainData$posExpBiasScale)] <- NA

FTmainData <- FTmainData[ , c(1:16, 43, 17:42)]
# View(FTmainData)

# Compute negExpBiasScale by taking the mean of verySick, offend, stuck, 
# and ruining. Use mean across available items for item-level missing data.

# sum(!is.na(FTmainData$verySick))
# sum(!is.na(FTmainData$offend))
# sum(!is.na(FTmainData$stuck))
# sum(!is.na(FTmainData$ruining))

FTmainData$negExpBiasScale <- rowMeans(FTmainData[ , c("verySick", "offend", 
                                                       "stuck", "ruining")], 
                                       na.rm = TRUE)
# sum(!is.na(FTmainData$negExpBiasScale))

FTmainData$negExpBiasScale[is.nan(FTmainData$negExpBiasScale)] <- NA

FTmainData <- FTmainData[ , c(1:21, 44, 22:43)]
# View(FTmainData)

# Compute growthMindScale by reverse-scoring learn and then taking the 
# mean of learnRev, particularThinking, and alwaysChangeThinking. Use mean 
# across available items for item-level missing data. Note that although the 
# original 8 items are summed, because we used only 3 items, we use the mean.

# sum(!is.na(FTmainData$learn))

# table(FTmainData$learn)
FTmainData$learnRev <- 4 - FTmainData$learn
# table(FTmainData$learnRev)

# sum(!is.na(FTmainData$learnRev))
# sum(!is.na(FTmainData$particularThinking))
# sum(!is.na(FTmainData$alwaysChangeThinking))

FTmainData$growthMindScale <- rowMeans(FTmainData[ , c("learnRev", 
                                                       "particularThinking", 
                                                       "alwaysChangeThinking")], 
                                       na.rm = TRUE)
# sum(!is.na(FTmainData$growthMindScale))

FTmainData$growthMindScale[is.nan(FTmainData$growthMindScale)] <- NA

FTmainData <- FTmainData[ , c(1:27, 45, 28:29, 46, 30:44)]
# View(FTmainData)

# Compute selfEffScale by taking the mean of difficultTasks, 
# performEffectively, and compared. Use mean across available items for 
# item-level missing data. Note that although the original 8 items are summed, 
# because we used only 3 items, we will use the mean.

# sum(!is.na(FTmainData$difficultTasks))
# sum(!is.na(FTmainData$performEffectively))
# sum(!is.na(FTmainData$compared))

FTmainData$selfEffScale <- rowMeans(FTmainData[ , c("difficultTasks", 
                                                    "performEffectively", 
                                                    "compared")], na.rm = TRUE)
# sum(!is.na(FTmainData$selfEffScale))

FTmainData$selfEffScale[is.nan(FTmainData$selfEffScale)] <- NA

FTmainData <- FTmainData[ , c(1:34, 47, 35:46)]
# View(FTmainData)

# Compute optimismScale by reverse-scoring wrongWill and hardlyEver and then 
# taking the mean of wrongWillRev and hardlyEverRev. Use mean across 
# available items for item-level missing data. Note that although the original
# 10 items are summed, because we used only 2 items, we use the mean.

# sum(!is.na(FTmainData$wrongWill))
# sum(!is.na(FTmainData$hardlyEver))

# table(FTmainData$wrongWill)
FTmainData$wrongWillRev <- 4 - FTmainData$wrongWill
# table(FTmainData$wrongWillRev)

# table(FTmainData$hardlyEver)
FTmainData$hardlyEverRev <- 4 - FTmainData$hardlyEver
# table(FTmainData$hardlyEverRev)

# sum(!is.na(FTmainData$wrongWillRev))
# sum(!is.na(FTmainData$hardlyEverRev))

FTmainData$optimismScale <- rowMeans(FTmainData[ , c("wrongWillRev", 
                                                     "hardlyEverRev")], 
                                     na.rm = TRUE)
# sum(!is.na(FTmainData$optimismScale))

FTmainData$optimismScale[is.nan(FTmainData$optimismScale)] <- NA

FTmainData <- FTmainData[ , c(1:36, 48, 37, 49, 50, 38:47)]
# View(FTmainData)

# Write scored items and scales to a CSV file.

FTmainDataItemsScales <- FTmainData[ , 1:40]

# View(FTmainDataItemsScales)

write.csv(FTmainDataItemsScales, file = "./Data/Clean/FTmainDataItemsScales.csv")

# Write scales only to a CSV file.

FTmainDataScales <- FTmainData[ , c(1:3, 9, 12, 17, 22, 31, 35, 40)]

# View(FTmainDataScales)

write.csv(FTmainDataScales, file = "./Data/Clean/FTmainDataScales.csv")

# Write demographics items only to a CSV file.

FTmainDataDemog <- FTmainData[ , c(1:3, 41:50)]

# View(FTmainDataDemog)

write.csv(FTmainDataDemog, file = "./Data/Temp/FTmainDataDemog.csv")

#------------------------------------------------------------------------------#
# Define analysis samples ----
#------------------------------------------------------------------------------#

# Determine best indicator of treatment completers.

# Identify subjects who completed training at Session 4 per taskLogTbl2. Note 
# that these include only subjects who enrolled through 3/27/18 but are based 
# on data collected through 9/10/18.

taskLogTbl2completerIDs <- 
  unique(taskLogTbl2[taskLogTbl2$sessionName == "fourthSession" & 
                       taskLogTbl2$taskName == "JsPsychTrial", ]$participantId)

length(taskLogTbl2completerIDs)

# Identify subjects who completed training at Session 4 per psychTrialTbl2. Note 
# that these are based on data collected only through 3/27/18. Nevertheless, it
# is possible to identify whether subjects who have a "JsPsychTrial" entry at 
# Session 4 before 3/27/18 in taskLogTbl2 also have training data at Session 
# 4 before this date in psychTrialTbl2.

psychTrialTblold <- read.csv("./Data/Raw/JsPsychTrial_recovered_Mar_27_2018.csv", 
                             header = TRUE)

psychTrialTbl2old <- subset(psychTrialTblold, !(psychTrialTblold$participantId %in% 
                                                  participantTblTestAccts$study_id))

psychTrialTbl2old <- psychTrialTbl2old[ , c("participantId", "condition", "session", 
                                            "id", "trial_index", "internal_node_id", 
                                            "stimulus", "trial_type", 
                                            "button_pressed", "correct", 
                                            "rt_correct", "rt", "time_elapsed", 
                                            "device", "study")]

psychTrialTbl2old$session <- 
  ordered(psychTrialTbl2old$session, levels = c("firstSession", "secondSession", 
                                                "thirdSession", "fourthSession"))

psychTrialTbl2old <- 
  psychTrialTbl2old[order(psychTrialTbl2old$condition, 
                          psychTrialTbl2old$participantId, 
                          psychTrialTbl2old$session, 
                          psychTrialTbl2old$id), ]

tap <- NA

for(i in 1:nrow(psychTrialTbl2old)){
  if(psychTrialTbl2old$trial_index[i] == 0){
    tap <- 0
  }
  if(psychTrialTbl2old$trial_type[i] == "sentence-reveal"){
    tap <- tap + 1
  }
  psychTrialTbl2old$scenarioIndex[i] <- tap
}

psychTrialTbl2oldcompleterIDs <- 
  unique(psychTrialTbl2old[psychTrialTbl2old$session == "fourthSession" &
                             psychTrialTbl2old$scenarioIndex == 40 &
                             psychTrialTbl2old$stimulus == "final score screen", 
                           ]$participantId)

length(psychTrialTbl2oldcompleterIDs)

# 19 subjects have a "JsPsychTrial" entry at Session 4 in taskLogTbl2 indicating 
# that they completed training at that session, but they don't have training 
# data for that session in psychTrialTbl2. 

length(setdiff(taskLogTbl2completerIDs, psychTrialTbl2oldcompleterIDs))

# Use this to search specific subjects.

# View(taskLogTbl2[taskLogTbl2$participantId == 49, ])
# View(psychTrialTbl2old[psychTrialTbl2old$participantId == 49, ])

# The 19 subjects' "JsPsychTrial" entries in taskLogTbl came about 30 min or more 
# after the "preAffect" entries in taskLogTbl. Dan Funk thinks that the reason 
# they don't have corresponding training data in psychTrialTbl is that their 
# online session had timed out (see approximate elapsed time since the "preAffect"
# entries below) and when they pressed the button to complete the training, they 
# were redirected to log in again. Dan thinks that a "JsPsychTrial" entry was 
# recorded in taskLogTbl when they pressed the button but that the redirection 
# resulted in a loss of their training data for the session. Thus, these subjects
# are still treatment completers.

# 49: 52 min
# 104: 24 hr
# 127: 33 min
# 251: 35 min
# 266: 15 hr
# 529: 30 min
# 621: 30 min
# 662: 33 min
# 665: 1 hr
# 785: 4 hr
# 824: 4 hr
# 857: 52 min
# 859: 43 min
# 870: 44 min
# 892: 3 hr
# 975: 36 min
# 1072: 10 days
# 1272: 1 hr
# 1299: 45 min

# Everyone who completed treatment also started S1.

setdiff(taskLogTbl2completerIDs, IDsStartedS1)

# The taskLogTbl2completerIDs is thus the correct list of people who completed
# treatment. Use these IDs rather than psychTrialTbl2oldcompleterIDs. View them 
# by condition.

# View(unique(taskLogTbl2[taskLogTbl2$participantId %in% 
#                           taskLogTbl2completerIDs, c("participantId", 
#                                                      "condition")]))

completerCond <- unique(taskLogTbl2[taskLogTbl2$participantId %in% 
                                      taskLogTbl2completerIDs, c("participantId", 
                                                                 "condition")])

table(completerCond$condition)

# Consider who from the excluded IDs started treatment or completed treatment.
# Use this code to search for specific participants.

# 1303 %in% IDsStartedS1
# 1303 %in% taskLogTbl2completerIDs

# Create data frame with 1221 randomized subjects.

FTmainAnalysisSamples <- data.frame("participantId" = unique(randomizeTbl2$id))

# Add condition.

tempRandomize <- randomizeTbl2
tempRandomize$participantId <- tempRandomize$id
tempRandomize$id <- NULL
tempRandomize <- tempRandomize[ , c(2, 1)]

FTmainAnalysisSamples <- merge(FTmainAnalysisSamples, tempRandomize, 
                               by = c("participantId"), 
                               all.x = TRUE)

# Exclude subjects to define the 958 ITT and 289 treatment completer samples.

# From the 971 who started training at S1 and the 293 who completed training 
# at S4, exclude 14 subjects who attempted expectancyBias multiple times at 
# screening and 1 who answered NA for all expectancyBias items at screening.

length(IDsStartedS1)
ittSample <- setdiff(IDsStartedS1, c(495, 788, 961, 392, 412, 582, 634, 577, 
                                     627, 942, 138, 200, 453, 496))
ittSample <- setdiff(ittSample, c(1303))
length(ittSample)

for(i in 1:nrow(FTmainAnalysisSamples)){
  if(FTmainAnalysisSamples$participantId[i] %in% ittSample){
    FTmainAnalysisSamples$ittSample[i] <- 1
  } else{
    FTmainAnalysisSamples$ittSample[i] <- 0
  }
}
sum(FTmainAnalysisSamples$ittSample)

length(taskLogTbl2completerIDs)
txCompSample <- setdiff(taskLogTbl2completerIDs, c(495, 788, 961, 392, 412, 
                                                   582, 634, 577, 627, 942, 
                                                   138, 200, 453, 496))
txCompSample <- setdiff(txCompSample, c(1303))
length(txCompSample)

for(i in 1:nrow(FTmainAnalysisSamples)){
  if(FTmainAnalysisSamples$participantId[i] %in% txCompSample){
    FTmainAnalysisSamples$txCompSample[i] <- 1
  } else{
    FTmainAnalysisSamples$txCompSample[i] <- 0
  }
}
sum(FTmainAnalysisSamples$txCompSample)

FTmainAnalysisSamples <- 
  FTmainAnalysisSamples[order(FTmainAnalysisSamples$participantId), ]

# View(FTmainAnalysisSamples)

# Write IDs in ITT and treatment completer samples to a CSV file.

write.csv(FTmainAnalysisSamples, file = "./Data/Clean/FTmainAnalysisSamples.csv")

#------------------------------------------------------------------------------#
# Import the exported data ----
#------------------------------------------------------------------------------#

# These data are clean.

FTmainDataItemsScales <- read.csv("./Data/Clean/FTmainDataItemsScales.csv")
FTmainDataScales <- read.csv("./Data/Clean/FTmainDataScales.csv")
FTmainAnalysisSamples <- read.csv("./Data/Clean/FTmainAnalysisSamples.csv")

# These data require additional cleaning.

FTmainDataDemog <- read.csv("./Data/Temp/FTmainDataDemog.csv")
mentalHealthHxTbl3 <- read.csv("./Data/Temp/mentalHealthHxTbl3.csv")

# Remove the X column, which are just the row names at time of export to 
# CSV and are redundant.

FTmainDataItemsScales$X <- NULL
FTmainDataScales$X <- NULL
FTmainDataDemog$X <- NULL
FTmainAnalysisSamples$X <- NULL
mentalHealthHxTbl3$X <- NULL

# Reorder the levels of session and make it an ordered factor, except for
# mentalHealthHxTbl3, which only has rows at "preTest"

FTmainDataItemsScales$session <- 
  ordered(FTmainDataItemsScales$session, 
            levels = c("Eligibility", "preTest", "firstSession", 
                       "secondSession", "thirdSession", "fourthSession", 
                       "PostFollowUp"))

FTmainDataScales$session <- 
  ordered(FTmainDataScales$session, 
            levels = c("Eligibility", "preTest", "firstSession", 
                       "secondSession", "thirdSession", "fourthSession", 
                       "PostFollowUp"))

FTmainDataDemog$session <- 
  ordered(FTmainDataDemog$session, 
            levels = c("Eligibility", "preTest", "firstSession", "secondSession", 
                       "thirdSession", "fourthSession", "PostFollowUp"))

table(mentalHealthHxTbl3$session)

# View(FTmainDataItemsScales)
# View(FTmainDataScales)
# View(FTmainDataDemog)
# View(FTmainAnalysisSamples)
# View(mentalHealthHxTbl3)