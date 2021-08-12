#------------------------------------------------------------------------------#
# Further Cleaning Mental Health History
# Author: Jeremy W. Eberle
#------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------- #
# Check R version and load packages ----
# ---------------------------------------------------------------------------- #

script_R_version <- "R version 4.1.0 (2021-05-18)"
current_R_version <- R.Version()$version.string

if(current_R_version != script_R_version) {
  warning(paste0("This script is based on ", script_R_version,
                 ". You are running ", current_R_version, "."))
}

# No packages loaded

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/jp5ws/).

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

# Import partially cleaned mental health history data, remove first column "X",
# rearrange columns, and convert character columns to factors.

mentalHealthHx <- read.csv("./Data/Temp/mentalHealthHxTbl3.csv")

mentalHealthHx$X <- NULL

mentalHealthHx <- 
  mentalHealthHx[, c("participantId", "condition", "session", 
                     "disorders", "other_Desc", 
                     "pastDisorders", "other_DescNo", 
                     "help", "other_HelpCurrent", 
                     "pastHelp", "other_HelpPast", 
                     "psychiatrist", "psychologist", "school_counselor", "lmhc", 
                       "general_practitioner", "teacher", "family", "friend", 
                       "religious_leader", "coach", "book", "medicine", "online", 
                       "app", "support_group", "other", 
                     "psychiatrist_past", "psychologist_past", 
                       "school_counselor_past", "lmhc_past", 
                       "general_practitioner_past", "teacher_past", "family_past", 
                       "friend_past", "religious_leader_past", "coach_past", 
                       "book_past", "medicine_past", "online_past", "app_past", 
                       "support_group_past", "other_past", 
                     "noHelp_Reason", "other_NoHelpReason", 
                     "changeHelp", "other_HelpChange", 
                     "timeOnPage", "date", "id")]

# str(mentalHealthHx)
mentalHealthHx[sapply(mentalHealthHx, is.character)] <-
  lapply(mentalHealthHx[sapply(mentalHealthHx, is.character)], as.factor)
# str(mentalHealthHx)

# Import sample data and restrict mental health history data to 958 ITT subjects.

p <- read.csv("./Data/Clean/FTmainAnalysisSamples.csv")

ITT <- subset(p, ittSample == 1)
completers <- subset(p, txCompSample == 1)

mentalHealthHx.itt <- 
  mentalHealthHx[mentalHealthHx$participantId %in% ITT$participantId, ]

# Order condition levels based on mental health history table in manuscript and
# verify number of ITT subjects in each condition

# levels(mentalHealthHx.itt$condition)
mentalHealthHx.itt$condition <- factor(mentalHealthHx.itt$condition,
         levels(mentalHealthHx.itt$condition)[c(5, 4, 1, 2, 3)])

# table(mentalHealthHx.itt$condition)
group.size <- summary(as.factor(mentalHealthHx.itt$condition))

# ---------------------------------------------------------------------------- #
# Clean free responses following "Other" ----
# ---------------------------------------------------------------------------- #

# TODO: Current diagnoses

mentalHealthHx.itt[grepl("Other", mentalHealthHx.itt$disorders), "other_Desc"]

# [10] BPD = personality
# [16] PMDD = depression
# [20] Schizoaffective Disorder/ Bipolar 1 with Psychotic features = 
#        schiz AND bipolar
# [24] acquired brain injury = dem
# [35] CPTSD = PSTD
# [45] Dysthymia, seasonal affective disorder and substance abuse = 
#        depression AND subst
# [49] CPTSD = PSTD
# [50] depression = depression
# [51] mood disorder and depression with anxiety = depression





# TODO: Past diagnoses

mentalHealthHx.itt[grepl("Other", mentalHealthHx.itt$pastDisorders), "other_DescNo"]





# Current help

mentalHealthHx.itt[grepl("Other", mentalHealthHx.itt$help), "other_HelpCurrent"]





# Past help

mentalHealthHx.itt[grepl("Other", mentalHealthHx.itt$pastHelp), "other_HelpPast"]





# ---------------------------------------------------------------------------- #
# Further clean mental health history data and compute descriptives ----
# ---------------------------------------------------------------------------- #

# Current diagnoses

table(mentalHealthHx.itt$disorders)
table(mentalHealthHx.itt$other_Desc)

sum(grepl("PTSD", mentalHealthHx.itt$disorders)) # Typo in codebook
sum(grepl("PSTD", mentalHealthHx.itt$disorders)) # Search for "PSTD" instead

sum(grepl("subst", mentalHealthHx.itt$disorders)) # Not sure why Jianhui has 0

sum(grepl("other", mentalHealthHx.itt$disorders)) # Typo in codebook
sum(grepl("Other", mentalHealthHx.itt$disorders)) # Search for "Other" instead





# Past diagnoses

table(mentalHealthHx.itt$pastDisorders)
table(mentalHealthHx.itt$other_DescNo)

sum(grepl("PTSD", mentalHealthHx.itt$pastDisorders)) # Typo in codebook
sum(grepl("PSTD", mentalHealthHx.itt$pastDisorders)) # Search for "PSTD" instead

sum(grepl("subst", mentalHealthHx.itt$pastDisorders)) # Not sure why Jianhui has 0

sum(grepl("other", mentalHealthHx.itt$pastDisorders)) # Typo in codebook
sum(grepl("Other", mentalHealthHx.itt$pastDisorders)) # Search for "Other" instead





# Current help

table(mentalHealthHx.itt$help)
table(mentalHealthHx.itt$other_HelpCurrent)

sum(grepl("school_counselor", mentalHealthHx.itt$help)) # Not sure why Jianhui has 0

sum(grepl("religious_leader", mentalHealthHx.itt$help)) # Not sure why Jianhui has 0

sum(grepl("support_group", mentalHealthHx.itt$help)) # Not sure why Jianhui has 0





# Past help

table(mentalHealthHx.itt$pastHelp)
table(mentalHealthHx.itt$other_HelpPast)

sum(grepl("school_counselor", mentalHealthHx.itt$pastHelp)) # Not sure why Jianhui has 0

sum(grepl("religious_leader", mentalHealthHx.itt$pastHelp)) # Not sure why Jianhui has 0

sum(grepl("support_group", mentalHealthHx.itt$pastHelp)) # Not sure why Jianhui has 0





# TODO: Clean other variables (e.g., recode 555)









