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
# downloaded from the Open Science Framework (https://osf.io/jp5ws/)

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

# Import partially cleaned mental health history data, remove first column "X",
# rearrange columns, and convert character columns to factors

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

mentalHealthHx[sapply(mentalHealthHx, is.character)] <-
  lapply(mentalHealthHx[sapply(mentalHealthHx, is.character)], as.factor)

# Import sample data and restrict mental health history data to 958 ITT subjects

p <- read.csv("./Data/Clean/FTmainAnalysisSamples.csv")

ITT <- subset(p, ittSample == 1)
completers <- subset(p, txCompSample == 1)

mentalHealthHx_itt <- 
  mentalHealthHx[mentalHealthHx$participantId %in% ITT$participantId, ]

# Order condition levels based on mental health history table in manuscript and
# verify number of ITT subjects in each condition

mentalHealthHx_itt$condition <- factor(mentalHealthHx_itt$condition,
         levels(mentalHealthHx_itt$condition)[c(5, 4, 1, 2, 3)])

group.size <- summary(as.factor(mentalHealthHx_itt$condition))

# ---------------------------------------------------------------------------- #
# Clean selected variables based on free responses ----
# ---------------------------------------------------------------------------- #

# Change selected variables from factors to characters

mentalHealthHx_itt$disorders <- as.character(mentalHealthHx_itt$disorders)
mentalHealthHx_itt$pastDisorders <- as.character(mentalHealthHx_itt$pastDisorders)
mentalHealthHx_itt$help <- as.character(mentalHealthHx_itt$help)
mentalHealthHx_itt$pastHelp <- as.character(mentalHealthHx_itt$pastHelp)

# The following variables were cleaned with a fairly strict interpretation of the
# available response categories (e.g., for "disorders" variable, "cyclothymia" was 
# not considered "bipolar disorder" because it is not strictly bipolar disorder;
# for "help" variable, "therapy" was not considered "LMHC" because psychiatrists,
# for example, are another response option and can provide therapy too)

# Current diagnoses ("disorders")

View(mentalHealthHx_itt[grepl("Other", mentalHealthHx_itt$disorders) |
                          mentalHealthHx_itt$other_Desc != "", ])

# For participant 458, given "other_Desc" of "PMDD", replace "Other" in "disorders"
# with "depression" (not already there)
mentalHealthHx_itt[mentalHealthHx_itt$participantId == 458, "disorders"] <-
  sub("Other", "depression", 
    mentalHealthHx_itt[mentalHealthHx_itt$participantId == 458, "disorders"])

# For participant 660, given "other_Desc" of "Schizoaffective Disorder/ Bipolar 1 
# with Psychotic features", replace "Other" in "disorders" with "schiz" (not already 
# there) to accompany "bipolar" (already there)
mentalHealthHx_itt[mentalHealthHx_itt$participantId == 660, "disorders"] <-
  sub("Other", "schiz", 
      mentalHealthHx_itt[mentalHealthHx_itt$participantId == 660, "disorders"])

# For participant 729, given "other_Desc" of "acquired brain injury", replace 
# "Other" in "disorders" with "dem" (not already there)
mentalHealthHx_itt[mentalHealthHx_itt$participantId == 729, "disorders"] <-
  sub("Other", "dem", 
      mentalHealthHx_itt[mentalHealthHx_itt$participantId == 729, "disorders"])

# For participant 1021, given "other_Desc" of "Dysthymia, seasonal affective 
# disorder and substance abuse", replace "Other" in "disorders" with "depression"
# and "subst" (not already there)
mentalHealthHx_itt[mentalHealthHx_itt$participantId == 1021, "disorders"] <-
  sub("u'Other'", "u'depression', u'subst'", 
      mentalHealthHx_itt[mentalHealthHx_itt$participantId == 1021, "disorders"])

# For participant 1151, given "other_Desc" of "depression", replace "Other" in 
# "disorders" with "depression" (not already there)
mentalHealthHx_itt[mentalHealthHx_itt$participantId == 1151, "disorders"] <-
  sub("Other", "depression", 
      mentalHealthHx_itt[mentalHealthHx_itt$participantId == 1151, "disorders"])

# For participant 1162, given "other_Desc" of "mood disorder and depression with
# anxiety", replace "Other" in "disorders" with "depression" (not already there)
mentalHealthHx_itt[mentalHealthHx_itt$participantId == 1162, "disorders"] <-
  sub("Other", "depression", 
      mentalHealthHx_itt[mentalHealthHx_itt$participantId == 1162, "disorders"])

# Past diagnoses ("pastDisorders")

View(mentalHealthHx_itt[grepl("Other", mentalHealthHx_itt$pastDisorders) |
                          mentalHealthHx_itt$other_DescNo != "", ])

# For participant 628, given "other_DescNo" of "Postpartum depression", replace 
# "Other" in "pastDisorders" with "depression" (not already there)
mentalHealthHx_itt[mentalHealthHx_itt$participantId == 628, "pastDisorders"] <-
  sub("Other", "depression", 
      mentalHealthHx_itt[mentalHealthHx_itt$participantId == 628, "pastDisorders"])

# For participant 991, given "other_DescNo" of "none", replace "Other" in 
# "pastDisorders" with "noDiagnosis"
mentalHealthHx_itt[mentalHealthHx_itt$participantId == 991, "pastDisorders"] <-
  sub("Other", "noDiagnosis", 
      mentalHealthHx_itt[mentalHealthHx_itt$participantId == 991, "pastDisorders"])

# Current help ("help")

View(mentalHealthHx_itt[grepl("Other", mentalHealthHx_itt$help) |
                          mentalHealthHx_itt$other_HelpCurrent != "", ])

# For participant 277, given "other_HelpCurrent" containing "mental health group", 
# replace "Other" in "help" with "support_group"
  mentalHealthHx_itt[mentalHealthHx_itt$participantId == 277, "help"] <-
  sub("Other", "support_group", 
      mentalHealthHx_itt[mentalHealthHx_itt$participantId == 277, "help"])

# For participant 626, given "other_HelpCurrent" of "On prescription medicine", 
# replace "Other" in "help" with "medicine"
mentalHealthHx_itt[mentalHealthHx_itt$participantId == 626, "help"] <-
  sub("Other", "medicine", 
      mentalHealthHx_itt[mentalHealthHx_itt$participantId == 626, "help"])

# Past help ("pastHelp")

View(mentalHealthHx_itt[grepl("Other", mentalHealthHx_itt$pastHelp) |
                          mentalHealthHx_itt$other_HelpPast != "", ])

# No changes

# ---------------------------------------------------------------------------- #
# Export partially cleaned CSV file ----
# ---------------------------------------------------------------------------- #

# Note: "Prefer not to answer" remains coded as 555

write.csv(mentalHealthHx_itt, file = "./Data/Temp/mentalHealthHx_itt.csv")