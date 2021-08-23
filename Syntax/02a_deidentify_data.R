#------------------------------------------------------------------------------#
# Deidentify Data
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

# Store working directory

wd_dir <- getwd()

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# This script imports raw data from the "Data/Raw" folder and, for files that 
# need redaction, outputs redacted files into the same folder with "-redacted" 
# appended to the file name. The unredacted file can then be manually deleted.
# The "Data/Raw" files uploaded to OSF have already gone through this process.

# Rather than changing the structure of the raw data files, the present script 
# is designed to label instances of redaction with "REDACTED". Because it does
# not change the structure of the raw data files, the present script should run
# on redacted raw data files in addition to unredacted raw data files. In this
# way, the script reflects exactly what was redacted.

# Scope: This script is based on the data files listed below. The script may 
# need to be updated when applied to more recent versions of these files or to
# other data files, as there may have been changes to the database or newly 
# collected data not accounted for by this script.

# ---------------------------------------------------------------------------- #
# Name files checked for deidentification ----
# ---------------------------------------------------------------------------- #

deidentified <- c("Credibility_recovered_Sep_10_2018.csv",
                  "Demographics_recovered_Sep_10_2018.csv",
                  "Evaluation_recovered_Sep_10_2018.csv",
                  "ExpectancyBias_recovered_Sep_10_2018.csv",
                  "JsPsychTrial_recovered_Mar_27_2018.csv",
                  "MentalHealthHistory_recovered_Sep_10_2018-redacted.csv",
                  "Phq4_recovered_Sep_10_2018.csv",
                  "ReasonsForEnding_recovered_Sep_10_2018.csv",
                  "Relatability_recovered_Sep_10_2018.csv",
                  "report_Aug_19_2018.csv",
                  "T.participant_Mar_27_2018_fixed.csv",
                  "TaskLog_recovered_corrected_Sep_10_2018.csv",
                  "WhatIBelieve_recovered_Sep_10_2018.csv")

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

# Obtain file names of raw data files, regardless of whether they are CSV files

raw_data_dir <- paste0(wd_dir, "/Data/Raw")
filenames <- list.files(raw_data_dir, full.names = FALSE)

# Report files not yet checked for deidentification. Once a file is checked,
# add its file name to the "deidentified" character vector above.

if (length(setdiff(filenames, deidentified)) != 0) {
  stop(paste0("This file needs to be checked for deidentification: ",
               setdiff(filenames, deidentified)))
}

# Import CSV data files and store them in a list

filenames_csv <- list.files(raw_data_dir, pattern = "*.csv", full.names = FALSE)
data <- lapply(paste0(raw_data_dir, "/", filenames_csv), read.csv)

# Name each data file in the list

split_char <- "_"
names(data) <- unlist(lapply(filenames_csv, 
                             function(f) {
                               unlist(strsplit(f, 
                                               split = split_char, 
                                               fixed = FALSE))[1]
                             }
))

# Report the names of the imported tables

cat("Imported tables: ")
names(data)

# ---------------------------------------------------------------------------- #
# Deidentify Data ----
# ---------------------------------------------------------------------------- #

# Before doing any data cleaning, confirm that the raw data are deidentified so
# that subsequent cleaning scripts can be run on a deidentified dataset that can
# be shared and made public. This way all data cleaning steps can be transparent
# and reproducible without compromising identifiable information.

# ---------------------------------------------------------------------------- #
# Determine which columns contain potential identifiers ----
# ---------------------------------------------------------------------------- #

# Regarding various dates, Bethany indicated on 1/13/2021 that MindTrails is not
# subject to HIPAA regulations, so restrictions on dislcosing dates of service, 
# dates of test measures, and birth years for people over age 89 do not apply

# Manually inspected columns for tables in the "deidentified" vector above and 
# determined instances where removing potential identifiers may be advisable

# ---------------------------------------------------------------------------- #
# Remove potential identifiers from MentalHealthHistory table ----
# ---------------------------------------------------------------------------- #

data$MentalHealthHistory[grepl("Started going to ",
                         data$MentalHealthHistory$other_HelpChange), ]$other_HelpChange <-
  "Started going to [REDACTED mental health advocacy organizations]"

data$MentalHealthHistory[grepl("Facebook mental health group called ",
                               data$MentalHealthHistory$other_HelpCurrent), ]$other_HelpCurrent <-
  "Facebook mental health group called [REDACTED]"

data$MentalHealthHistory[grepl(" guided meditations online",
                               data$MentalHealthHistory$other_HelpCurrent), ]$other_HelpCurrent <-
  "[REDACTED university] guided meditations online"

data$MentalHealthHistory[grepl("Counselor at ",
                               data$MentalHealthHistory$other_HelpPast), ]$other_HelpPast <-
  "Counselor at [REDACTED charity]"

# ---------------------------------------------------------------------------- #
# Save deidentified data ----
# ---------------------------------------------------------------------------- #

# Save redacted "MentalHealthHistory". Remember to manually delete the original file.

MentalHealthHistory_redacted_filename <- 
  paste0(gsub("*.csv", "", filenames_csv[grep("MentalHealthHistory", filenames_csv)]),
         "-redacted.csv")

write.csv(data$MentalHealthHistory, 
          paste0("./Data/Raw/", MentalHealthHistory_redacted_filename),
          row.names = FALSE)