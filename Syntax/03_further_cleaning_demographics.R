#------------------------------------------------------------------------------#
# Further Cleaning Demographics
# Authors: Jianhui Sun and Jeremy W. Eberle
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

# Import partially cleaned demographic data, remove first column "X", convert
# character columns to factors, and restrict to preTest.

Demographic <- read.csv("./Data/Temp/FTmainDataDemog.csv")

Demographic$X <- NULL

# str(Demographic)
Demographic[sapply(Demographic, is.character)] <- 
  lapply(Demographic[sapply(Demographic, is.character)], as.factor)
# str(Demographic)

Demographic.pretest <- Demographic[Demographic$session == 'preTest', ]

# Import sample data and restrict demograhpic data to the 958 ITT subjects.

p <- read.csv("./Data/Clean/FTmainAnalysisSamples.csv")

ITT <- subset(p, ittSample == 1)
completers <- subset(p, txCompSample == 1)

Demographic.pretest.itt <- 
  Demographic.pretest[which(Demographic.pretest$participantId 
                                                   %in% ITT$participantId), ]

# View(Demographic.pretest.itt)

# Order condition levels based on demographic table in manuscript and verify 
# number of ITT subjects in each condition.

# levels(Demographic.pretest.itt$condition)
Demographic.pretest.itt$condition <- 
  factor(Demographic.pretest.itt$condition, 
         levels(Demographic.pretest.itt$condition)[c(5, 4, 1, 2, 3)])
# levels(Demographic.pretest.itt$condition)

# table(Demographic.pretest.itt$condition)

group.size <- summary(Demographic.pretest.itt$condition)

# ---------------------------------------------------------------------------- #
# Further clean demographic data and compute descriptives ----
# ---------------------------------------------------------------------------- #

# Note: Subject 779 has only question marks for most items. Use the subject's 
# data where present; do not remove the subject from analyses. Where question 
# marks are present, assume this is a data collection error (rather than a 
# report that the subject doesn't know) because this occurred even in cases 
# where the subject did not have a free response field to put question marks.
# If an Unknown category exists for a variable, put the subject in that; if 
# an Unknown category does not already exist, create one.

Demographic.pretest.itt[Demographic.pretest.itt$participantId == 779, ]

levels(Demographic.pretest.itt$genderId) <- 
  c(levels(Demographic.pretest.itt$genderId), "Unknown")
levels(Demographic.pretest.itt$maritalStat) <- 
  c(levels(Demographic.pretest.itt$maritalStat), "Unknown")
levels(Demographic.pretest.itt$education) <- 
  c(levels(Demographic.pretest.itt$education), "Unknown")
levels(Demographic.pretest.itt$employmentStat) <- 
  c(levels(Demographic.pretest.itt$employmentStat), "Unknown")

Demographic.pretest.itt[Demographic.pretest.itt$participantId == 779,
                        'genderId'] <- "Unknown"
Demographic.pretest.itt[Demographic.pretest.itt$participantId == 779,
                        'race'] <- "Other or Unknown"
Demographic.pretest.itt[Demographic.pretest.itt$participantId == 779, 
                        'ethnicity'] <- "Unknown"
Demographic.pretest.itt[Demographic.pretest.itt$participantId == 779, 
                        'maritalStat'] <- "Unknown"
Demographic.pretest.itt[Demographic.pretest.itt$participantId == 779,
                        'education'] <- "Unknown"
Demographic.pretest.itt[Demographic.pretest.itt$participantId == 779,
                        'employmentStat'] <- "Unknown"
Demographic.pretest.itt[Demographic.pretest.itt$participantId == 779, 
                        'income'] <- "Don't know"

# No subjects have question marks in data anymore. Remove question marks as 
# levels of the variables.

sum(grepl("?", Demographic.pretest.itt$income, fixed = TRUE))

Demographic.pretest.itt$genderId <- 
  droplevels(Demographic.pretest.itt$genderId)
Demographic.pretest.itt$race <- 
  droplevels(Demographic.pretest.itt$race)
Demographic.pretest.itt$ethnicity <- 
  droplevels(Demographic.pretest.itt$ethnicity)
Demographic.pretest.itt$maritalStat <- 
  droplevels(Demographic.pretest.itt$maritalStat)
Demographic.pretest.itt$education <- 
  droplevels(Demographic.pretest.itt$education)
Demographic.pretest.itt$employmentStat <- 
  droplevels(Demographic.pretest.itt$employmentStat)
Demographic.pretest.itt$income <- 
  droplevels(Demographic.pretest.itt$income)

sum(grepl("?", levels(Demographic.pretest.itt$income), fixed = TRUE))

# Age

## Identify subjects with unreasonable birthYear. 

table(Demographic.pretest.itt$birthYear)

Demographic.pretest.itt[(Demographic.pretest.itt$birthYear >= 2000) |
                          (Demographic.pretest.itt$birthYear <= 1900), 
                             "birthYear"]
Demographic.pretest.itt[(Demographic.pretest.itt$birthYear >= 2000) |
                          (Demographic.pretest.itt$birthYear <= 1900), 
                             "participantId"]

## Use the code below to implement the following decisions for 8 ITT subjects.

### Subject 528 has birthYear = 1900. Based on demographicDateStamp, they would
### be at least 117 years old. As of 1/14/2019, the oldest living person is 116
### years old. Make the birthYear NA.

### Subject 774 has birthYear = 2017. This impossible. Make the birthYear NA.

### Subjects 47, 751, and 1272 appear to have entered more than just birth year.
### Infer the birth year based on the last two digits.

### Subjects 746, 769, and 1130 have birthYear of 2000 or 2002 and 
### demographicDateStamp suggesting they are less than 18 years old. Keep them 
### in the analysis but note this in the table.

Demographic.pretest.itt[(Demographic.pretest.itt$birthYear >= 2000) |
                          (Demographic.pretest.itt$birthYear <= 1900),
                             "birthYear"] <-
  c('1967', NA, '2002', '1967', '2000', NA, '2000', '1982')

## Extract year as characters 12-16 from demograhpicDateStamp, which is 31 
## characters long and the same format for each subject. 

head(as.character(Demographic.pretest.itt$demographicDateStamp))
table(nchar(as.character(Demographic.pretest.itt$demographicDateStamp)))

Demographic.pretest.itt$demographicDateStampYear <-
  substr(as.character(Demographic.pretest.itt$demographicDateStamp), 12, 16)

table(Demographic.pretest.itt$demographicDateStampYear)

## Temporarily remove NA values from birthYear to compute age.

Demographic.pretest.itt.age <- 
  Demographic.pretest.itt[is.na(Demographic.pretest.itt$birthYear) == FALSE, ]

## Compute age as demographicDateStampYear - birthYear.

Demographic.pretest.itt.age$age <- 
  as.numeric(Demographic.pretest.itt.age$demographicDateStampYear) - 
  as.numeric(Demographic.pretest.itt.age$birthYear)

summary(Demographic.pretest.itt.age$age)
hist(Demographic.pretest.itt.age$age,
     main = "Histogram of Participants' Ages", xlab = 'Age')

head(sort(Demographic.pretest.itt.age$age))

## Compute age M and SD for all 956 non-NA subjects and in each condition.

mean(Demographic.pretest.itt.age$age)
sd(Demographic.pretest.itt.age$age)

round(tapply(Demographic.pretest.itt.age$age, 
             Demographic.pretest.itt.age$condition, mean), 2)
round(tapply(Demographic.pretest.itt.age$age, 
             Demographic.pretest.itt.age$condition, sd), 2)

## Add age to dataset with all 958 ITT subjects.

colnames(Demographic.pretest.itt.age)
Demographic.pretest.itt.age <- Demographic.pretest.itt.age[, c(1, 15)]

colnames(Demographic.pretest.itt)
Demographic.pretest.itt <- 
  merge(Demographic.pretest.itt, Demographic.pretest.itt.age,
        by = "participantId", all.x = TRUE)

Demographic.pretest.itt <- 
  Demographic.pretest.itt[order(Demographic.pretest.itt$participantId), ]

# View(Demographic.pretest.itt)

# Gender

## Reorder levels.

levels(Demographic.pretest.itt$genderId)
Demographic.pretest.itt$genderId <- 
  factor(Demographic.pretest.itt$genderId,
         levels(Demographic.pretest.itt$genderId)[c(1, 2, 5, 3, 6, 4)])
levels(Demographic.pretest.itt$genderId)

## Compute distribution overall and in each condition.

round(prop.table(table(Demographic.pretest.itt$genderId))*100, 1)

round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'POSITIVE_NEGATION', "genderId"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'POSITIVE', "genderId"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'FIFTY_FIFTY_BLOCKED', "genderId"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'FIFTY_FIFTY_RANDOM', "genderId"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'NEUTRAL', "genderId"]))*100, 1)

# Race

## Reorder levels.

levels(Demographic.pretest.itt$race)
Demographic.pretest.itt$race <- 
  factor(Demographic.pretest.itt$race,
         levels(Demographic.pretest.itt$race)[c(1, 2, 3, 4, 7, 8, 5, 6)])
levels(Demographic.pretest.itt$race)

## Compute distribution overall and in each condition.

round(prop.table(table(Demographic.pretest.itt$race))*100, 1)

round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'POSITIVE_NEGATION', "race"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'POSITIVE', "race"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'FIFTY_FIFTY_BLOCKED', "race"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'FIFTY_FIFTY_RANDOM', "race"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'NEUTRAL', "race"]))*100, 1)

# Ethnicity

## Reorder levels.

levels(Demographic.pretest.itt$ethnicity)
Demographic.pretest.itt$ethnicity <- 
  factor(Demographic.pretest.itt$ethnicity,
         levels(Demographic.pretest.itt$ethnicity)[c(1, 2, 4, 3)])
levels(Demographic.pretest.itt$ethnicity)

## Compute distribution overall and in each condition.

round(prop.table(table(Demographic.pretest.itt$ethnicity))*100, 1)

round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'POSITIVE_NEGATION', "ethnicity"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'POSITIVE', "ethnicity"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'FIFTY_FIFTY_BLOCKED', "ethnicity"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'FIFTY_FIFTY_RANDOM', "ethnicity"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'NEUTRAL', "ethnicity"]))*100, 1)

# Country

## Participants are from 39 countries.

table(Demographic.pretest.itt$country)

length(unique(Demographic.pretest.itt$country[Demographic.pretest.itt$country != 
                                                "NoAnswer"]))

## Find the top 5 countries.

sort(table(Demographic.pretest.itt$country), decreasing = TRUE)
sort(table(Demographic.pretest.itt$country), decreasing = TRUE)[1:5]

## Group levels not in the top 5 into an Other category and reorder.

levels(Demographic.pretest.itt$country)

Demographic.pretest.itt$countryGrp <- Demographic.pretest.itt$country
levels(Demographic.pretest.itt$countryGrp) <- c("Other", 
                                               "Australia",
                                               rep("Other", 2),
                                               "Canada",
                                               rep("Other", 22),
                                               "NoAnswer",
                                               rep("Other", 4),
                                               "Russian Federation",
                                               rep("Other", 7),
                                               "United Kingdom",
                                               "United States")
levels(Demographic.pretest.itt$countryGrp)

Demographic.pretest.itt$countryGrp <-
  factor(Demographic.pretest.itt$countryGrp,
         levels(Demographic.pretest.itt$countryGrp)[c(7, 6, 3, 2, 5, 1, 4)])

## Compute distribution overall and in each condition.

round(prop.table(table(Demographic.pretest.itt$countryGrp))*100, 1)

round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'POSITIVE_NEGATION', "countryGrp"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'POSITIVE', "countryGrp"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'FIFTY_FIFTY_BLOCKED', "countryGrp"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'FIFTY_FIFTY_RANDOM', "countryGrp"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'NEUTRAL', "countryGrp"]))*100, 1)

# Education

## Group levels and reorder.

levels(Demographic.pretest.itt$education)

Demographic.pretest.itt$educationGrp <- Demographic.pretest.itt$education
levels(Demographic.pretest.itt$educationGrp) <-
  c('Some college', 'College graduate', 'High school graduate', 'Advanced degree',
    'Some high school', 'Advanced degree', 'Advanced degree', 'Advanced degree',
    'Advanced degree', 'Advanced degree', 'Prefer not to answer', 'Some college', 
    'Some graduate school', 'Some high school', 'Unknown')
Demographic.pretest.itt$educationGrp <- 
  factor(Demographic.pretest.itt$educationGrp,
         levels(Demographic.pretest.itt$educationGrp)[c(5, 3, 1, 2, 7, 4, 8, 6)])
levels(Demographic.pretest.itt$educationGrp)

## Compute distribution overall and in each condition.

round(prop.table(table(Demographic.pretest.itt$educationGrp))*100, 1)

19.5 + 27.3 + 6.7 + 39.6 # At least some college
6.7 + 39.6               # At least some graduate school

round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'POSITIVE_NEGATION', "educationGrp"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'POSITIVE', "educationGrp"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'FIFTY_FIFTY_BLOCKED', "educationGrp"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'FIFTY_FIFTY_RANDOM', "educationGrp"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'NEUTRAL', "educationGrp"]))*100, 1)

# Employment

## Group levels and reorder.

levels(Demographic.pretest.itt$employmentStat)

Demographic.pretest.itt$employmentStatGrp <- Demographic.pretest.itt$employmentStat
levels(Demographic.pretest.itt$employmentStatGrp) <-
  c('Homemaker', 'Looking for work', 'Other', 'Prefer not to answer', 'Retired',
    'Student', 'Unemployed', 'Working full time', 'Working part time', 'Unknown')
levels(Demographic.pretest.itt$employmentStatGrp) <-
  c('Homemaker', 'Unemployed', 'Other', 'Prefer not to answer', 'Retired',
    'Student', 'Unemployed', 'Working full time', 'Working part time', 'Unknown')
Demographic.pretest.itt$employmentStatGrp <- 
  factor(Demographic.pretest.itt$employmentStatGrp,
         levels(Demographic.pretest.itt$employmentStatGrp)[c(6,1,2,8,7,5,3,9,4)])
levels(Demographic.pretest.itt$employmentStatGrp)

## Compute distribution overall and in each condition.

round(prop.table(table(Demographic.pretest.itt$employmentStatGrp))*100, 1)

round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'POSITIVE_NEGATION', "employmentStatGrp"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'POSITIVE', "employmentStatGrp"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'FIFTY_FIFTY_BLOCKED', "employmentStatGrp"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'FIFTY_FIFTY_RANDOM', "employmentStatGrp"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'NEUTRAL', "employmentStatGrp"]))*100, 1)

# Annual income

## Reorder levels.

levels(Demographic.pretest.itt$income)
Demographic.pretest.itt$income = 
  factor(Demographic.pretest.itt$income,
         levels(Demographic.pretest.itt$income)[c(13,9,2,4,6,8,10,11,1,3,5,7,12,14)])
levels(Demographic.pretest.itt$income)

## Compute distribution overall and in each condition.

table(Demographic.pretest.itt$income)
income.group.size <- summary(Demographic.pretest.itt$income)
round(prop.table(table(Demographic.pretest.itt$income))*100, 1)

4.0 + 3.0 + 2.1 + 4.6 + 6.5 + 8.4 # Less than $50,000
14.3 + 10.3                       # Between $50,000 and $100,000
15.9 + 6.2 + 3.3 + 6.3            # Greater than $100,000

round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'POSITIVE_NEGATION', "income"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'POSITIVE' , "income"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'FIFTY_FIFTY_BLOCKED', "income"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'FIFTY_FIFTY_RANDOM', "income"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'NEUTRAL', "income"]))*100, 1)

# Marital status

## Reorder levels.

levels(Demographic.pretest.itt$maritalStat)
levels(Demographic.pretest.itt$maritalStat) <- 
  c('Divorced', 'In a domestic/civil union', 'Married', 'Other', 'Prefer not to answer', 
    'Separated', 'Single', 'Dating', 'Engaged', 'In a marriage-like relationship', 
    'Widow/widower', 'Unknown')
Demographic.pretest.itt$maritalStat <- 
  factor(Demographic.pretest.itt$maritalStat, 
         levels(Demographic.pretest.itt$maritalStat)[c(7:10, 3, 2, 6, 1, 11, 4, 12, 5)])
levels(Demographic.pretest.itt$maritalStat)

## Compute distribution overall and in each condition.

round(prop.table(table(Demographic.pretest.itt$maritalStat))*100, 1)

8.1 + 2.7 + 7.6 + 44.2 + 1.1 # In a relationship
23.2                         # Single

round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'POSITIVE_NEGATION', "maritalStat"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'POSITIVE', "maritalStat"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'FIFTY_FIFTY_BLOCKED', "maritalStat"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'FIFTY_FIFTY_RANDOM', "maritalStat"]))*100, 1)
round(prop.table(table(Demographic.pretest.itt[
  Demographic.pretest.itt$condition == 'NEUTRAL', "maritalStat"]))*100, 1)

# Rearrange columns and save clean data file for ITT subjects.

colnames(Demographic.pretest.itt)

Demographic.pretest.itt <- 
  Demographic.pretest.itt[, c(1:5, 14:15, 6:10, 17, 11, 18, 12:13, 16)]

write.csv(Demographic.pretest.itt, "./Data/Clean/FTmainDataDemogITT.csv")