# ---------------------------------------------------------------------------- #
# Baseline Demographics Differences
# Authors: Jianhui Sun and Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Check R version, load packages, and set seed ----
# ---------------------------------------------------------------------------- #

script_R_version <- "R version 4.1.0 (2021-05-18)"
current_R_version <- R.Version()$version.string

if(current_R_version != script_R_version) {
  warning(paste0("This script is based on ", script_R_version,
                 ". You are running ", current_R_version, "."))
}

library(groundhog)
meta.groundhog("2021-07-01")
groundhog_day <- "2021-05-20"

groundhog.library(dplyr, groundhog_day)
groundhog.library(ggplot2, groundhog_day)
groundhog.library(car, groundhog_day)

# Note: plyr is loaded in "Income" section.

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/jp5ws/).

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

# Import demographic data for ITT subjects, and remove the X column, which are 
# just the row names at time of export to CSV and are redundant.

d <- read.csv("./Data/Clean/FTmainDataDemogITT.csv")
d$X <- NULL

# ---------------------------------------------------------------------------- #
# Age ----
# ---------------------------------------------------------------------------- #

# Age is not normally distributed, but homoscedasticity is met. Use one-way 
# ANOVA because it is robust to normality violations.

plot(lm(age ~ condition, data = d), which = 2)

shapiro.test(d[d$condition == "POSITIVE_NEGATION", ]$age)
shapiro.test(d[d$condition == "POSITIVE", ]$age)
shapiro.test(d[d$condition == "FIFTY_FIFTY_BLOCKED", ]$age)
shapiro.test(d[d$condition == "FIFTY_FIFTY_RANDOM", ]$age)
shapiro.test(d[d$condition == "NEUTRAL", ]$age)

hist(d[d$condition == "POSITIVE_NEGATION", ]$age)
hist(d[d$condition == "POSITIVE", ]$age)
hist(d[d$condition == "FIFTY_FIFTY_BLOCKED", ]$age)
hist(d[d$condition == "FIFTY_FIFTY_RANDOM", ]$age)
hist(d[d$condition == "NEUTRAL", ]$age)

leveneTest(age ~ condition, data = d)

anova(lm(age ~ condition, data = d))

# For sensitivity analysis given normality violation, use Kruskal-Wallis H test.
# Distributions at different levels look similar in shape.

ggplot(d, aes(x = age)) + geom_histogram() + facet_grid(~ condition) + theme_bw()

ggplot(d, aes(age)) +
  geom_bar(aes(y = (..count..) / tapply(..count.., ..PANEL.., sum)[..PANEL..])) +
  facet_grid(~ condition) + theme_bw()

kruskal.test(d$age ~ d$condition)

# ---------------------------------------------------------------------------- #
# Exclusions from other variables ----
# ---------------------------------------------------------------------------- #

# For each variable below, group "Unknown" and "Prefer not to answer" because 
# they have overlapping possibilities (i.e., the presented options and "Other") 
# and exclude them from the analysis (treat them as NA).

# ---------------------------------------------------------------------------- #
# Gender ----
# ---------------------------------------------------------------------------- #

summary(d$genderId)

d$genderIdTemp <- 
  recode(d$genderId, 
         "c('Unknown', 'Prefer not to answer') = NA")

summary(d$genderIdTemp)

# Use Fisher's exact test instead of chi-square given expected values of less
# than 5 in many cells. Using many simulations yields p-values consistent to 
# two decimal places. Note: Analysis may take up to 1 min to run.

gender.table <- table(d$genderIdTemp, d$condition)

gender.chi <- chisq.test(gender.table)
gender.chi$expected

fisher.test(gender.table, simulate.p.value = TRUE, B = 10000000)

# ---------------------------------------------------------------------------- #
# Race ----
# ---------------------------------------------------------------------------- #

summary(d$race)

d$raceTemp <- 
  recode(d$race, 
         "c('Prefer not to answer') = NA")

summary(d$raceTemp)

# Use Fisher's exact test instead of chi-square given expected values of less
# than 5 in many cells. Using many simulations yields p-values consistent to 
# two decimal places. Note: Analysis may take up to 1 min to run.

race.table <- table(d$raceTemp, d$condition)

race.chi <- chisq.test(race.table)
race.chi$expected

fisher.test(race.table, simulate.p.value = TRUE, B = 10000000)

# ---------------------------------------------------------------------------- #
# Ethnicity ----
# ---------------------------------------------------------------------------- #

summary(d$ethnicity)

d$ethnicityTemp <- 
  recode(d$ethnicity, 
         "c('Unknown', 'Prefer not to answer') = NA")

summary(d$ethnicityTemp)

# Use chi-square as expected values are greater than 5.

ethnicity.table <- table(d$ethnicityTemp, d$condition)

ethnicity.chi <- chisq.test(ethnicity.table)
ethnicity.chi$expected
ethnicity.chi

# ---------------------------------------------------------------------------- #
# Country ----
# ---------------------------------------------------------------------------- #

summary(d$country)

d$countryTemp <- 
  recode(d$country, 
         "c('Antarctica', 'NoAnswer') = NA")

summary(d$countryTemp)

# Use Fisher's exact test instead of chi-square given expected values of less
# than 5 in many cells. Using many simulations yields p-values consistent to 
# two decimal places. Note: Analysis may take up to 1 min to run.

country.table <- table(d$countryTemp, d$condition)

country.chi <- chisq.test(country.table)
country.chi$expected

fisher.test(country.table, simulate.p.value = TRUE, B = 10000000)

# ---------------------------------------------------------------------------- #
# Employment ----
# ---------------------------------------------------------------------------- #

summary(d$employmentStat)

d$employmentStatTemp <- 
  recode(d$employmentStat, 
         "c('Unknown', 'Prefer not to answer') = NA")

summary(d$employmentStatTemp)

# Use chi-square as expected values are greater than 5.

employmentStat.table <- table(d$employmentStatTemp, d$condition)

employmentStat.chi <- chisq.test(employmentStat.table)
employmentStat.chi$expected
employmentStat.chi

# ---------------------------------------------------------------------------- #
# Marital status ----
# ---------------------------------------------------------------------------- #

summary(d$maritalStat)

d$maritalStatTemp <- 
  recode(d$maritalStat, 
         "c('Unknown', 'Prefer not to answer') = NA")

summary(d$maritalStatTemp)

# Use Fisher's exact test instead of chi-square given expected values of less
# than 5 in many cells. Using many simulations yields p-values consistent to 
# two decimal places. Note: Analysis may take up to 1 min to run.

maritalStat.table <- table(d$maritalStatTemp, d$condition)

maritalStat.chi <- chisq.test(maritalStat.table)
maritalStat.chi$expected

fisher.test(maritalStat.table, simulate.p.value = TRUE, B = 10000000)

# ---------------------------------------------------------------------------- #
# Education ----
# ---------------------------------------------------------------------------- #

# To simplify conceptual overlap for creating an ordered variable, group
# "Elementary School," "Junior High," and "Some High School" into "Not High School
# Graduate"; group "Associate's Degree" and "Some College" into "Some College"; 
# rename "Bachelor's Degree" as "College Graduate"; and group "J.D.," "M.B.A.," 
# "M.D.," "Master's Degree," "Other Advanced Degree," and "Ph.D." into 
# "Advanced degree."

summary(d$education)
summary(d$educationGrp)

d$educationGrpNew <- 
  recode(d$educationGrp, 
         "c('No high school', 'Some high school') = 'Not high school graduate';
         c('Unknown', 'Prefer not to answer') = NA")

summary(d$educationGrpNew)

d$educationGrpNew2 <- ordered(d$educationGrpNew,
                              levels = c("Not high school graduate",
                                         "High school graduate",
                                         "Some college",
                                         "College graduate",
                                         "Some graduate school",
                                         "Advanced degree"))

summary(d$educationGrpNew2)

# Distributions at different levels look similar in shape.

ggplot(d, aes(educationGrpNew2)) +
  geom_bar(aes(y = (..count..) / tapply(..count.., ..PANEL.., sum)[..PANEL..])) +
  facet_grid(~ condition) + theme_bw()

# For more than two groups, use Kruskal-Wallis H test.

kruskal.test(d$educationGrpNew2, d$condition)

# ---------------------------------------------------------------------------- #
# Income ----
# ---------------------------------------------------------------------------- #

summary(d$income)

groundhog.library(plyr, groundhog_day)

d$incomeNew <- 
  revalue(d$income, 
          c("Don't know" = NA, "Prefer not to answer" = NA))

summary(d$incomeNew)

d$incomeNew2 <- ordered(d$incomeNew,
                        levels = c("Less than $5,000",
                                   "$5,000 through $11,999",
                                   "$12,000 through $15,999",
                                   "$16,000 through $24,999",
                                   "$25,000 through $34,999",
                                   "$35,000 through $49,999",
                                   "$50,000 through $74,999",
                                   "$75,000 through $99,999",
                                   "$100,000 through $149,999",
                                   "$150,000 through $199,999",
                                   "$200,000 through $249,999",
                                   "$250,000 or greater"))

summary(d$incomeNew2)

# Distributions at different levels look similar in shape.

ggplot(d, aes(incomeNew2)) +
  geom_bar(aes(y = (..count..) / tapply(..count.., ..PANEL.., sum)[..PANEL..])) +
  facet_grid(~ condition) + theme_bw()

# For more than two groups, use Kruskal-Wallis H test.

kruskal.test(d$incomeNew2, d$condition)