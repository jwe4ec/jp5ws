# ---------------------------------------------------------------------------- #
# Table S1, Scale-Level Missingness, Auxiliary Variables, Multiple Imputation (MI)
# Authors: Mehdi Boukhechba and Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Load packages and set seed ----
# ---------------------------------------------------------------------------- #

library(groundhog)
groundhog_day <- "2020-07-12"

groundhog.library(data.table, groundhog_day)
groundhog.library(plyr, groundhog_day)
groundhog.library(dplyr, groundhog_day)
groundhog.library(DescTools, groundhog_day)
groundhog.library(ggplot2, groundhog_day)
groundhog.library(car, groundhog_day)
groundhog.library(psych, groundhog_day)

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Define preparedata function ----
# ---------------------------------------------------------------------------- #

# Define a function for preparing data.

preparedata <- function(x) {
  
  # Combine Eligibility and preTest rows into a row called Baseline.
  
  x$session <- gsub('Eligibility', 'Baseline', x$session)
  x$session <- gsub('preTest', 'Baseline', x$session)
  x$session <- factor(x$session, levels = c("Baseline", "firstSession",
                                            "secondSession", "thirdSession",
                                            "fourthSession", "PostFollowUp"))
  x <- aggregate(
    x = x[c("posExpBiasScale", "negExpBiasScale", "depressionScale",
            "anxietyScale", "selfEffScale", "growthMindScale", "optimismScale")],
    by = list(participantId = x$participantId,
              session = x$session,
              condition = x$condition),
    mean, na.rm = TRUE
  )
  
  # Replace NaN with NA.
  
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  x[is.nan(x)] <- NA
  
  # Sort by participantId and session.
  
  x <- x[order(x$participantId, x$session), ]
  
  # Compute session_int.
  
  x$session_int <- as.integer(x$session)
  x$session_int <- x$session_int - 1
  
  return(x)
  
}

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/jp5ws/).

# ---------------------------------------------------------------------------- #
# Import and prepare data ----
# ---------------------------------------------------------------------------- #

# Import demographic data for ITT subjects, and remove the X column, which are 
# just the row names at time of export to Excel and are redundant.

d <- read.csv("./Data/Clean/FTmainDataDemogITT.csv")
d$X <- NULL

# Import outcome data, remove the X column, use preparedata function above to 
# combine Eligibility and preTest into Baseline, and use sample data to restrict 
# outcome data to ITT subjects.

x <- read.csv("./Data/Clean/FTmainDataScales.csv")
x$X <- NULL

x <- preparedata(x)

p <- read.csv("./Data/Clean/FTmainAnalysisSamples.csv")
p$X <- NULL
ITT <- subset(p, ittSample == 1)
Completers <- subset(p, txCompSample == 1)

ittx <- x[which(x$participantId %in% ITT$participantId), ]

rownames(ittx) <- 1:nrow(ittx)

# Merge demographic data with outcome data.

total <- merge(d, ittx, by = c("participantId", "condition"))
total <- total[order(total$participantId, total$session_int), ]

# ---------------------------------------------------------------------------- #
# Define compute function ----
# ---------------------------------------------------------------------------- #

# Define function for computing n, M, and SD for each outcome by condition at 
# each time point.

compute <- function(outcome){
  
  fml <- as.formula(paste0(outcome, "~ session_int + condition"))
  ag <- aggregate(fml, data = tbldata,
                  FUN = function(tbldata) c(n = length(tbldata), 
                                            mean = mean(tbldata), 
                                            sd = sd(tbldata)))
  df <- data.frame("session_int" = ag[, 1], "condition" = ag[, 2], ag[, 3])
  ag.wide <- dcast(setDT(df), session_int ~ condition, 
                   value.var = c("n", "mean", "sd"))
  ag.wide <- data.frame(outcome, ag.wide)
  
}

# ---------------------------------------------------------------------------- #
# Generate Table S1 ----
# ---------------------------------------------------------------------------- #

# Refactor condition for the table.

tbldata <- ittx

levels(tbldata$condition)

tbldata$condition <- 
  factor(tbldata$condition, 
         levels = c("POSITIVE_NEGATION", "POSITIVE", "FIFTY_FIFTY_BLOCKED", 
                    "FIFTY_FIFTY_RANDOM", "NEUTRAL"))

# Compute n, M, and SD for each outcome by condition at each time point.

ag.wide.posExpBiasScale <- compute("posExpBiasScale")
ag.wide.negExpBiasScale <- compute("negExpBiasScale")
ag.wide.anxietyScale    <- compute("anxietyScale")
ag.wide.depressionScale <- compute("depressionScale")
ag.wide.selfEffScale    <- compute("selfEffScale")
ag.wide.growthMindScale <- compute("growthMindScale")
ag.wide.optimismScale   <- compute("optimismScale")

# Build table.

ag.wide.all <- rbind(ag.wide.posExpBiasScale,
                     ag.wide.negExpBiasScale,
                     ag.wide.anxietyScale,
                     ag.wide.depressionScale,
                     ag.wide.selfEffScale,
                     ag.wide.growthMindScale,
                     ag.wide.optimismScale)

table <- ag.wide.all[c(1:2,
                       3, 8, 13,
                       4, 9, 14,
                       5, 10, 15,
                       6, 11, 16,
                       7, 12, 17)]

# Note that 1 subject in NEUTRAL refused to answer all self-efficacy items at 
# baseline and that 1 subject in POSITIVE refused to answer all self-efficacy, 
# growth mindset, and optimism items at baseline. This is why there is one
# fewer observation in certain cells than others. See below.

# View(table[table$session_int == 0, ])

# Round to two decimal places.

tableFinal <- table %>% mutate_at(vars(starts_with("mean")), list(~round(., 2)))
tableFinal <- tableFinal %>% mutate_at(vars(starts_with("sd")), list(~round(., 2)))

View(tableFinal)

write.csv(tableFinal, file = "./Results/Table S1/table_s1.csv")

# ---------------------------------------------------------------------------- #
# Compute proportions of scale-level missing data ----
# ---------------------------------------------------------------------------- #

sum(ag.wide.posExpBiasScale[3:7])/(958*6)
sum(ag.wide.negExpBiasScale[3:7])/(958*6)
sum(ag.wide.anxietyScale[3:7])/(958*4)
sum(ag.wide.depressionScale[3:7])/(958*4)
sum(ag.wide.selfEffScale[3:7])/(958*4)
sum(ag.wide.growthMindScale[3:7])/(958*4)
sum(ag.wide.optimismScale[3:7])/(958*4)

# ---------------------------------------------------------------------------- #
# Prepare data for tests of auxiliary variables ----
# ---------------------------------------------------------------------------- #

# Create missing data indicators for outcome variables.

total$posInd <- as.integer(as.logical(is.na(total$posExpBiasScale)))
total$negInd <- as.integer(as.logical(is.na(total$negExpBiasScale)))
total$depInd <- as.integer(as.logical(is.na(total$depressionScale)))
total$anxInd <- as.integer(as.logical(is.na(total$anxietyScale)))
total$selfInd <- as.integer(as.logical(is.na(total$selfEffScale)))
total$growInd <- as.integer(as.logical(is.na(total$growthMindScale)))
total$optInd <- as.integer(as.logical(is.na(total$optimismScale)))

# Compute number of missing observations of posExpBiasScale per subject as a 
# proxy for number of missing sessions.

## Note that 2 subjects (528, 744) have missing age.

unique(total[is.na(total$age), ]$participantId)

## Compute number and add it to the data frame.

temp <- aggregate(posInd ~ participantId, data = total, FUN = sum)
temp$posIndSum <- temp$posInd
temp$posInd <- NULL

total2 <- merge(total, temp, by = "participantId", all.x = TRUE)
total2 <- total2[order(total2$participantId, total2$session_int), ]

## Note that posIndSum is not even roughly normally distributed. Thus, we need
## to use nonparametric tests.

## Restrict to baseline.

total2baseline <- total2[total2$session_int == 0, ]
total2baseline <- total2baseline[, c(1:2, 27, 4:18, 35)]

hist(total2baseline$posIndSum)
shapiro.test(total2baseline$posIndSum)

## Add confidence items.

credItems <- read.csv("./Data/Clean/FTmainDataItemsScales.csv")
credItems$X <- NULL

credItems <- 
  credItems[credItems$session == "preTest", 
            c("participantId", "session", "confident_design",
              "confident_online", "important")]

total2baseline <- merge(total2baseline, credItems, 
                        by = "participantId", all.x = TRUE)

total2baseline$session <- NULL

## Assess correlation between confident_design and confident_online items. As
## they are not normally distributed, use nonparametric tests. 

hist(total2baseline$confident_design)
shapiro.test(total2baseline$confident_design)

hist(total2baseline$confident_online)
shapiro.test(total2baseline$confident_online)

### Use Goodman & Kruskal's gamma because confident_online and confident_online
### have many tied ranks. See
### https://statistics.laerd.com/spss-tutorials/goodman-and-kruskals-gamma-using-spss-statistics.php

cor.test(total2baseline$confident_design, total2baseline$confident_online, 
         method = c("pearson"))

plot(total2baseline$confident_design, total2baseline$confident_online)
abline(lm(total2baseline$confident_online ~ total2baseline$confident_design))

GoodmanKruskalGamma(total2baseline$confident_online, 
                    total2baseline$confident_design, conf.level = .95)

## Compute mean of confident_design and confident_online items because they are
## highly correlated and this is how Hohensee et al. (2020) analyzed them,
## finding that their mean predicted dropout. Use mean of available items for
## item-level missing data.

# sum(is.na(total2baseline$confident_design))
# sum(is.na(total2baseline$confident_online))

total2baseline$confidentScale <- 
  rowMeans(total2baseline[ , c("confident_design", "confident_online")], 
           na.rm = TRUE)

# sum(!is.na(total2baseline$confidentScale))
# sum(is.nan(total2baseline$confidentScale))
# which(is.nan(total2baseline$confidentScale))

# ---------------------------------------------------------------------------- #
# Test for auxiliary confidence and change importance variables ----
# ---------------------------------------------------------------------------- #

# Test whether confidentScale and important predict missingness.

## confidentScale

### Use Goodman & Kruskal's gamma because posIndSum and confidentScale have 
### many tied ranks. See
### https://statistics.laerd.com/spss-tutorials/goodman-and-kruskals-gamma-using-spss-statistics.php

cor.test(total2baseline$posIndSum, total2baseline$confidentScale, 
         method = c("pearson"))

plot(total2baseline$confidentScale, total2baseline$posIndSum)
abline(lm(total2baseline$posIndSum ~ total2baseline$confidentScale))

GoodmanKruskalGamma(total2baseline$posIndSum, 
                    total2baseline$confidentScale, conf.level = .95)

## important

### important is not even roughly normally distributed. Use nonparametric tests.

hist(total2baseline$important)
shapiro.test(total2baseline$important)

### Use Goodman & Kruskal's gamma because posIndSum and important have 
### many tied ranks. See
### https://statistics.laerd.com/spss-tutorials/goodman-and-kruskals-gamma-using-spss-statistics.php

cor.test(total2baseline$posIndSum, total2baseline$important, 
         method = c("pearson"))

plot(total2baseline$important, total2baseline$posIndSum)
abline(lm(total2baseline$posIndSum ~ total2baseline$important))

GoodmanKruskalGamma(total2baseline$posIndSum, 
                    total2baseline$important, conf.level = .95)

# ---------------------------------------------------------------------------- #
# Test for auxiliary demographic variables ----
# ---------------------------------------------------------------------------- #

# Test whether demographic variables predict missingness.

## Age

### Use Goodman & Kruskal's gamma because posIndSum has many tied ranks. See
### https://statistics.laerd.com/spss-tutorials/goodman-and-kruskals-gamma-using-spss-statistics.php

cor.test(total2baseline$posIndSum, total2baseline$age, method = c("pearson"))

plot(total2baseline$age, total2baseline$posIndSum)
abline(lm(total2baseline$posIndSum ~ total2baseline$age))

GoodmanKruskalGamma(total2baseline$posIndSum, total2baseline$age, conf.level = .95)

## Other demographic variables

### First, group "Unknown" and "Prefer not to answer" because they have 
### overlapping possibilities (i.e., the given options and "Other") and exclude 
### them from the analysis (treat them as NA). Morover, given that some levels 
### have insufficient sample sizes for Kruskal-Wallis H test (i.e., < 5; see
### https://support.minitab.com/en-us/minitab-express/1/help-and-how-to/modeling-statistics/anova/how-to/kruskal-wallis-test/before-you-start/data-considerations/),
### group given levels with < 5 subjects with "Other" and include this new 
### grouping in the analysis if it has >= 5 subjects; otherwise, exclude the 
### grouping from the analysis. If given levels have >= 5 subjects, analyze 
### them separate from "Other"; include "Other" only if it has >= 5 subjects. 
### Also consider whether the shapes of the distributions of posIndSum for each 
### group of subjects are similar (see
### https://support.minitab.com/en-us/minitab-express/1/help-and-how-to/modeling-statistics/anova/how-to/kruskal-wallis-test/before-you-start/data-considerations/).

### Gender. Per above guidelines, analyze only Female and Male.

summary(total2baseline$genderId)

total2baseline$genderIdNew <- 
  recode(total2baseline$genderId, 
         "c('Transgender', 'Other', 'Unknown', 'Prefer not to answer') = NA")

summary(total2baseline$genderIdNew)

#### Distributions at different levels look similar in shape.

ggplot(total2baseline,
       aes(x = posIndSum)) + geom_histogram() + facet_grid(~ genderIdNew) + theme_bw()

ggplot(total2baseline, aes(posIndSum)) +
  geom_bar(aes(y = (..count..) / tapply(..count.., ..PANEL.., sum)[..PANEL..])) +
  facet_grid( ~ genderIdNew) + theme_bw()

#### For two groups, use Mann-Whitney U test.

anova(lm(total2baseline$posIndSum ~ total2baseline$genderIdNew))

wilcox.test(total2baseline$posIndSum ~ total2baseline$genderIdNew)

### Race

#### Per above guidelines, combine "American Indian/Alaska Native" and 
#### "Native Hawaiian/Pacific Islander" with "Other or Unknown." A limitation of
#### this analysis is that these groups and Other are conflated with Unknown.

summary(total2baseline$race)

total2baseline$raceNew <- 
  recode(total2baseline$race, 
         "c('American Indian/Alaska Native',
            'Native Hawaiian/Pacific Islander') = 'Other or Unknown';
            'Prefer not to answer' = NA")

summary(total2baseline$raceNew)

#### Distributions at different levels look similar in shape.

ggplot(total2baseline, 
       aes(x = posIndSum)) + geom_histogram() + facet_grid(~ raceNew) + theme_bw()

ggplot(total2baseline, aes(posIndSum)) +
  geom_bar(aes(y = (..count..) / tapply(..count.., ..PANEL.., sum)[..PANEL..])) +
  facet_grid(~ raceNew) + theme_bw()

#### For more than two groups, use Kruskal-Wallis H test.

anova(lm(total2baseline$posIndSum ~ total2baseline$raceNew))

kruskal.test(total2baseline$posIndSum, total2baseline$raceNew) 

### Ethnicity.

summary(total2baseline$ethnicity)

total2baseline$ethnicityNew <- 
  recode(total2baseline$ethnicity, 
         "c('Unknown', 'Prefer not to answer') = NA")

summary(total2baseline$ethnicityNew)

#### Distributions at different levels look similar in shape.

ggplot(total2baseline, 
       aes(x = posIndSum)) + geom_histogram() + facet_grid(~ ethnicityNew) + theme_bw()

ggplot(total2baseline, aes(posIndSum)) +
  geom_bar(aes(y = (..count..) / tapply(..count.., ..PANEL.., sum)[..PANEL..])) +
  facet_grid(~ ethnicityNew) + theme_bw()

#### For two groups, use Mann-Whitney U test.

anova(lm(total2baseline$posIndSum ~ total2baseline$ethnicityNew))

wilcox.test(total2baseline$posIndSum ~ total2baseline$ethnicityNew)

### Marital status.

#### Per above guidelines, no grouping other than "Unknown" versus "Prefer not 
#### to answer" is needed.

summary(total2baseline$maritalStat)

total2baseline$maritalStatNew <- 
  recode(total2baseline$maritalStat, 
         "c('Unknown', 'Prefer not to answer') = NA")

summary(total2baseline$maritalStatNew)

#### Distributions at different levels look similar in shape.

ggplot(total2baseline, 
       aes(x = posIndSum)) + geom_histogram() + facet_grid(~ maritalStatNew) + theme_bw()

ggplot(total2baseline, aes(posIndSum)) +
  geom_bar(aes(y = (..count..) / tapply(..count.., ..PANEL.., sum)[..PANEL..])) +
  facet_grid(~ maritalStatNew) + theme_bw()

#### For more than two groups, use Kruskal-Wallis H test.

anova(lm(total2baseline$posIndSum ~ total2baseline$maritalStatNew))

kruskal.test(total2baseline$posIndSum, total2baseline$maritalStatNew)

### Education

#### To address small sample sizes and simplify conceptual overlap, group
#### "Elementary School," "Junior High," and "Some High School" into "Not High 
#### School Graduate." To address conceptual overlap, group "Associate's Degree" 
#### and "Some College" into "Some College"; rename "Bachelor's Degree" as 
#### "College Graduate"; and group "J.D.," "M.B.A.," "M.D.," "Master's Degree," 
#### "Other Advanced Degree," and "Ph.D."

summary(total2baseline$education)
summary(total2baseline$educationGrp)

total2baseline$educationGrpNew <- 
  recode(total2baseline$educationGrp, 
         "c('No high school', 'Some high school') = 'Not high school graduate';
          c('Unknown', 'Prefer not to answer') = NA")

summary(total2baseline$educationGrpNew)

total2baseline$educationGrpNew2 <- ordered(total2baseline$educationGrpNew,
                                           levels = c("Not high school graduate",
                                                      "High school graduate",
                                                      "Some college",
                                                      "College graduate",
                                                      "Some graduate school",
                                                      "Advanced degree"))

summary(total2baseline$educationGrpNew2)

#### Distributions at different levels look similar in shape.

ggplot(total2baseline, 
       aes(x = posIndSum)) + geom_histogram() + facet_grid(~ educationGrpNew2) + theme_bw()

ggplot(total2baseline, aes(posIndSum)) +
  geom_bar(aes(y = (..count..) / tapply(..count.., ..PANEL.., sum)[..PANEL..])) +
  facet_grid(~ educationGrpNew2) + theme_bw()

#### For more than two ordered groups, use Jonckheere-Terpstra test.

anova(lm(total2baseline$posIndSum ~ total2baseline$educationGrpNew2))

plot(total2baseline$educationGrpNew2, total2baseline$posIndSum)
abline(lm(total2baseline$posIndSum ~ as.numeric(total2baseline$educationGrpNew2)))

plot(as.numeric(total2baseline$educationGrpNew2), total2baseline$posIndSum)
abline(lm(total2baseline$posIndSum ~ as.numeric(total2baseline$educationGrpNew2)))

JonckheereTerpstraTest(total2baseline$posIndSum, total2baseline$educationGrpNew2, 
                       alternative = "two.sided", nperm = 1000)

describeBy(total2baseline$posIndSum, total2baseline$educationGrpNew2)

#### Assess relationship between age and education. Use Goodman & Kruskal's 
#### gamma because educationGrpNew2 has many tied ranks. See
#### https://statistics.laerd.com/spss-tutorials/goodman-and-kruskals-gamma-using-spss-statistics.php

cor.test(total2baseline$age, as.numeric(total2baseline$educationGrpNew2,
                                        method = "pearson"))

plot(total2baseline$age, total2baseline$educationGrpNew2)
abline(lm(as.numeric(total2baseline$educationGrpNew2) ~ total2baseline$age))

GoodmanKruskalGamma(total2baseline$educationGrpNew2, total2baseline$age, 
                    conf.level = .95)

#### Pearson correlation has small (r = .10) to medium (r = .30) effect size. A
#### large effect wize would be r = .50.

### Employment

#### Per above guidelines, no grouping other than "Unknown" versus "Prefer not 
#### to answer" is needed.

summary(total2baseline$employmentStat)
summary(total2baseline$employmentStatGrp) # Not used here

total2baseline$employmentStatNew <- 
  recode(total2baseline$employmentStat, 
         "c('Unknown', 'Prefer not to answer') = NA")

summary(total2baseline$employmentStatNew)

#### Distributions at different levels look similar in shape.

ggplot(total2baseline, 
       aes(x = posIndSum)) + geom_histogram() + facet_grid(~ employmentStatNew) + theme_bw()

ggplot(total2baseline, aes(posIndSum)) +
  geom_bar(aes(y = (..count..) / tapply(..count.., ..PANEL.., sum)[..PANEL..])) +
  facet_grid(~ employmentStatNew) + theme_bw()

#### For more than two groups, use Kruskal-Wallis H test.

anova(lm(total2baseline$posIndSum ~ total2baseline$employmentStatNew))

kruskal.test(total2baseline$posIndSum, total2baseline$employmentStatNew)

### Income

#### Per above guidelines, no grouping other than "Don't know" versus "Prefer 
#### not to answer" is needed.

summary(total2baseline$income)

total2baseline$incomeNew <- 
  revalue(total2baseline$income, 
          c("Don't know" = NA, "Prefer not to answer" = NA))

summary(total2baseline$incomeNew)

total2baseline$incomeNew2 <- ordered(total2baseline$incomeNew,
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

summary(total2baseline$incomeNew2)

#### Distributions at different levels look similar in shape.

ggplot(total2baseline, 
       aes(x = posIndSum)) + geom_histogram() + facet_grid(~ incomeNew2) + theme_bw()

ggplot(total2baseline, aes(posIndSum)) +
  geom_bar(aes(y = (..count..) / tapply(..count.., ..PANEL.., sum)[..PANEL..])) +
  facet_grid(~ incomeNew2) + theme_bw()

#### For more than two ordered groups, use Jonckheere-Terpstra test.

anova(lm(total2baseline$posIndSum ~ total2baseline$incomeNew2))

JonckheereTerpstraTest(total2baseline$posIndSum, total2baseline$incomeNew2, 
                       alternative = "two.sided", nperm = 1000)

### Country

#### Group countries per "6b_country_groupings.xlsx" so each Continental Region 
#### or Sub-Region has at least 5 subjects. Prefer Sub-Regions over Continental 
#### Regions when Sub-Regions have at least 5 subjects. Continental Regions and
#### Sub-Regions were obtained from https://unstats.un.org/unsd/methodology/m49/

summary(total2baseline$country)
summary(total2baseline$countryGrp) # Not used here

total2baseline$countryNew <- 
  recode(total2baseline$country, 
         "'South Africa'                = 'Sub-Saharan Africa';
          c('Puerto Rico', 'Mexico', 'Panama', 'Brazil', 'Colombia', 'Ecuador', 
            'Guyana')                   = 'Latin American and the Caribbean';
          c('Canada', 'United States')  = 'Northern America';
          c('Japan', 'Malaysia', 'India', 'Kuwait', 'Turkey')
                                        = 'Asia';
          c('Czech Republic', 'Moldova, Republic of', 'Romania', 
            'Russian Federation', 'Slovakia', 'Ukraine')
                                        = 'Eastern Europe';
          c('Denmark', 'Estonia', 'Ireland', 'Norway', 'Sweden', 
            'United Kingdom')           = 'Northern Europe';
          c('Croatia', 'Gibraltar', 'Greece', 'Italy', 'Spain')
                                        = 'Southern Europe';
          c('France', 'Germany', 'Netherlands', 'Switzerland')
                                        = 'Western Europe';
          c('Australia', 'New Zealand') = 'Australia and New Zealand';
          c('Antarctica', 'NoAnswer')   = NA")

summary(total2baseline$countryNew)

#### Distributions at different levels look similar in shape.

ggplot(total2baseline, 
       aes(x = posIndSum)) + geom_histogram() + facet_grid(~ countryNew) + theme_bw()

ggplot(total2baseline, aes(posIndSum)) +
  geom_bar(aes(y = (..count..) / tapply(..count.., ..PANEL.., sum)[..PANEL..])) +
  facet_grid(~ countryNew) + theme_bw()

#### For more than two groups, use Kruskal-Wallis H test.

anova(lm(total2baseline$posIndSum ~ total2baseline$countryNew))

kruskal.test(total2baseline$posIndSum, total2baseline$countryNew)

# Merge auxiliary demographic variables (age, educationGrpNew2) with outcome data.

aux <- total2baseline[, c(1:2, 7, 29)]

final <- merge(aux, ittx, by = c("participantId", "condition"))
final <- final[order(final$participantId, final$session_int), ]

# Make NEUTRAL condition the reference group.

table(final$condition)

final$condition <- factor(final$condition, 
                          levels = c("NEUTRAL", "POSITIVE_NEGATION", "POSITIVE",
                                     "FIFTY_FIFTY_BLOCKED", "FIFTY_FIFTY_RANDOM"))

# ---------------------------------------------------------------------------- #
# Save intermediate data ----
# ---------------------------------------------------------------------------- #

save(final, file = "./Data/Clean/final.RData")

# ---------------------------------------------------------------------------- #
# Define histbytime function ----
# ---------------------------------------------------------------------------- #

# Define function for investigating distributions of data at each time point 
# before imputation.

histbytime <- function(data, path, outcome, xmin, xmax, ymin, ymax, scalemin, scalemax) {
  x <- data
  
  pdf(paste0(path, ".pdf"), width = 9, height = 7)
  
  par(new = FALSE)
  par(mfrow = c(2, 3))
  
  for (k in unique(x$session_int)) {
    hist(x[x$session_int == k, outcome],
         main = paste0("session_int = ", k),
         breaks = c(xmin:xmax),
         right = FALSE,
         xlim = range(xmin, xmax),
         ylim = range(ymin, ymax),
         xlab = outcome)
    
    abline(v = scalemin)
    abline(v = scalemax)
    par(new = FALSE)
  }
  dev.off()
  par(mfrow = c(1,1))
}

# ---------------------------------------------------------------------------- #
# Investigate distributions of data at each time point before imputation ----
# ---------------------------------------------------------------------------- #

histbytime(final, './Results/Imputation/histograms before imputation/posExpBiasScale', 
           "posExpBiasScale", -5, 13, 0, 400, 1, 7)
histbytime(final, './Results/Imputation/histograms before imputation/negExpBiasScale', 
           "negExpBiasScale", -5, 13, 0, 400, 1, 7)
histbytime(final, './Results/Imputation/histograms before imputation/depressionScale', 
           "depressionScale", -9, 12, 0, 400, 0, 6)
histbytime(final, './Results/Imputation/histograms before imputation/anxietyScale', 
           "anxietyScale", -9, 12, 0, 400, 0, 6)
histbytime(final, './Results/Imputation/histograms before imputation/selfEffScale', 
           "selfEffScale", -4, 8, 0, 400, 0, 4)
histbytime(final, './Results/Imputation/histograms before imputation/growthMindScale', 
           "growthMindScale", -4, 8, 0, 400, 0, 4)
histbytime(final, './Results/Imputation/histograms before imputation/optimismScale', 
           "optimismScale", -4, 8, 0, 400, 0, 4)