# ---------------------------------------------------------------------------- #
# Cleaning Data - Multiple Entry Decisions
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# This script is a companion to "1a_cleaning_data.R" and depends on packages
# loaded and objects defined in that script. The present script documents the 
# decisions for cleaning multiple entries implemented in "1a_cleaning_data.R".

# ---------------------------------------------------------------------------- #
# Investigate multiple entries ----
# ---------------------------------------------------------------------------- #

# There are 99 participants with multiple entries in taskLogTbl2.

taskLogTbl2multipleIDs

# 49   56   91   99  125  130  137  138  146  178  186  194  200  223  228  239  
# 261  308 352  369  392  396  408  412  434  439  446  453  455  464  469  495  
# 496  514  517  533 565  577  582  584  594  607  627  634  659  662  683  711  
# 732  734  757  758  760  782 788  814  831  834  852  860  862  868  874  879  
# 883  895  916  934  942  943  947  950 961  969  971  986  992 1025 1031 1034 
# 1055 1061 1066 1068 1094 1112 1130 1135 1138 1141 1160 1162 1165 1173 1184 
# 1186 1235 1245 1271

length(taskLogTbl2multipleIDs)

# Use this to search relevant tables for participants.

# View(taskLogTbl2[which(taskLogTbl2$participantId == 137), ])
# View(credibilityTbl2[which(credibilityTbl2$participantRSA == 868), ])
# View(expectancyBiasTbl2[which(expectancyBiasTbl2$participant == 879), ])
# View(demographicTbl2[which(demographicTbl2$participantRSA == 734), ])
# View(phq4Tbl2[which(phq4Tbl2$participantRSA == 879), ])
# View(whatIBelieveTbl2[which(whatIBelieveTbl2$participantRSA == 879), ])
# View(mentalHealthHxTbl2[which(mentalHealthHxTbl2$participantRSA == 734), ])

# The 76 subjects with multiple entries in the tables below are also reflected 
# as having multiple entries in taskLogTbl2.

length(unique(randomizeTbl2multipleIDs))
length(unique(credibilityTbl2multipleIDs))
length(unique(phq4Tbl2multipleIDs))
length(unique(expectancyBiasTbl2multipleIDs))
length(unique(whatIBelieveTbl2multipleIDs))
length(unique(as.integer(as.character(demographicTbl2multipleIDs))))
length(unique(mentalHealthHxTbl2multipleIDs))

setdiff(randomizeTbl2multipleIDs, taskLogTbl2multipleIDs)
setdiff(credibilityTbl2multipleIDs, taskLogTbl2multipleIDs)
setdiff(phq4Tbl2multipleIDs, taskLogTbl2multipleIDs)
setdiff(expectancyBiasTbl2multipleIDs, taskLogTbl2multipleIDs)
setdiff(whatIBelieveTbl2multipleIDs, taskLogTbl2multipleIDs)
setdiff(as.integer(as.character(demographicTbl2multipleIDs)), 
        taskLogTbl2multipleIDs)
setdiff(mentalHealthHxTbl2multipleIDs, taskLogTbl2multipleIDs)

length(unique(c(randomizeTbl2multipleIDs,
                credibilityTbl2multipleIDs,
                phq4Tbl2multipleIDs, 
                expectancyBiasTbl2multipleIDs, 
                whatIBelieveTbl2multipleIDs,
                as.integer(as.character(demographicTbl2multipleIDs)),
                mentalHealthHxTbl2multipleIDs)))

# ---------------------------------------------------------------------------- #
# DECISION 1 ----
# ---------------------------------------------------------------------------- #

# EXCLUDE THESE 14 SUBJECTS FROM ANALYSES DUE TO NONIDENTICAL REPEATED SCREEN.

# 200, 392, 412, 495, 496, 577, 582, 627, 634, 961:
#   Did expectancyBias at Screening twice, 2-20 min apart. Responses different 
#   and not incomplete.
# 453:
#   Did expectancyBias at Screening 3 times, a few min apart. Responses 
#   different and not incomplete.
# 788:
#   Did expectancyBias at Screening 3 times, a few min apart. Responses 
#   different and not incomplete.
# 138:
#   Did expectancyBias at Screening, Pretest, and preAffect question twice, 
#   a few min apart. Responses on tables above different and not incomplete.
# 942:
#   Did expectancyBias at Screening, Pretest, and preAffect question twice, 
#   a few min apart. Responses on tables above different and not incomplete.

# ---------------------------------------------------------------------------- #
# DECISION 2 ----
# ---------------------------------------------------------------------------- #

# ANALYZE THE FIRST EXPECTANCY BIAS SUBMISSION AT SCREENING.

# 178, 434, 517, 594, 758, 874, 1055, 1061, 1068, 1094, 1138, 1141, 1162, 1165, 
# 1186, 1245, 1271:
#   Did expectancyBias at Screening twice, 2-20 min apart. Responses identical 
#   and not incomplete.

# ---------------------------------------------------------------------------- #
# DECISION 3 ----
# ---------------------------------------------------------------------------- #

# ANALYZE THE FIRST EXPECTANCY BIAS SUBMISSION.

# 992:
#   Did expectancyBias at Screening twice, 1 min apart. Responses identical 
#   and not incomplete. Also did preAffect question twice at S1, a few min 
#   apart.

# ---------------------------------------------------------------------------- #
# DECISION 4 ----
# ---------------------------------------------------------------------------- #

# ANALYZE THE FIRST CREDIBILITY, DEMOGRAPHICS, AND MENTAL HEALTH HISTORY
# SUBMISSIONS.

# 239:
#   Did Demographics at Pretest twice, a few min apart. Responses identical 
#   and not incomplete.
# 352:
#   Did Credibility and Demographics at Pretest twice, a few min apart. 
#   Responses on Demographics and Credibility identical and not incomplete.
# 971:
#   Did Credibility and Demographics at Pretest twice, a few min apart. 
#   Responses on Demographics and Credibility identical and not incomplete.
# 834:
#   Did Credibility 5 times and Demographics 3 times at Pretest, a few min 
#   apart. Responses on Demographics identical and Credibility nearly 
#   identical and not incomplete.
# 862:
#   Did Credibility, Demographics, and MentalHealthHistory at Pretest twice, 
#   a few min apart. Responses on Demographics and Credibility identical and 
#   Mental Health History similar and not incomplete.
# 868:
#   Did Credibility at Pretest twice, a few min apart. Responses similar 
#   and not incomplete.

# ---------------------------------------------------------------------------- #
# DECISION 5 ----
# ---------------------------------------------------------------------------- #

# ANALYZE THE FIRST CREDIBILITY, DEMOGRAPHICS, AND MENTAL HEALTH HISTORY
# SUBMISSIONS AND THE FIRST SESSION 3 SUBMISSION.

# 734:
#   Did Credibility, Demographics, and MentalHealthHistory at Pretest and 
#   then did all Pretest again a few min later. Also did S3 twice, 5 days 
#   apart. Responses on Credibility, Demographics, and MentalHealthHistory 
#   identical and not incomplete. Responses on ExpectancyBias differ on one 
#   item and not incomplete.

# ---------------------------------------------------------------------------- #
# DECISION 6 ----
# ---------------------------------------------------------------------------- #

# ANALYZE THE FIRST CREDIBILITY AND DEMOGRAPHICS SUBMISSIONS.

# 969:
#   Did extra Credibility and Demographics after preAffect of S1, a few 
#   min apart. Responses on Credibility and Demographics similar and not 
#   incomplete.
# 469:
#   Did extra Credibility and Demographics after preAffect of S1, a few 
#   min apart. Responses to Credibility and Demographics similar and not 
#   incomplete.
# 99:
#   Did extra Credibility after preAffect of S1, a few min apart. Responses
#   to Credibility identical and not incomplete.
# 186:
#   Did extra Credibility after preAffect of S1, a few min apart. Responses
#   to Credibility identical and not incomplete.
# 194:
#   Did extra Credibility after preAffect of S1, a few min apart. Responses
#   to Credibility identical and not incomplete.
# 831:
#   Did extra Credibility after S1, a few min apart. Responses to Credibility
#   similar and not incomplete.

# ---------------------------------------------------------------------------- #
# DECISION 7 ----
# ---------------------------------------------------------------------------- #

# ANALYZE THE FIRST PRETEST SUBMISSIONS.

# 883:
#   Did Pretest twice, a few min apart. Responses on tables above nearly 
#   identical and not incomplete.
# 584:
#   Did Pretest twice, a few min apart. Responses on tables above similar 
#   and not incomplete.
# 91:
#   Did Pretest twice, a few min apart. Responses on tables above similar 
#   and not incomplete.
# 1112:
#   Did Pretest twice, a few min apart. Responses on tables above similar 
#   and not incomplete.
# 1130:
#   Did Pretest twice, a few min apart. Responses on tables above similar 
#   and not incomplete.
# 261:
#   Did Pretest twice, a few min apart. Responses on tables above similar 
#   and not incomplete.
# 223:
#   Did Pretest twice, a few days apart. Responses on tables above similar 
#   and not incomplete.
# 814:
#   Did Pretest twice, a few days apart. Responses on some tables above 
#   different and not incomplete.
# 1031:
#   Did Pretest 3 times, a few min apart. Responses on tables above similar 
#   and not incomplete.

# ---------------------------------------------------------------------------- #
# DECISION 8 ----
# ---------------------------------------------------------------------------- #

# ANALYZE THE FIRST PRETEST SUBMISSIONS.

# 130:
#   Did Pretest and preAffect question twice, a few min apart. Responses on 
#   tables above identical and not incomplete.
# 137:
#   Did Pretest and preAffect question twice, a few min apart. Responses on 
#   tables above identical and not incomplete.
# 760:
#   Did Pretest and preAffect question twice, a few min apart. Responses on 
#   tables above identical and not incomplete.
# 950:
#   Did Pretest and preAffect question twice, a few min apart. Responses on 
#   tables above similar and not incomplete.
# 711:
#   Did Pretest and preAffect question twice, a few min apart. Responses on 
#   tables above similar and not incomplete.
# 533:
#   Did Pretest and preAffect question twice, a few days apart. Responses on 
#   tables above similar and not incomplete.
# 947:
#   Did Pretest and preAffect question twice, a few min apart. Responses on 
#   tables above different and not incomplete.
# 895:
#   Did Pretest and preAffect question twice, a few min apart. Responses on 
#   tables above different and not incomplete.

# ---------------------------------------------------------------------------- #
# DECISION 9 ----
# ---------------------------------------------------------------------------- #

# ANALYZE THE FIRST PRETEST SUBMISSION. ALSO, ANALYZE THE FIRST FULL SET 
# ADJACENT SUBMISSIONS FOR SESSION 3 (DO NOT REQUIRE RETURN INTENTION FOR 
# SESSION 3 TO BE COMPLETE).

# 916:
#   Did Pretest and preAffect question twice, a day apart. Also did all but 
#   ReturnIntention for S3 and then the entire S3 again on 8/16/17; then did 
#   all but preAffect for S3 again on 8/21/17. Responses on tables above 
#   similar and not incomplete.

# ---------------------------------------------------------------------------- #
# DECISION 10 ----
# ---------------------------------------------------------------------------- #

# ANALYZE THE FIRST PRETEST SUBMISSION AND THE FIRST FULL SET OF ADJACENT 
# SUBMISSIONS FOR SESSION 1.

# 943:
#   Did Pretest and part of S1 twice, a day apart. Responses on tables above 
#   different and not incomplete.
# 565:
#   Did Pretest and part of S1 twice, 2 months apart. Responses on tables 
#   above different and not incomplete.
# 1066:
#   Did Pretest and part of S1 twice, 17 days apart. Responses on most 
#   tables above similar and not incomplete.
# 49:
#   Did Pretest and S1 on 8/3/17 and then both again on 8/13/17. Responses 
#   on tables above similar and not incomplete.
# 1160:
#   Did Pretest and S1 on 9/18/17 and then both again on 9/23/17. Responses 
#   on some tables above different but not incomplete.

# ---------------------------------------------------------------------------- #
# DECISION 11 ----
# ---------------------------------------------------------------------------- #

# ANALYZE THE FIRST FULL SET OF ADJACENT SUBMISSIONS FOR SESSION 1.

# 1184:
#   Did postAffect and outcome measures twice at S1, a few min apart. 
#   Responses on tables above similar and not incomplete.
# 1135:
#   Did S1 twice, 13 days apart. Responses on tables above similar and not 
#   incomplete. Responses on tables above similar and not incomplete.

# ---------------------------------------------------------------------------- #
# DECISION 12 ----
# ---------------------------------------------------------------------------- #

# ANALYZE THE FIRST FULL SET OF ADJACENT SUBMISSIONS FOR SESSION 2 AND THE MOST 
# COMPLETE SET OF ADJACENT SUBMISSIONS FOR SESSION 4 (DO NOT REQUIRE RETURN 
# INTENTION FOR THE SESSION TO BE COMPLETE).

# 125:
#   Did all but ReturnIntention for S2 on 8/5/17 and then the entire S2 
#   again on 8/7/17. Responses on tables above similar and not incomplete.
# 879:
#   Did part of S2 through ExpectancyBias on 8/3/17, then did all of S2 
#   again the next day.
# 782:
#   Did S2 twice, 6 days apart. Also did parts of S4 twice, 9 days apart. 
#   Responses on some tables above different and not incomplete.

# ---------------------------------------------------------------------------- #
# DECISION 13 ----
# ---------------------------------------------------------------------------- #

# ANALYZE THE FIRST FULL SET OF ADJACENT SUBMISSIONS FOR SESSION 4 (DO NOT 
# REQUIRE RETURN INTENTION FOR SESSION 4 TO BE COMPLETE).

# 408:
#   Did all but ReturnIntention for S4 on 8/19/17 and then the entire S4 
#   again on 8/24/17. Responses on tables above similar and not incomplete.
# 732:
#   Did all but HelpSeeking and Return Intention for S4 and then did entire 
#   S4 again a few min later. Responses on tables above similar and not 
#   incomplete.

# ---------------------------------------------------------------------------- #
# DECISION 14 ----
# ---------------------------------------------------------------------------- #

# ANALYZE THE FIRST FOLLOW-UP SUBMISSION (STUDY SHOULD HAVE ENDED AFTER THAT).

# 662:
#   Did FU twice, 23 days apart. Responses on some tables above different 
#   and not incomplete.

# ---------------------------------------------------------------------------- #
# DECISION 15 ----
# ---------------------------------------------------------------------------- #

# ANALYZE THE LAST REASONS FOR ENDING SUBMISSION FOR THIS PARTICIPANT.

# 137:
#   Did Reasons for Ending four times, a few min apart. First and last responses
#   are the same, but last response has additional free-text information.