# ---------------------------------------------------------------------------- #
# Create Figures - Linear Spline
# Authors: Jeremy W. Eberle
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

groundhog.library(mitml, groundhog_day)
groundhog.library(ggplot2, groundhog_day)
groundhog.library(cowplot, groundhog_day)

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Set working directory ----
# ---------------------------------------------------------------------------- #

# Use setwd function to set working directory to the folder containing files
# downloaded from the Open Science Framework (https://osf.io/jp5ws/).

# ---------------------------------------------------------------------------- #
# Load results ----
# ---------------------------------------------------------------------------- #

load("./Results/Longitudinal Outcome - Linear Spline/ITT/result_itt.RData")
load("./Results/Longitudinal Outcome - Linear Spline/Completers/result_pp.RData")

# ---------------------------------------------------------------------------- #
# Define functions used throughout script ----
# ---------------------------------------------------------------------------- #

# Define function to compute model-predicted means and standard errors for
# model two_conditions_vs_neutral for various outcomes

compute_pred_means <- function(modelList) {
  # Create data frame with codings of condition and time variables
  
  condition <- rep(c("Both Positive", "Both 50/50", "Neutral Control"), each = 6)
  assessment <- rep(c("Baseline", "Session 1", "Session 2", "Session 3", 
                      "Session 4", "Follow-Up"), times = 3)
  
  pred_means <- data.frame(condition, assessment)
  
  pred_means$conditionBothPositive <- ifelse(pred_means$condition == "Both Positive", 1, 0)
  pred_means$conditionBothFiftyFifty <- ifelse(pred_means$condition == "Both 50/50", 1, 0)
  pred_means$time1[pred_means$assessment == "Baseline"] <- 0
  pred_means$time1[pred_means$assessment == "Session 1"] <- 1
  pred_means$time1[pred_means$assessment == "Session 2"] <- 2
  pred_means$time1[pred_means$assessment == "Session 3"] <- 3
  pred_means$time1[pred_means$assessment == "Session 4"] <- 4
  pred_means$time1[pred_means$assessment == "Follow-Up"] <- 4
  pred_means$time2[pred_means$assessment == "Baseline"] <- 0
  pred_means$time2[pred_means$assessment == "Session 1"] <- 0
  pred_means$time2[pred_means$assessment == "Session 2"] <- 0
  pred_means$time2[pred_means$assessment == "Session 3"] <- 0
  pred_means$time2[pred_means$assessment == "Session 4"] <- 0
  pred_means$time2[pred_means$assessment == "Follow-Up"] <- 1
  
  # Compute predicted means and standard errors using testConstraints
  
  for (i in 1:nrow(pred_means)) {
    conditionBothPositive <- pred_means$conditionBothPositive[i]
    conditionBothFiftyFifty <- pred_means$conditionBothFiftyFifty[i]
    time1 <- pred_means$time1[i]
    time2 <- pred_means$time2[i]
    
    ctr <- paste0("(Intercept) + ",
                  "conditionBothPositive * ", conditionBothPositive, " + ",
                  "conditionBothFiftyFifty * ", conditionBothFiftyFifty, " + ",
                  "time1 * ", time1, " + ",
                  "time2 * ", time2, " + ",
                  "`conditionBothPositive:time1` * (", conditionBothPositive, "*", time1, ") + ",
                  "`conditionBothFiftyFifty:time1` * (", conditionBothFiftyFifty, "*", time1, ") + ",
                  "`conditionBothPositive:time2` * (", conditionBothPositive, "*", time2, ") + ",
                  "`conditionBothFiftyFifty:time2` * (", conditionBothFiftyFifty, "*", time2, ")")
    mean <- testConstraints(modelList, constraints = ctr)
    
    pred_means$mean[i] <- mean$Qbar
    pred_means$se[i] <- sqrt(mean$T)
  }
  
  # Recode condition and assessment as factors for ggplot2
  
  pred_means$condition <- factor(pred_means$condition,
                                 levels = c("Both Positive",
                                            "Both 50/50",
                                            "Neutral Control"))
  pred_means$assessment <- factor(pred_means$assessment,
                                  levels = c("Baseline",
                                             "Session 1",
                                             "Session 2",
                                             "Session 3",
                                             "Session 4",
                                             "Follow-Up"))
  
  return(pred_means)
}

# Define function to plot estimated means and standard errors. Colors obtained
# from Color Brewer 2.0 three-class Dark2 palette
# (https://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3). Checked for
# vision deficiency using HCL Wizard (http://hclwizard.org:3000/cvdemulator/).

create_plot <- function(pred_means, hidden_pts, title, y_title, scale_min, 
                        scale_max, legend_position) {
  ggplot(pred_means, 
         aes(x = assessment, y = mean, 
             group = condition, color = condition, linetype = condition)) +
    geom_line() +
    geom_point(data = pred_means[!(pred_means$assessment %in% hidden_pts), ]) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  pred_means[!(pred_means$assessment %in% hidden_pts), ],
                  width = .3) +
    labs(title = title, 
         x = "Assessment",
         y = y_title) +
    scale_linetype_manual(name = "Condition",
                          values = c("Both Positive" = "longdash",
                                     "Both 50/50" = "solid",
                                     "Neutral Control" = "twodash")) +
    scale_color_manual(name = "Condition",
                       values = c("Both Positive" = "#1b9e77", 
                                  "Both 50/50" = "#7570b3", 
                                  "Neutral Control" = "#d95f02")) +
    scale_y_continuous(breaks = scale_min:scale_max, 
                       limits = c(scale_min, scale_max)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 11),
          legend.key.width = unit(2, "cm")) +
    theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
    theme(legend.position = legend_position)
}

# ---------------------------------------------------------------------------- #
# Create figures for two_conditions_vs_neutral model in ITT sample ----
# ---------------------------------------------------------------------------- #

# Compute estimated means and standard errors

pred_means_posExpBiasScale <- 
  compute_pred_means(result_itt$two_conditions_vs_neutral$posExpBiasScale$modelList)
pred_means_negExpBiasScale <- 
  compute_pred_means(result_itt$two_conditions_vs_neutral$negExpBiasScale$modelList)
pred_means_anxietyScale <- 
  compute_pred_means(result_itt$two_conditions_vs_neutral$anxietyScale$modelList)
pred_means_depressionScale <- 
  compute_pred_means(result_itt$two_conditions_vs_neutral$depressionScale$modelList)
pred_means_selfEffScale <- 
  compute_pred_means(result_itt$two_conditions_vs_neutral$selfEffScale$modelList)
pred_means_growthMindScale <- 
  compute_pred_means(result_itt$two_conditions_vs_neutral$growthMindScale$modelList)
pred_means_optimismScale <- 
  compute_pred_means(result_itt$two_conditions_vs_neutral$optimismScale$modelList)

# Create plots

p_posExpBiasScale <- 
  create_plot(pred_means_posExpBiasScale, NA, "Positive Expectancy Bias", 
              "Average Item Score", 1, 7, c(0.8, 0.2))
p_negExpBiasScale <- 
  create_plot(pred_means_negExpBiasScale, NA, "Negative Expectancy Bias", 
              "Average Item Score", 1, 7, c(0.8, 0.8))
p_anxietyScale <- 
  create_plot(pred_means_anxietyScale, c("Session 1", "Session 3"), "Anxiety Symptoms", 
              "Sum Score", 0, 6, c(0.8, 0.8))
p_depressionScale <- 
  create_plot(pred_means_depressionScale, c("Session 1", "Session 3"), "Depression Symptoms",
              "Sum Score", 0, 6, c(0.8, 0.8))
p_selfEffScale <- 
  create_plot(pred_means_selfEffScale, c("Session 1", "Session 3"), "Self-Efficacy", 
              "Average Item Score", 0, 4, c(0.8, 0.2))
p_growthMindScale <- 
  create_plot(pred_means_growthMindScale, c("Session 1", "Session 3"), "Growth Mindset", 
              "Average Item Score", 0, 4, c(0.8, 0.2))
p_optimismScale <- 
  create_plot(pred_means_optimismScale, c("Session 1", "Session 3"), "Optimism", 
              "Average Item Score", 0, 4, c(0.8, 0.2))

# Arrange plots for posExpBiasScale, negExpBiasScale, selfEffScale, and optimismScale

plot1_grid <- plot_grid(p_posExpBiasScale + 
                          theme(legend.position = "none") + 
                          xlab(NULL), 
                        p_negExpBiasScale + 
                          theme(legend.position = "none") + 
                          xlab(NULL) + 
                          ylab(NULL), 
                        p_selfEffScale + 
                          theme(legend.position = "none"), 
                        p_optimismScale + 
                          theme(legend.position = "none") +
                          ylab(NULL),
                        align = "hv",
                        ncol = 2)
plot1_leg <- get_legend(p_posExpBiasScale + 
                          theme(legend.position = "bottom"))
plot1 <- plot_grid(plot1_grid, plot1_leg, ncol = 1, rel_heights = c(1, .1))

# Arrange plots for anxietyScale, depressionScale, and growthMindScale

plot2_grid <- plot_grid(p_anxietyScale +
                          theme(legend.position = "none") +
                          xlab(NULL), 
                        p_depressionScale +
                          theme(legend.position = "none") +
                          ylab(NULL), 
                        p_growthMindScale +
                          theme(legend.position = "none"),
                        align = "hv",
                        ncol = 2)
plot2_leg <- get_legend(p_anxietyScale + 
                          theme(legend.position = "bottom"))
plot2 <- plot_grid(plot2_grid, plot2_leg, ncol = 1, rel_heights = c(1, .1))

# Save plots

ggsave2("./Results/Longitudinal Outcome - Linear Spline/ITT/plots/2conditions_vs_NEUTRAL_plot_1.png",
        plot = plot1,
        width = 10, height = 10)
ggsave2("./Results/Longitudinal Outcome - Linear Spline/ITT/plots/2conditions_vs_NEUTRAL_plot_2.png",
        plot = plot2,
        width = 10, height = 10)

ggsave2("./Results/Longitudinal Outcome - Linear Spline/ITT/plots/2conditions_vs_NEUTRAL_plot_1.pdf",
        plot = plot1,
        width = 10, height = 10)
ggsave2("./Results/Longitudinal Outcome - Linear Spline/ITT/plots/2conditions_vs_NEUTRAL_plot_2.pdf",
        plot = plot2,
        width = 10, height = 10)