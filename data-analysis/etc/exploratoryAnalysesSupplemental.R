if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if(!require(gridExtra)) install.packages("gridExtra")
library(gridExtra) # for side by side plots
if(!require(patchwork)) install.packages("patchwork")
library(patchwork) # for side-by-side plots
if(!require(car)) install.packages("car")
library(car) # for recoding scale responses

# Clean up R's memory
rm(list = ls()) 

source("helpers.R")

# Read participant data (n=548)
participants <- read.csv('../participants-raw.csv')

# Recode cognitive load metric
participants$PT14_rec <- recode(participants$PT14, "1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10")
# Recode precision metric
participants$PT15_rec <- recode(participants$PT15, "1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10")

# For easier access, separate participants into the three preference elicitation expressiveness groups: OD = low, RF = medium, ASF = high
twopointlGroup <- participants[participants$IV03_01 == "OD", ]
threepointlGroup <- participants[participants$IV03_01 == "RF", ]
nonlinearGroup <- participants[participants$IV03_01 == "ASF", ]

######## Perceived Cognitive Load and Precision ########

# Perceived Cognitive Load 
# Plot boxplots for each expressiveness condition
boxplots <- ggplot(participants, aes(x=IV03_01, y=PT14_rec)) +
  geom_boxplot() + 
  xlab("Preference Elicitation Expressiveness") +
  ylab("Perceived Cognitive Load") +
  scale_y_continuous(limits = c(0,10),  breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  theme_minimal()
boxplots

# Compute mean and standard deviation for each expressiveness condition
mean(twopointlGroup$PT14_rec)
sd(twopointlGroup$PT14_rec)
mean(threepointlGroup$PT14_rec)
sd(threepointlGroup$PT14_rec)
mean(nonlinearGroup$PT14_rec)
sd(nonlinearGroup$PT14_rec)

# Perceived Precision
# Plot boxplots for each expressiveness condition
boxplots <- ggplot(participants, aes(x=IV03_01, y=PT15_rec)) +
  geom_boxplot() + 
  xlab("Preference Elicitation Expressiveness") +
  ylab("Perceived Precision") +
  scale_y_continuous(limits = c(0,10),  breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  theme_minimal()
boxplots

# Compute mean and standard deviation for each expressiveness condition
mean(twopointlGroup$PT15_rec)
sd(twopointlGroup$PT15_rec)
mean(threepointlGroup$PT15_rec)
sd(threepointlGroup$PT15_rec)
mean(nonlinearGroup$PT15_rec)
sd(nonlinearGroup$PT15_rec)

######## Repeated Analyses Grouped by Mini-VLAT Scores ########

# Plot distribution of Mini-VLAT scores as histogram
histogram <- ggplot(participants, aes(x = IV01_01)) +
  geom_histogram(binwidth=0.5) +
  xlab("VLAT Score") +
  scale_x_continuous(limits = c(0,12), breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  theme_minimal()
histogram

# Repeated analyses for H1 to H5 by Mini-VLAT groups

# Read pre-processed participant data (n=548)
participants <- read.csv('../participants-preprocessed.csv')

# For easier access, separate participants into the three preference elicitation expressiveness groups: OD = low, RF = medium, ASF = high
twopointlGroup <- participants[participants$Elicitation == "OD", ]
threepointlGroup <- participants[participants$Elicitation == "RF", ]
nonlinearGroup <- participants[participants$Elicitation == "ASF", ]

# Where to store results
results.H.1_low <- data.frame(label = character(0), ci.point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0), cond = character(0))
results.H.1_medium <- data.frame(label = character(0), ci.point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0), cond = character(0))
results.H.1_high <- data.frame(label = character(0), ci.point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0), cond = character(0))
results.H.2_low <- data.frame(label = character(0), ci.point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0), cond = character(0))
results.H.2_medium <- data.frame(label = character(0), ci.point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0), cond = character(0))
results.H.2_high <- data.frame(label = character(0), ci.point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0), cond = character(0))
results.H.3_low <- data.frame(label = character(0), point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0))
results.H.3_medium <- data.frame(label = character(0), point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0))
results.H.3_high <- data.frame(label = character(0), point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0))
results.H.4_low <- data.frame(label = character(0), point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0))
results.H.4_medium <- data.frame(label = character(0), point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0))
results.H.4_high <- data.frame(label = character(0), point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0))

# H1
twopointlGroup_low <- subset(twopointlGroup, VLATScore < 5)
twopointlGroup_medium <- subset(twopointlGroup, VLATScore > 4 & VLATScore < 8)
twopointlGroup_high <- subset(twopointlGroup, VLATScore > 7)

twopointl.pc.accuracy_low <- twopointlGroup_low$AccuracyPC
twopointl.tv.accuracy_low <- twopointlGroup_low$AccuracyTV
results.H.1_low <- accuracyCondition(twopointl.pc.accuracy_low, results.H.1_low) 
results.H.1_low <- accuracyCondition(twopointl.tv.accuracy_low, results.H.1_low)
results.H.1_low <- accuracyCondition(twopointl.pc.accuracy_low - twopointl.tv.accuracy_low, results.H.1_low)

twopointl.pc.accuracy_medium <- twopointlGroup_medium$AccuracyPC
twopointl.tv.accuracy_medium <- twopointlGroup_medium$AccuracyTV
results.H.1_medium <- accuracyCondition(twopointl.pc.accuracy_medium, results.H.1_medium) 
results.H.1_medium <- accuracyCondition(twopointl.tv.accuracy_medium, results.H.1_medium)
results.H.1_medium <- accuracyCondition(twopointl.pc.accuracy_medium - twopointl.tv.accuracy_medium, results.H.1_medium)

twopointl.pc.accuracy_high <- twopointlGroup_high$AccuracyPC
twopointl.tv.accuracy_high <- twopointlGroup_high$AccuracyTV
results.H.1_high <- accuracyCondition(twopointl.pc.accuracy_high, results.H.1_high) 
results.H.1_high <- accuracyCondition(twopointl.tv.accuracy_high, results.H.1_high)
results.H.1_high <- accuracyCondition(twopointl.pc.accuracy_high - twopointl.tv.accuracy_high, results.H.1_high)

# Plot CIs for H1
to_plot <- rbind(results.H.1_low[1:2, ], results.H.1_medium[1:2, ], results.H.1_high[1:2, ])
plot_all<- plotAllCI_fixedBreaks(to_plot, plotRange = c(0, 1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) 
to_plot_diff <- rbind(results.H.1_low[3:3, ], results.H.1_medium[3:3, ], results.H.1_high[3:3, ])
plot_diff    <- plotDiffCI_fixedBreaks(to_plot_diff, plotRange=c(-0.5, 0.5), breaks=c(-0.1, 0, 0.1))
wrap_plots(plot_all, plot_diff, ncol=2, nrow=1, widths = c(1, 1))

# H2
threepointlGroup_low <- subset(threepointlGroup, VLATScore < 5)
threepointlGroup_medium <- subset(threepointlGroup, VLATScore > 4 & VLATScore < 8)
threepointlGroup_high <- subset(threepointlGroup, VLATScore > 7)

threepointl.pc.accuracy_low <- threepointlGroup_low$AccuracyPC
threepointl.tv.accuracy_low <- threepointlGroup_low$AccuracyTV
results.H.2_low <- accuracyCondition(threepointl.pc.accuracy_low, results.H.2_low) 
results.H.2_low <- accuracyCondition(threepointl.tv.accuracy_low, results.H.2_low)
results.H.2_low <- accuracyCondition(threepointl.pc.accuracy_low - threepointl.tv.accuracy_low, results.H.2_low)

threepointl.pc.accuracy_medium <- threepointlGroup_medium$AccuracyPC
threepointl.tv.accuracy_medium <- threepointlGroup_medium$AccuracyTV
results.H.2_medium <- accuracyCondition(threepointl.pc.accuracy_medium, results.H.2_medium) 
results.H.2_medium <- accuracyCondition(threepointl.tv.accuracy_medium, results.H.2_medium)
results.H.2_medium <- accuracyCondition(threepointl.pc.accuracy_medium - threepointl.tv.accuracy_medium, results.H.2_medium)

threepointl.pc.accuracy_high <- threepointlGroup_high$AccuracyPC
threepointl.tv.accuracy_high <- threepointlGroup_high$AccuracyTV
results.H.2_high <- accuracyCondition(threepointl.pc.accuracy_high, results.H.2_high) 
results.H.2_high <- accuracyCondition(threepointl.tv.accuracy_high, results.H.2_high)
results.H.2_high <- accuracyCondition(threepointl.pc.accuracy_high - threepointl.tv.accuracy_high, results.H.2_high)

nonlinearGroup_low <- subset(nonlinearGroup, VLATScore < 5)
nonlinearGroup_medium <- subset(nonlinearGroup, VLATScore > 4 & VLATScore < 8)
nonlinearGroup_high <- subset(nonlinearGroup, VLATScore > 7)

nonlinear.pc.accuracy_low <- nonlinearGroup_low$AccuracyPC
nonlinear.tv.accuracy_low <- nonlinearGroup_low$AccuracyTV
results.H.2_low <- accuracyCondition(nonlinear.pc.accuracy_low, results.H.2_low) 
results.H.2_low <- accuracyCondition(nonlinear.tv.accuracy_low, results.H.2_low)
results.H.2_low <- accuracyCondition(nonlinear.pc.accuracy_low - nonlinear.tv.accuracy_low, results.H.2_low)

nonlinear.pc.accuracy_medium <- nonlinearGroup_medium$AccuracyPC
nonlinear.tv.accuracy_medium <- nonlinearGroup_medium$AccuracyTV
results.H.2_medium <- accuracyCondition(nonlinear.pc.accuracy_medium, results.H.2_medium) 
results.H.2_medium <- accuracyCondition(nonlinear.tv.accuracy_medium, results.H.2_medium)
results.H.2_medium <- accuracyCondition(nonlinear.pc.accuracy_medium - nonlinear.tv.accuracy_medium, results.H.2_medium)

nonlinear.pc.accuracy_high <- nonlinearGroup_high$AccuracyPC
nonlinear.tv.accuracy_high <- nonlinearGroup_high$AccuracyTV
results.H.2_high <- accuracyCondition(nonlinear.pc.accuracy_high, results.H.2_high) 
results.H.2_high <- accuracyCondition(nonlinear.tv.accuracy_high, results.H.2_high)
results.H.2_high <- accuracyCondition(nonlinear.pc.accuracy_high - nonlinear.tv.accuracy_high, results.H.2_high)

# Plot CIs for H2
to_plot <- rbind(results.H.2_low[1:2, ], results.H.2_medium[1:2, ], results.H.2_high[1:2, ], results.H.2_low[4:5, ], results.H.2_medium[4:5, ], results.H.2_high[4:5, ])
plot_all<- plotAllCI_fixedBreaks(to_plot, plotRange = c(0, 1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) 
to_plot_diff <- rbind(results.H.2_low[3:3, ], results.H.2_medium[3:3, ], results.H.2_high[3:3, ], results.H.2_low[6:6, ], results.H.2_medium[6:6, ], results.H.2_high[6:6, ])
plot_diff    <- plotDiffCI_fixedBreaks(to_plot_diff, plotRange=c(-0.5, 0.5), breaks=c(-0.1, 0, 0.1))
wrap_plots(plot_all, plot_diff, ncol=2, nrow=1, widths = c(1, 1))

# H3
participants_low <- subset(participants, VLATScore < 5)
participants_medium <- subset(participants, VLATScore > 4 & VLATScore < 8)
participants_high <- subset(participants, VLATScore > 7)

results.H.3_low <- diffOfDiffsCondition(participants_low, dataCol = "AccuracyDiff", groups = c("OD", "RF"), results.H.3_low)
results.H.3_low <- diffOfDiffsCondition(participants_low, dataCol = "AccuracyDiff", groups = c("OD", "ASF"), results.H.3_low)
results.H.3_low <- diffOfDiffsCondition(participants_low, dataCol = "AccuracyDiff", groups = c("RF", "ASF"), results.H.3_low)

results.H.3_medium <- diffOfDiffsCondition(participants_medium, dataCol = "AccuracyDiff", groups = c("OD", "RF"), results.H.3_medium)
results.H.3_medium <- diffOfDiffsCondition(participants_medium, dataCol = "AccuracyDiff", groups = c("OD", "ASF"), results.H.3_medium)
results.H.3_medium <- diffOfDiffsCondition(participants_medium, dataCol = "AccuracyDiff", groups = c("RF", "ASF"), results.H.3_medium)

results.H.3_high <- diffOfDiffsCondition(participants_high, dataCol = "AccuracyDiff", groups = c("OD", "RF"), results.H.3_high)
results.H.3_high <- diffOfDiffsCondition(participants_high, dataCol = "AccuracyDiff", groups = c("OD", "ASF"), results.H.3_high)
results.H.3_high <- diffOfDiffsCondition(participants_high, dataCol = "AccuracyDiff", groups = c("RF", "ASF"), results.H.3_high)

# Plot CIs for H3
plot_diffofdiffs  <- plotDiffCI_fixedBreaks(rbind(results.H.3_low, results.H.3_medium, results.H.3_high), plotRange=c(-0.5, 0.5), breaks=c(-0.1, 0, 0.1, 0.2)) 
plot_diffofdiffs

# H4
twopointl.accuracy_low <- twopointlGroup_low$AccuracyMean
threepointl.accuracy_low <- threepointlGroup_low$AccuracyMean
nonlinear.accuracy_low <- nonlinearGroup_low$AccuracyMean
results.H.4_low <- accuracyCondition(twopointl.accuracy_low, results.H.4_low)
results.H.4_low <- accuracyCondition(threepointl.accuracy_low, results.H.4_low)
results.H.4_low <- accuracyCondition(nonlinear.accuracy_low, results.H.4_low)
results.H.4_low <- diffOfDiffsCondition(participants_low, dataCol = "AccuracyMean", groups = c("OD", "RF"), results.H.4_low)
results.H.4_low <- diffOfDiffsCondition(participants_low, dataCol = "AccuracyMean", groups = c("OD", "ASF"), results.H.4_low)
results.H.4_low <- diffOfDiffsCondition(participants_low, dataCol = "AccuracyMean", groups = c("RF", "ASF"), results.H.4_low)

twopointl.accuracy_medium <- twopointlGroup_medium$AccuracyMean
threepointl.accuracy_medium <- threepointlGroup_medium$AccuracyMean
nonlinear.accuracy_medium <- nonlinearGroup_medium$AccuracyMean
results.H.4_medium <- accuracyCondition(twopointl.accuracy_medium, results.H.4_medium)
results.H.4_medium <- accuracyCondition(threepointl.accuracy_medium, results.H.4_medium)
results.H.4_medium <- accuracyCondition(nonlinear.accuracy_medium, results.H.4_medium)
results.H.4_medium <- diffOfDiffsCondition(participants_medium, dataCol = "AccuracyMean", groups = c("OD", "RF"), results.H.4_medium)
results.H.4_medium <- diffOfDiffsCondition(participants_medium, dataCol = "AccuracyMean", groups = c("OD", "ASF"), results.H.4_medium)
results.H.4_medium <- diffOfDiffsCondition(participants_medium, dataCol = "AccuracyMean", groups = c("RF", "ASF"), results.H.4_medium)

twopointl.accuracy_high <- twopointlGroup_high$AccuracyMean
threepointl.accuracy_high <- threepointlGroup_high$AccuracyMean
nonlinear.accuracy_high <- nonlinearGroup_high$AccuracyMean
results.H.4_high <- accuracyCondition(twopointl.accuracy_high, results.H.4_high)
results.H.4_high <- accuracyCondition(threepointl.accuracy_high, results.H.4_high)
results.H.4_high <- accuracyCondition(nonlinear.accuracy_high, results.H.4_high)
results.H.4_high <- diffOfDiffsCondition(participants_high, dataCol = "AccuracyMean", groups = c("OD", "RF"), results.H.4_high)
results.H.4_high <- diffOfDiffsCondition(participants_high, dataCol = "AccuracyMean", groups = c("OD", "ASF"), results.H.4_high)
results.H.4_high <- diffOfDiffsCondition(participants_high, dataCol = "AccuracyMean", groups = c("RF", "ASF"), results.H.4_high)

# Plot CIs for H4
to_plot       <- rbind(results.H.4_low[1:3, ], results.H.4_medium[1:3, ], results.H.4_high[1:3, ])
plot_all      <- plotAllCI_fixedBreaks(to_plot, plotRange = c(0, 1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) 
to_plot_diff  <- rbind(results.H.4_low[4:6, ], results.H.4_medium[4:6, ], results.H.4_high[4:6, ])
plot_diff     <- plotDiffCI_fixedBreaks(to_plot_diff, plotRange=c(-0.5, 0.5), breaks=c(-0.1, 0, 0.1))
wrap_plots(plot_all, plot_diff, ncol=2, nrow=1, widths = c(1, 1))

# H5
boxplots_low <- plotMeanAccuracyBoxplots(participants_low, plotRange=c(0,1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
boxplots_medium <- plotMeanAccuracyBoxplots(participants_medium, plotRange=c(0,1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
boxplots_high <- plotMeanAccuracyBoxplots(participants_high, plotRange=c(0,1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
wrap_plots(boxplots_low, boxplots_medium, boxplots_high, ncol=3, nrow=1, widths = c(1, 1, 1))

######## Repeated Analyses with Filtered Participant Subset ########

# Filter out participants with one failed attention check 
screened_out_ac <- subset(participants, AttentionScore < 2)
participants <- subset(participants, AttentionScore > 1)

# Filter out participants with failed visualization training tasks
screened_out_vistraining <- subset(participants, TrainingScoreTV < 2 | TrainingScorePC < 2)
participants <- subset(participants, TrainingScoreTV > 1 & TrainingScorePC > 1)

# Filter out participants with failed preference training tasks
screened_out_preftraining <- subset(participants, Elicitation != "OD" & TrainingScorePref < 1)
participants <- subset(participants, Elicitation == "OD" | TrainingScorePref > 0)

# Again, separate participants into the three preference elicitation expressiveness groups: OD = low, RF = medium, ASF = high
twopointlGroup <- participants[participants$Elicitation == "OD", ]
threepointlGroup <- participants[participants$Elicitation == "RF", ]
nonlinearGroup <- participants[participants$Elicitation == "ASF", ]

# Where to store results
results.H.1 <- data.frame(label = character(0), ci.point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0), cond = character(0))
results.H.2 <- data.frame(label = character(0), point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0), cond = character(0))
results.H.3 <- data.frame(label = character(0), point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0))
results.H.4 <- data.frame(label = character(0), point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0), cond = character(0))

# H1: Mean difference in accuracy between PC and TV for low expressiveness condition
# Accuracy scores for PC task of participants exposed to low expressiveness condition
twopointl.pc.accuracy <- twopointlGroup$AccuracyPC
# Accuracy scores for TV task of participants exposed to low expressiveness condition
twopointl.tv.accuracy <- twopointlGroup$AccuracyTV
# Confidence interval of PC accuracies for low expressiveness condition
results.H.1 <- accuracyCondition(twopointl.pc.accuracy, results.H.1) 
# Confidence interval of TV accuracies for low expressiveness condition
results.H.1 <- accuracyCondition(twopointl.tv.accuracy, results.H.1)
# Mean PC-TV differences for low expressiveness condition
results.H.1 <- accuracyCondition(twopointl.pc.accuracy - twopointl.tv.accuracy, results.H.1)

# Plot CIs for H1
to_plot       <- rbind(results.H.1[1:2, ])
plot_all      <- plotAllCI_fixedBreaks(to_plot, plotRange = c(0, 1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
to_plot_diff  <- rbind(results.H.1[3:3, ])
plot_diff     <- plotDiffCI_fixedBreaks(to_plot_diff, plotRange = c(-0.5, 0.5), breaks=c(-0.1, 0, 0.1))
wrap_plots(plot_all, plot_diff, ncol=2, nrow=1, widths = c(1, 1))

#H2: Mean difference in accuracy between PC and TV for medium and high expressiveness conditions
# Accuracy scores for PC task of participants exposed to medium expressiveness condition
threepointl.pc.accuracy <- threepointlGroup$AccuracyPC
# Accuracy scores for TV task of participants exposed to medium expressiveness condition
threepointl.tv.accuracy <- threepointlGroup$AccuracyTV
# Accuracy scores for PC task of participants exposed to high expressiveness condition
nonlinear.pc.accuracy <- nonlinearGroup$AccuracyPC
# Accuracy scores for TV task of participants exposed to high expressiveness condition
nonlinear.tv.accuracy <- nonlinearGroup$AccuracyTV
# Confidence interval of PC accuracies for medium expressiveness condition
results.H.2 <- accuracyCondition(threepointl.pc.accuracy, results.H.2)
# Confidence interval of TV accuracies for medium expressiveness condition
results.H.2 <- accuracyCondition(threepointl.tv.accuracy, results.H.2)
# Mean PC-TV differences for medium expressiveness condition
results.H.2 <- accuracyCondition(threepointl.pc.accuracy - threepointl.tv.accuracy, results.H.2)
# Confidence interval of PC accuracies for high expressiveness condition
results.H.2 <- accuracyCondition(nonlinear.pc.accuracy, results.H.2)
# Confidence interval of TV accuracies for high expressiveness condition
results.H.2 <- accuracyCondition(nonlinear.tv.accuracy, results.H.2)
# Mean PC-TV differences for high expressiveness condition
results.H.2 <- accuracyCondition(nonlinear.pc.accuracy - nonlinear.tv.accuracy, results.H.2)

# Plot CIs for H2
to_plot       <- rbind(results.H.2[1:2, ], results.H.2[4:5, ])
plot_all      <- plotAllCI_fixedBreaks(to_plot, plotRange = c(0, 1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
to_plot_diff  <- rbind(results.H.2[3:3, ], results.H.2[6:6, ])
plot_diff     <- plotDiffCI_fixedBreaks(to_plot_diff, plotRange = c(-0.5, 0.5), breaks=c(-0.1, 0, 0.1))
wrap_plots(plot_all, plot_diff, ncol=2, nrow=1, widths = c(1, 1))

#H3: "Difference of differences" between elicitation conditions
# Mean differences between PC-TV differences of low and medium expressiveness conditions 
results.H.3 <- diffOfDiffsCondition(participants, dataCol = "AccuracyDiff", groups = c("OD", "RF"), results.H.3)
# Mean differences between PC-TV differences of low and high expressiveness conditions 
results.H.3 <- diffOfDiffsCondition(participants, dataCol = "AccuracyDiff", groups = c("OD", "ASF"), results.H.3)
# Mean differences between PC-TV differences of medium and high expressiveness conditions 
results.H.3 <- diffOfDiffsCondition(participants, dataCol = "AccuracyDiff", groups = c("RF", "ASF"), results.H.3)

#Plot CIs for H3
plot_diffofdiffs  <- plotDiffCI_fixedBreaks(results.H.3, plotRange = c(-0.5, 0.5), breaks=c(-0.1, 0, 0.1)) 
plot_diffofdiffs

#H4: Mean accuracy difference across visualization techniques between expressiveness levels
# Means between accuracy scores of PC and TV tasks of participants exposed to low expressiveness condition
twopointl.accuracy <- twopointlGroup$AccuracyMean
# Means between accuracy scores of PC and TV tasks of participants exposed to medium expressiveness condition
threepointl.accuracy <- threepointlGroup$AccuracyMean
# Means between accuracy scores of PC and TV tasks of participants exposed to high expressiveness condition
nonlinear.accuracy <- nonlinearGroup$AccuracyMean
# Confidence interval of accuracies across PC and TV for low expressiveness condition
results.H.4 <- accuracyCondition(twopointl.accuracy, results.H.4)
# Confidence interval of accuracies across PC and TV for medium expressiveness condition
results.H.4 <- accuracyCondition(threepointl.accuracy, results.H.4)
# Confidence interval of accuracies across PC and TV for high expressiveness condition
results.H.4 <- accuracyCondition(nonlinear.accuracy, results.H.4)
# Mean differences between low and medium expressiveness conditions
results.H.4 <- diffOfDiffsCondition(participants, dataCol = "AccuracyMean", groups = c("OD", "RF"), results.H.4)
# Mean differences between low and high expressiveness conditions
results.H.4 <- diffOfDiffsCondition(participants, dataCol = "AccuracyMean", groups = c("OD", "ASF"), results.H.4)
# Mean differences between medium and high expressiveness conditions
results.H.4 <- diffOfDiffsCondition(participants, dataCol = "AccuracyMean", groups = c("RF", "ASF"), results.H.4)

#Plot CIs for H4
to_plot       <- rbind(results.H.4[1:3, ])
plot_all      <- plotAllCI_fixedBreaks(to_plot, plotRange = c(0, 1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
to_plot_diff  <- rbind(results.H.4[4:6, ])
plot_diff     <- plotDiffCI_fixedBreaks(to_plot_diff, plotRange = c(-0.5, 0.5), breaks=c(-0.1, 0, 0.1))
wrap_plots(plot_all, plot_diff, ncol=2, nrow=1, widths = c(1, 1))

#H5: Variation of accuracy responses for each expressiveness condition
# Plot boxplots for each expressiveness condition
boxplots <- plotMeanAccuracyBoxplots(participants, plotRange=c(0,1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
boxplots