# This analysis script is inspired by the data analysis provided with the preregistration 
# "A Data Visualization Tale of Two Tails: Decoupling Judgment and Decision Making" (https://osf.io/t82nq)
# by Başak Oral, Evanthia Dimara, and Pierre Dragicevic, which was licensed under CC Attribution-ShareAlike

if(!require(patchwork)) install.packages("patchwork")
library(patchwork) # for side-by-side plots

# Clean up R's memory
rm(list = ls()) 

source("helpers.R")

# Read pre-processed participant data (n=548)
participants <- read.csv('../data/participants-preprocessed.csv')

# For easier access, separate participants into the three preference elicitation expressiveness groups: OD = low, RF = medium, ASF = high
twopointlGroup <- participants[participants$Elicitation == "OD", ]
threepointlGroup <- participants[participants$Elicitation == "RF", ]
nonlinearGroup <- participants[participants$Elicitation == "ASF", ]

######## Planned Analyses for H1 to H5 ########

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
plot_all      <- plotAllCI_fixedBreaks(to_plot, plotRange = c(-0.05, 1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
to_plot_diff  <- rbind(results.H.1[3:3, ])
plot_diff     <- plotDiffCI_fixedBreaks(to_plot_diff, plotRange = c(-0.12, 0.1), breaks=c(-0.1, 0, 0.1))
print(wrap_plots(plot_all, plot_diff, ncol=2, nrow=1, widths = c(2, 1)))

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
plot_all      <- plotAllCI_fixedBreaks(to_plot, plotRange = c(-0.05, 1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
to_plot_diff  <- rbind(results.H.2[3:3, ], results.H.2[6:6, ])
plot_diff     <- plotDiffCI_fixedBreaks(to_plot_diff, plotRange = c(-0.12, 0.1), breaks=c(-0.1, 0, 0.1))
print(wrap_plots(plot_all, plot_diff, ncol=2, nrow=1, widths = c(2, 1)))

# Joint plot for H1+H2
to_plot <- rbind(results.H.1[1:2, ], results.H.2[1:2, ], results.H.2[4:5, ])
plot_all <- plotAllCI_fixedBreaks(to_plot, plotRange = c(-0.05, 1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
to_plot_diff <- rbind(results.H.1[3:3, ], results.H.2[3:3, ], results.H.2[6:6, ])
plot_diff <- plotDiffCI_fixedBreaks(to_plot_diff, plotRange = c(-0.12, 0.1), breaks=c(-0.1, 0, 0.1))
print(wrap_plots(plot_all, plot_diff, ncol=2, nrow=1, widths = c(2, 1)))

#H3: "Difference of differences" between elicitation conditions
# Mean differences between PC-TV differences of low and medium expressiveness conditions 
results.H.3 <- diffOfDiffsCondition(participants, dataCol = "AccuracyDiff", groups = c("OD", "RF"), results.H.3)
# Mean differences between PC-TV differences of low and high expressiveness conditions 
results.H.3 <- diffOfDiffsCondition(participants, dataCol = "AccuracyDiff", groups = c("OD", "ASF"), results.H.3)
# Mean differences between PC-TV differences of medium and high expressiveness conditions 
results.H.3 <- diffOfDiffsCondition(participants, dataCol = "AccuracyDiff", groups = c("RF", "ASF"), results.H.3)

#Plot CIs for H3
plot_diffofdiffs  <- plotDiffCI_fixedBreaks(results.H.3, plotRange = c(-0.12, 0.15), breaks=c(-0.1, 0, 0.1)) 
print(plot_diffofdiffs)

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
plot_all      <- plotAllCI_fixedBreaks(to_plot, plotRange = c(-0.05, 1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
to_plot_diff  <- rbind(results.H.4[4:6, ])
plot_diff     <- plotDiffCI_fixedBreaks(to_plot_diff, plotRange = c(-0.12, 0.1), breaks=c(-0.1, 0, 0.1))
print(wrap_plots(plot_all, plot_diff, ncol=2, nrow=1, widths = c(2, 1)))

#H5: Variation of accuracy responses for each expressiveness condition
# Plot boxplots for each expressiveness condition
boxplots <- plotMeanAccuracyBoxplots(participants, plotRange=c(0,1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
print(boxplots)
# Numeric interquartile ranges for each expressiveness condition 
IQR(twopointlGroup$AccuracyMean)
IQR(threepointlGroup$AccuracyMean)
IQR(nonlinearGroup$AccuracyMean)