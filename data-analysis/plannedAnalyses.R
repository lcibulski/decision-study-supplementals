# This analysis script is inspired by the data analysis provided with the preregistration 
# "A Data Visualization Tale of Two Tails: Decoupling Judgment and Decision Making" (https://osf.io/t82nq)
# by Başak Oral, Evanthia Dimara, and Pierre Dragicevic, which was licensed under CC Attribution-ShareAlike

library(patchwork) # for side-by-side plots
library(car) # for recoding scale responses

# Clean up R's memory
rm(list = ls()) 

source("measures.R")

# Read pre-processed participant data (n=548)
participants <- read.csv('../participants-preprocessed.csv')

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
# Numeric box plot statistics for each expressiveness condition
box_stats_low <- boxplot.stats(twopointlGroup$AccuracyMean)
print(box_stats_low$stats) 
box_stats_medium <- boxplot.stats(threepointlGroup$AccuracyMean)
print(box_stats_medium$stats) 
box_stats_high <- boxplot.stats(nonlinearGroup$AccuracyMean)
print(box_stats_high$stats) 
# Numeric interquartile ranges for each expressiveness condition 
IQR(twopointlGroup$AccuracyMean)
IQR(threepointlGroup$AccuracyMean)
IQR(nonlinearGroup$AccuracyMean)
# Fit a one-way ANOVA model
anova_model <- aov(AccuracyMean ~ Elicitation, data = participants)
summary(anova_model)
plot(anova_model)

######## Planned Analyses for Subjective Choice Assessment and Technique Preference ########

# Read raw participant data (n=548)
participants <- read.csv('../participants-raw.csv')

# Recode choice assessment metrics
# PC Satisfaction
participants$PT01_rec <- recode(participants$PT01, "2=1;1=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;3=11")
# PC Confidence
participants$PT02_rec <- recode(participants$PT02, "2=1;1=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;3=11")
# PC Easiness
participants$PT03_rec <- recode(participants$PT03, "2=1;1=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;3=11")
# PC Attachment
participants$PT04_rec <- recode(participants$PT04, "2=1;1=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;3=11")
# PC Preference
participants$PT06_rec <- recode(participants$PT06, "2=1;1=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;3=11")
# TV Satisfaction
participants$PT10_rec <- recode(participants$PT10, "2=1;1=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;3=11")
# TV Confidence
participants$PT11_rec <- recode(participants$PT11, "2=1;1=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;3=11")
# TV Easiness
participants$PT12_rec <- recode(participants$PT12, "2=1;1=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;3=11")
# TV Attachment
participants$PT13_rec <- recode(participants$PT13, "2=1;1=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;3=11")
# TV Preference
participants$PT07_rec <- recode(participants$PT07, "2=1;1=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;3=11")

# For easier access, separate participants into the three preference elicitation expressiveness groups: OD = low, RF = medium, ASF = high
twopointlGroup <- participants[participants$IV03_01 == "OD", ]
threepointlGroup <- participants[participants$IV03_01 == "RF", ]
nonlinearGroup <- participants[participants$IV03_01 == "ASF", ]

# Where to store results
results.satisfaction <- data.frame(label = character(0), ci.point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0), cond = character(0))
results.confidence <- data.frame(label = character(0), ci.point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0), cond = character(0))
results.easiness <- data.frame(label = character(0), ci.point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0), cond = character(0))
results.attachment <- data.frame(label = character(0), ci.point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0), cond = character(0))
results.preference <- data.frame(label = character(0), ci.point = numeric(0), ci.lower = numeric(0), ci.upper = numeric(0), cond = character(0))

# Satisfaction
twopointl.pc.satisfaction <- twopointlGroup$PT01_rec
twopointl.tv.satisfaction <- twopointlGroup$PT10_rec
results.satisfaction <- accuracyCondition(twopointl.pc.satisfaction, results.satisfaction) 
results.satisfaction <- accuracyCondition(twopointl.tv.satisfaction, results.satisfaction)
results.satisfaction <- accuracyCondition(twopointl.pc.satisfaction - twopointl.tv.satisfaction, results.satisfaction)

threepointl.pc.satisfaction <- threepointlGroup$PT01_rec
threepointl.tv.satisfaction <- threepointlGroup$PT10_rec
results.satisfaction <- accuracyCondition(threepointl.pc.satisfaction, results.satisfaction) 
results.satisfaction <- accuracyCondition(threepointl.tv.satisfaction, results.satisfaction)
results.satisfaction <- accuracyCondition(threepointl.pc.satisfaction - threepointl.tv.satisfaction, results.satisfaction)

nonlinear.pc.satisfaction <- nonlinearGroup$PT01_rec
nonlinear.tv.satisfaction <- nonlinearGroup$PT10_rec
results.satisfaction <- accuracyCondition(nonlinear.pc.satisfaction, results.satisfaction) 
results.satisfaction <- accuracyCondition(nonlinear.tv.satisfaction, results.satisfaction)
results.satisfaction <- accuracyCondition(nonlinear.pc.satisfaction - nonlinear.tv.satisfaction, results.satisfaction)

# Plot CIs for satisfaction
to_plot <- rbind(results.satisfaction[1:2, ], results.satisfaction[4:5, ], results.satisfaction[7:8, ])
plot_all<- plotAllCI_fixedBreaks(to_plot, plotRange = c(0, 10), breaks=c(0,1,2,3,4,5,6,7,8,9,10))
to_plot_diff <- rbind(results.satisfaction[3:3, ], results.satisfaction[6:6, ], results.satisfaction[9:9, ])
plot_diff    <- plotDiffCI_fixedBreaks(to_plot_diff, plotRange = c(-5, 5), breaks=c(-1,0,1))
wrap_plots(plot_all, plot_diff, ncol=2, nrow=1, widths = c(1, 1))

# Confidence
twopointl.pc.confidence <- twopointlGroup$PT02_rec
twopointl.tv.confidence <- twopointlGroup$PT11_rec
results.confidence <- accuracyCondition(twopointl.pc.confidence, results.confidence) 
results.confidence <- accuracyCondition(twopointl.tv.confidence, results.confidence)
results.confidence <- accuracyCondition(twopointl.pc.confidence - twopointl.tv.confidence, results.confidence)

threepointl.pc.confidence <- threepointlGroup$PT02_rec
threepointl.tv.confidence <- threepointlGroup$PT11_rec
results.confidence <- accuracyCondition(threepointl.pc.confidence, results.confidence) 
results.confidence <- accuracyCondition(threepointl.tv.confidence, results.confidence)
results.confidence <- accuracyCondition(threepointl.pc.confidence - threepointl.tv.confidence, results.confidence)

nonlinear.pc.confidence <- nonlinearGroup$PT02_rec
nonlinear.tv.confidence <- nonlinearGroup$PT11_rec
results.confidence <- accuracyCondition(nonlinear.pc.confidence, results.confidence) 
results.confidence <- accuracyCondition(nonlinear.tv.confidence, results.confidence)
results.confidence <- accuracyCondition(nonlinear.pc.confidence - nonlinear.tv.confidence, results.confidence)

# Plot CIs for confidence
to_plot <- rbind(results.confidence[1:2, ], results.confidence[4:5, ], results.confidence[7:8, ])
plot_all<- plotAllCI_fixedBreaks(to_plot, plotRange = c(0, 10), breaks=c(0,1,2,3,4,5,6,7,8,9,10))
to_plot_diff <- rbind(results.confidence[3:3, ], results.confidence[6:6, ], results.confidence[9:9, ])
plot_diff    <- plotDiffCI_fixedBreaks(to_plot_diff, plotRange = c(-5, 5), breaks=c(-1,0,1))
wrap_plots(plot_all, plot_diff, ncol=2, nrow=1, widths = c(1, 1))

# Easiness
twopointl.pc.easiness <- twopointlGroup$PT03_rec
twopointl.tv.easiness <- twopointlGroup$PT12_rec
results.easiness <- accuracyCondition(twopointl.pc.easiness, results.easiness) 
results.easiness <- accuracyCondition(twopointl.tv.easiness, results.easiness)
results.easiness <- accuracyCondition(twopointl.pc.easiness - twopointl.tv.easiness, results.easiness)

threepointl.pc.easiness <- threepointlGroup$PT03_rec
threepointl.tv.easiness <- threepointlGroup$PT12_rec
results.easiness <- accuracyCondition(threepointl.pc.easiness, results.easiness) 
results.easiness <- accuracyCondition(threepointl.tv.easiness, results.easiness)
results.easiness <- accuracyCondition(threepointl.pc.easiness - threepointl.tv.easiness, results.easiness)

nonlinear.pc.easiness <- nonlinearGroup$PT03_rec
nonlinear.tv.easiness <- nonlinearGroup$PT12_rec
results.easiness <- accuracyCondition(nonlinear.pc.easiness, results.easiness) 
results.easiness <- accuracyCondition(nonlinear.tv.easiness, results.easiness)
results.easiness <- accuracyCondition(nonlinear.pc.easiness - nonlinear.tv.easiness, results.easiness)

# Plot CIs for easiness
to_plot <- rbind(results.easiness[1:2, ], results.easiness[4:5, ], results.easiness[7:8, ])
plot_all<- plotAllCI_fixedBreaks(to_plot, plotRange = c(0, 10), breaks=c(0,1,2,3,4,5,6,7,8,9,10))
to_plot_diff <- rbind(results.easiness[3:3, ], results.easiness[6:6, ], results.easiness[9:9, ])
plot_diff    <- plotDiffCI_fixedBreaks(to_plot_diff, plotRange = c(-5, 5), breaks=c(-1,0,1))
wrap_plots(plot_all, plot_diff, ncol=2, nrow=1, widths = c(1, 1))

# Attachment
twopointl.pc.attachment <- twopointlGroup$PT04_rec
twopointl.tv.attachment <- twopointlGroup$PT13_rec
results.attachment <- accuracyCondition(twopointl.pc.attachment, results.attachment) 
results.attachment <- accuracyCondition(twopointl.tv.attachment, results.attachment)
results.attachment <- accuracyCondition(twopointl.pc.attachment - twopointl.tv.attachment, results.attachment)

threepointl.pc.attachment <- threepointlGroup$PT04_rec
threepointl.tv.attachment <- threepointlGroup$PT13_rec
results.attachment <- accuracyCondition(threepointl.pc.attachment, results.attachment) 
results.attachment <- accuracyCondition(threepointl.tv.attachment, results.attachment)
results.attachment <- accuracyCondition(threepointl.pc.attachment - threepointl.tv.attachment, results.attachment)

nonlinear.pc.attachment <- nonlinearGroup$PT04_rec
nonlinear.tv.attachment <- nonlinearGroup$PT13_rec
results.attachment <- accuracyCondition(nonlinear.pc.attachment, results.attachment) 
results.attachment <- accuracyCondition(nonlinear.tv.attachment, results.attachment)
results.attachment <- accuracyCondition(nonlinear.pc.attachment - nonlinear.tv.attachment, results.attachment)

# Plot CIs for attachment
to_plot <- rbind(results.attachment[1:2, ], results.attachment[4:5, ], results.attachment[7:8, ])
plot_all<- plotAllCI_fixedBreaks(to_plot, plotRange = c(0, 10), breaks=c(0,1,2,3,4,5,6,7,8,9,10))
to_plot_diff <- rbind(results.attachment[3:3, ], results.attachment[6:6, ], results.attachment[9:9, ])
plot_diff    <- plotDiffCI_fixedBreaks(to_plot_diff, plotRange = c(-5, 5), breaks=c(-1,0,1))
wrap_plots(plot_all, plot_diff, ncol=2, nrow=1, widths = c(1, 1))

# Technique Preference
twopointl.pc.preference <- twopointlGroup$PT06_rec
twopointl.tv.preference <- twopointlGroup$PT07_rec
results.preference <- accuracyCondition(twopointl.pc.preference, results.preference) 
results.preference <- accuracyCondition(twopointl.tv.preference, results.preference)
results.preference <- accuracyCondition(twopointl.pc.preference - twopointl.tv.preference, results.preference)

threepointl.pc.preference <- threepointlGroup$PT06_rec
threepointl.tv.preference <- threepointlGroup$PT07_rec
results.preference <- accuracyCondition(threepointl.pc.preference, results.preference) 
results.preference <- accuracyCondition(threepointl.tv.preference, results.preference)
results.preference <- accuracyCondition(threepointl.pc.preference - threepointl.tv.preference, results.preference)

nonlinear.pc.preference <- nonlinearGroup$PT06_rec
nonlinear.tv.preference <- nonlinearGroup$PT07_rec
results.preference <- accuracyCondition(nonlinear.pc.preference, results.preference) 
results.preference <- accuracyCondition(nonlinear.tv.preference, results.preference)
results.preference <- accuracyCondition(nonlinear.pc.preference - nonlinear.tv.preference, results.preference)

# Plot CIs for technique preference
to_plot <- rbind(results.preference[1:2, ], results.preference[4:5, ], results.preference[7:8, ])
plot_all<- plotAllCI_fixedBreaks(to_plot, plotRange = c(0, 10), breaks=c(0,1,2,3,4,5,6,7,8,9,10))
to_plot_diff <- rbind(results.preference[3:3, ], results.preference[6:6, ], results.preference[9:9, ])
plot_diff    <- plotDiffCI_fixedBreaks(to_plot_diff, plotRange = c(-5, 5), breaks=c(-1,0,1))
wrap_plots(plot_all, plot_diff, ncol=2, nrow=1, widths = c(1, 1))