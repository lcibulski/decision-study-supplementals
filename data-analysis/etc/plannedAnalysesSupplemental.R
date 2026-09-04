# This analysis script is inspired by the data analysis provided with the preregistration 
# "A Data Visualization Tale of Two Tails: Decoupling Judgment and Decision Making" (https://osf.io/t82nq)
# by Başak Oral, Evanthia Dimara, and Pierre Dragicevic, which was licensed under CC Attribution-ShareAlike

if(!require(patchwork)) install.packages("patchwork")
library(patchwork) # for side-by-side plots
if(!require(car)) install.packages("car")
library(car) # for recoding scale responses

# Clean up R's memory
rm(list = ls()) 

source("helpers.R")

# Read pre-processed participant data (n=548)
participants <- read.csv('../data/participants-preprocessed.csv')

######## H5 Variation of accuracy responses in response to varying expressiveness condition: One-Way ANOVA ########
anova_model <- aov(AccuracyMean ~ Elicitation, data = participants)
summary(anova_model)
plot(anova_model)

######## Planned Analyses for Subjective Choice Assessment and Technique Preference ########

# Read raw participant data (n=548)
participants <- read.csv('../data/participants-raw.csv')

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