# This analysis script is inspired by the data analysis provided with the preregistration 
# "A Data Visualization Tale of Two Tails: Decoupling Judgment and Decision Making" (https://osf.io/t82nq)
# by Başak Oral, Evanthia Dimara, and Pierre Dragicevic, which was licensed under CC Attribution-ShareAlike

library(boot)
library(bootES)
library(ggplot2)
library(stringr)

digits  <- 2 # number of digits to show after the decimal point.

######## Helpers ########

# Pad a string with white spaces. Use NA for no padding and a negative width to pad right (align left).
pad <- function(s, width) {
  if (is.na(width))
    s
  else
    sprintf(paste('%', width, 's', sep=''), s)
}

# Rounding a number for textual presentation
format_pure_number<- function(n, sigdigits, width = NA) {
  signif(n,sigdigits)
}

# Present a number and its CI
format_ci <- function(x, sigdigits = NA, width = NA, percent = FALSE) {
  nwidth <- floor((width - 8) / 3)
  pad(paste(
    format_pure_number(x[1], sigdigits = sigdigits, width = nwidth),
    ', CI[',
    format_pure_number(x[2], sigdigits = sigdigits, width = nwidth),
    ',',
    format_pure_number(x[3], sigdigits = sigdigits, width = nwidth),
    ']',
    sep=''), width = width)
}

get_condition_abbr <- function(condition_str) {
  string_components <- str_split_1(condition_str, "\\.")
  condition_verbose <- string_components[1]
  if(condition_verbose == "twopointl") {
    return("OD")
  }
  else if(condition_verbose == "threepointl") {
    return("RF")
  } else {
    return("ASF")
  }
}

######## Statistical measures ########

# Mean of observations
samplemean <- function(x, d) {
  return(mean(x[d]))
}

# Compute the mean of non-normally distributed data and its 95% (bootstrap) confidence interval
meanCI <- function(observations) {
  mean <- samplemean(observations)
  # Compute the bootstrap confidence interval
  if(mean != 0){
    set.seed(0) # make deterministic
    bootstrap_samples <- boot(data = observations, statistic = samplemean, R = 5000)
    bootci <- boot.ci(bootstrap_samples, type = "bca")
    c(mean,  bootci$bca[4], bootci$bca[5])
  }else {
    c(mean, NA, NA)
  }
}

accuracyCondition <- function(condition, results) 
{
  label_lookup      <- data.frame(condition=c('twopointl.pc.accuracy', 'twopointl.tv.accuracy', 'twopointl.pc.accuracy - twopointl.tv.accuracy', 'threepointl.pc.accuracy', 'threepointl.tv.accuracy', 'threepointl.pc.accuracy - threepointl.tv.accuracy', 'nonlinear.pc.accuracy', 'nonlinear.tv.accuracy', 'nonlinear.pc.accuracy - nonlinear.tv.accuracy', 'twopointl.pc.accuracy_low', 'twopointl.tv.accuracy_low', 'twopointl.pc.accuracy_low - twopointl.tv.accuracy_low', 'twopointl.pc.accuracy_medium', 'twopointl.tv.accuracy_medium', 'twopointl.pc.accuracy_medium - twopointl.tv.accuracy_medium', 'twopointl.pc.accuracy_high', 'twopointl.tv.accuracy_high', 'twopointl.pc.accuracy_high - twopointl.tv.accuracy_high', 'threepointl.pc.accuracy_low', 'threepointl.tv.accuracy_low', 'threepointl.pc.accuracy_low - threepointl.tv.accuracy_low', 'threepointl.pc.accuracy_medium', 'threepointl.tv.accuracy_medium', 'threepointl.pc.accuracy_medium - threepointl.tv.accuracy_medium', 'threepointl.pc.accuracy_high', 'threepointl.tv.accuracy_high', 'threepointl.pc.accuracy_high - threepointl.tv.accuracy_high', 'nonlinear.pc.accuracy_low', 'nonlinear.tv.accuracy_low', 'nonlinear.pc.accuracy_low - nonlinear.tv.accuracy_low', 'nonlinear.pc.accuracy_medium', 'nonlinear.tv.accuracy_medium', 'nonlinear.pc.accuracy_medium - nonlinear.tv.accuracy_medium', 'nonlinear.pc.accuracy_high', 'nonlinear.tv.accuracy_high', 'nonlinear.pc.accuracy_high - nonlinear.tv.accuracy_high', 'twopointl.accuracy', 'threepointl.accuracy', 'nonlinear.accuracy', 'twopointl.accuracy_low', 'threepointl.accuracy_low', 'nonlinear.accuracy_low', 'twopointl.accuracy_medium', 'threepointl.accuracy_medium', 'nonlinear.accuracy_medium', 'twopointl.accuracy_high', 'threepointl.accuracy_high', 'nonlinear.accuracy_high', 'twopointl.pc.satisfaction', 'twopointl.tv.satisfaction', 'twopointl.pc.satisfaction - twopointl.tv.satisfaction', 'threepointl.pc.satisfaction', 'threepointl.tv.satisfaction', 'threepointl.pc.satisfaction - threepointl.tv.satisfaction', 'nonlinear.pc.satisfaction', 'nonlinear.tv.satisfaction', 'nonlinear.pc.satisfaction - nonlinear.tv.satisfaction', 'twopointl.pc.confidence', 'twopointl.tv.confidence', 'twopointl.pc.confidence - twopointl.tv.confidence', 'threepointl.pc.confidence', 'threepointl.tv.confidence', 'threepointl.pc.confidence - threepointl.tv.confidence', 'nonlinear.pc.confidence', 'nonlinear.tv.confidence', 'nonlinear.pc.confidence - nonlinear.tv.confidence', 'twopointl.pc.easiness', 'twopointl.tv.easiness', 'twopointl.pc.easiness - twopointl.tv.easiness', 'threepointl.pc.easiness', 'threepointl.tv.easiness', 'threepointl.pc.easiness - threepointl.tv.easiness', 'nonlinear.pc.easiness', 'nonlinear.tv.easiness', 'nonlinear.pc.easiness - nonlinear.tv.easiness', 'twopointl.pc.attachment', 'twopointl.tv.attachment', 'twopointl.pc.attachment - twopointl.tv.attachment', 'threepointl.pc.attachment', 'threepointl.tv.attachment', 'threepointl.pc.attachment - threepointl.tv.attachment', 'nonlinear.pc.attachment', 'nonlinear.tv.attachment', 'nonlinear.pc.attachment - nonlinear.tv.attachment', 'twopointl.pc.preference', 'twopointl.tv.preference', 'twopointl.pc.preference - twopointl.tv.preference', 'threepointl.pc.preference', 'threepointl.tv.preference', 'threepointl.pc.preference - threepointl.tv.preference', 'nonlinear.pc.preference', 'nonlinear.tv.preference', 'nonlinear.pc.preference - nonlinear.tv.preference'), label=c('PC', 'TV', 'PC-TV', 'PC', 'TV', 'PC-TV', 'PC', 'TV', 'PC-TV', 'PC (low)', 'TV (low)', 'PC-TV (low)', 'PC (medium)', 'TV (medium)', 'PC-TV (med.)', 'PC (high)', 'TV (high)', 'PC-TV (high)', 'PC (low)', 'TV (low)', 'PC-TV (low)', 'PC (medium)', 'TV (medium)', 'PC-TV (med.)', 'PC (high)', 'TV (high)', 'PC-TV (high)', 'PC (low)', 'TV (low)', 'PC-TV (low)', 'PC (medium)', 'TV (medium)', 'PC-TV (med.)', 'PC (high)', 'TV (high)', 'PC-TV (high)', 'low', 'medium', 'high', 'low', 'low', 'low', 'medium', 'medium', 'mediun', 'high', 'high', 'high', 'PC', 'TV', 'PC-TV', 'PC', 'TV', 'PC-TV', 'PC', 'TV', 'PC-TV', 'PC', 'TV', 'PC-TV', 'PC', 'TV', 'PC-TV', 'PC', 'TV', 'PC-TV','PC', 'TV', 'PC-TV', 'PC', 'TV', 'PC-TV', 'PC', 'TV', 'PC-TV', 'PC', 'TV', 'PC-TV', 'PC', 'TV', 'PC-TV', 'PC', 'TV', 'PC-TV', 'PC', 'TV', 'PC-TV', 'PC', 'TV', 'PC-TV', 'PC', 'TV', 'PC-TV'))
  accuracyCI        <- meanCI(condition)  
  accuracyCIform    <- format_ci( accuracyCI, digits, width = NA, TRUE)
  condition_str <- deparse(substitute(condition))
  labelText         <- sprintf("%s (%s)", label_lookup$label[label_lookup$condition==condition_str], get_condition_abbr(condition_str)) 
  cat("\n Mean CI " ,  sprintf("%s | %s", accuracyCIform, labelText))
  results           <- rbind(results, data.frame(label = labelText , ci.point = accuracyCI[1], ci.lower = accuracyCI[2], ci.upper = accuracyCI[3], cond = get_condition_abbr(condition_str)))
}

# According to bootES documentation (https://www.rdocumentation.org/packages/bootES/versions/1.3.0/topics/bootES)
diffOfDiffsCondition <- function(condition, dataCol, groups, results)
{
  set.seed(0) # make deterministic
  accuracyCI        <- bootES(condition, data.col=dataCol, group.col = "Elicitation", contrast=groups, R = 5000)
  labelText         <- sprintf("%s-%s", groups[2], groups[1]) 
  df                <- data.frame(label = labelText, ci.point = accuracyCI["t0"], ci.lower = accuracyCI[["bounds"]][1], ci.upper = accuracyCI[["bounds"]][2], cond = labelText)
  names(df)         <- c('label', 'ci.point', "ci.lower", "ci.upper", "cond")
  results           <- rbind(results, df)
}

######## Plotting ########

# Plot accuracy CIs for a hypothesis
plotAllCI_fixedBreaks <- function(results, plotRange = c(-1, 1), breaks) {
  rows.to.plot <- 1:(nrow(results)) # plot all but the within-pair consistency score
  # Simplify the ggplot visual theme
  theme_set(theme_bw() + theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#999999", linewidth = 0.1),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    legend.key.size = unit(5,"mm"), 
    plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"), #top, right, bottom, left 
    text = element_text(color="#888888", size=15), # break labels 0,0.1,0.2, ...
    rect = element_rect(fill = "transparent")
  )) 
  
  # Color scheme for condition
  color_palette <- c('OD' = '#b3e2cd', 'RF' = '#fdcdac', 'ASF' = '#cbd5e8')
  
  # Plot all confidence intervals
  p <- ggplot(data = results, aes(x = rev(rows.to.plot), y = ci.point, label=label, fill=cond)) +
    geom_col(width=0.5) +
    geom_pointrange(aes(ymin=ci.lower, ymax=ci.upper), size=0.7, linewidth=1.2) + # draws CI as line with circle for mean
    geom_point(size=3, fill="#000000", colour="#ffffff", shape=21, stroke=1.2) +
    geom_text(color="#000000",y=plotRange[1], hjust=1, size=4, fontface="bold", nudge_y = 200) + # TODO: nudge_y should apply an offset positioning
    geom_abline(intercept = 1, slope = 0, lty = 3, size=1) +
    scale_y_continuous(limits = plotRange,  breaks=breaks, expand = expansion(add = c(0.17,0.05))) + # set x-domain to plot range [0,1] rather than [x_min,x_max]
    coord_flip() +
    scale_fill_manual(values = color_palette, limits = names(color_palette)) +
    guides(fill="none")
}

# Plot mean difference CIs for a hypothesis
plotDiffCI_fixedBreaks <- function(results, plotRange = c(-0.2, 0.2), breaks) {
  rows.to.plot <- 1:(nrow(results)) # plot all but the within-pair consistency score
  # Simplify the ggplot visual theme
  theme_set(theme_bw() + theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#999999", linewidth = 0.1),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    legend.key.size = unit(5,"mm"), 
    plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"), #top, right, bottom, left 
    text = element_text(color="#888888", size=15), # break labels 0,0.1,0.2, ...
    rect = element_rect(fill = "transparent")
  )) 
  
  # Plot all confidence intervals
  p <- ggplot(data = results, aes(x = rev(rows.to.plot), y = ci.point, label=label)) +
    geom_pointrange(aes(ymin=ci.lower, ymax=ci.upper), size=0.7, linewidth=1.2) + # draws CI as line with circle for mean
    geom_point(size=3, fill="#000000", colour="#ffffff", shape=21, stroke=1.2) +
    geom_text(color="#000000",y=plotRange[1], hjust=1, size=4, fontface="bold", nudge_y = 200) + # TODO: nudge_y should apply an offset positioning
    geom_abline(intercept = 0, slope = 0, lty = 3, size=1) +
    scale_y_continuous(limits = plotRange,  breaks=breaks, expand = expansion(add = c(0.17,0.05))) + # set x-domain to plot range [0,1] rather than [x_min,x_max]
    coord_flip()
}

plotMeanAccuracyBoxplots <- function(results, plotRange = c(-0.2, 0.2), breaks) {
  rows.to.plot <- 1:(nrow(results)) # plot all but the within-pair consistency score
  # Simplify the ggplot visual theme
  theme_set(theme_bw() + theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#999999", linewidth = 0.1),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    legend.key.size = unit(5,"mm"), 
    plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"), #top, right, bottom, left 
    text = element_text(color="#888888", size=15), # break labels 0,0.1,0.2, ...
    rect = element_rect(fill = "transparent")
  )) 
  
  p <- ggplot(results, aes(x = Elicitation, y = AccuracyMean)) +
    geom_boxplot() + 
    xlab("Preference Elicitation Expressiveness") +
    ylab("Accuracy") +
    scale_y_continuous(limits = plotRange,  breaks=breaks) +
    coord_flip() +
    theme_minimal()
} 