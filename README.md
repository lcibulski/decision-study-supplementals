# Evaluating Visual Decision Support: How Does Preference Elicitation Shape Metric Sensitivity?

This repository contains supplemental materials for the user study reported in the TVCG paper entitled "Evaluating Visual Decision Support: How Does Preference Elicitation Shape Metric Sensitivity?".

The preregistration for the study is available at https://osf.io/kw8uj.

The following materials are available in this repository:

- [Overview and additional analyses](Supplementals.pdf)
- [Questionnaire](Questionnaire.pdf) (cf. Section IV-D in the main paper)
- [Screen captures of interactive stimuli in questionnaire](interactive-stimuli/)
- [Raw and preprocessed data](data/) (preprocessed participant responses contain accuracy scores for chosen items, attention score, training scores, and Mini-VLAT score as extracted from raw data)
- [Data analysis scripts](data-analysis/) (planned analyses as reported in the main paper as well as planned and exploratory analyses as reported in the supplemental document)

## Reproducing the Results Reported in the Main Paper (Numbers in Section V-A + Figure 4)

The numbers and visuals underlying the results reported in Section V-A and Figure 4 of our paper can be reproduced with the analysis script ```./data-analysis/plannedAnalyses.R```. 
The generated plots were post-processed in [Inkscape](https://inkscape.org/) to obtain the final layout of Figure 4.

The script loads helper functions for computing and plotting confidence intervals and boxplots from ```./data-analysis/helpers.R```. 
It operates on the preprocessed data provided with ```./data/participants-prepocessed.csv```.
All required libraries are automatically installed and loaded.

To run the analysis, you need [R](https://cloud.r-project.org/) and [RStudio](https://docs.posit.co/ide/user/#rstudio-ide-oss-downloads) (version >= 2026.07.1+147) installed.
Open the ```./data-analysis/plannedAnalyses.R``` script in RStudio and run the entire document by pressing ```Ctrl+Shift+Enter``` (```Cmd+Shift+Enter``` on Mac) or using the "Source" button in the toolbar.
If the files imported via relative paths cannot be found right away, you might need to set the working directory to the source file location. To do so, right-click the script tab and choose "Set Working Directory" from the context menu or choose "Session -> Set Working Directory -> To Source File Location" in the RStudio menubar.

Numerical CI intervals (H1 to H4) and interquartile ranges (H5) as reported in Section V-A in the main paper are printed to the console. 
The plots underlying Figure 4 are generated one by one. You can inspect the generated plots by opening "View -> Show Plots" and step through the plots with the forward and backward arrows.

## Citation

The dataset and code are released under [![License: CC BY-SA 4.0](https://img.shields.io/badge/License-CC_BY--SA_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by-sa/4.0/).

If you use the dataset or code for your research, please cite this paper:

> L. Cibulski, T. Mertz, E. Dimara, and S. Bruckner (2026). Evaluating Visual Decision Support: How Does
Preference Elicitation Shape Metric Sensitivity? IEEE Transactions on Visualization and Computer Graphics.
