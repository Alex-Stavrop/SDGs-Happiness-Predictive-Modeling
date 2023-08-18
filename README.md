# Analysis of Sustainable Development Goals (SDGs) & World Happiness Indicator

## Overview

This project embarks on a detailed exploration to discern the relationship between the variables utilized to gauge the Sustainable Development Goals (SDGs) and the World Happiness Indicator. The primary objective was to predict the World Happiness Indicator for various countries for the year 2015. This analysis utilizes data from The World Bank data repository and The World Happiness Report.

## Data

The data is derived from The World Bank data repository, which encompasses relevant indicators derived from the World Development Indicators, structured in alignment with the goals and targets of the SDGs. This is complemented by The World Happiness Report, which aggregates survey data from over 150 countries, shedding light on how individuals evaluate their lives. 

After rigorous cleaning and preprocessing, two primary datasets emerged:
1. First dataset: 112 observations and 23 variables.
2. Second dataset: 50 observations and 86 variables.

Each dataset's entry corresponds to the World Happiness Index score and specific indicators for a country in 2015.

## Methodology

Several analytical and predictive methods were employed:

1. **Principal Component Analysis (PCA)**: Used for dimensionality reduction, especially for datasets with a high number of features per observation. 
2. **Lasso Regression**: Introduced as a method to swap bias for variance, optimizing the mean square error using penalization.
3. **Principal Component Regression (PCR)**: A regression technique based on PCA.
4. **Statistical Hypothesis Tests**: This analysis integrated the Permutation test and Bootstrapping to reinforce the accuracy and confidence in the selected principal components.

## Results

PCA emerged as a pivotal tool in determining the number of components for a reduced model. The Permutation Test and Kaiser’s rule were harmonious in suggesting that the first three principal components be retained, accounting for over 80% of the total variance in the data.

Two primary models, Lasso Regression and PCR, were developed and tested. While both showcased commendable predictive capabilities, the Lasso Regression exhibited a marginally superior performance in terms of Root Mean Square Deviation (RMSD)
## Conclusions

The analysis underscores a tangible relationship between the SDGs variables and the World Happiness Index. Both employed models, PCR and Lasso Regression, demonstrated robust predictive prowess. The nuanced performance differences between the models indicate their adaptability and efficiency in diverse datasets.
