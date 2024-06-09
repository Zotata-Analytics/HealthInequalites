# Overview

This repository contains an R script for analysing the spatial and soco-economic factors influencing disease risk patterns in England. The study’s overall goal is to use the variation in disease risk for cancer, circulatory
disease, and coronary heart disease, to estimate the severity of health inequalities across England. To do this, we will focus on these specific aims:
- Evaluate if the pattern of disease risk in England varies between cancer, circulatory disease, and coronary heart disease.
- Evaluate how inequality in disease risk differs between each three diseases.
- Determine which areas have the highest and lowest risk for each disease, and if these are common across all three diseases.
- Investigate which factors drive disease risk and if they vary by disease.

The first part of the script aims to join covariates onto a pre-existing dataset containing the Standardised Morbitity Ratio (SMR) for each Lower Tier Local Authority Area (LTLA) in England. These covariates are listed as: ethnicity, deprivation and percentage smokers prevalence. The second part of the report is the Exploratory analysis, where the goal is to investigate any variable relationships which may show themselves through a variety of graphical and numerical summaries. The last part, a formal analysis, aims to find estimates of each diseases' risk with more precision through the comparison of two types of Conditional AutoRegressive models, the Leroux CAR model and a multivarite Leroux CAR model.

## Required R packages

- tidyverse
- sf
- tmap
- maps
- ggplot2
- gridExtra
- grid
- ggthemes
- cowplot
- mgcv
- spdep

## Part One: Joining the Datasets

The deaths and population data required to calculate the SMR for each LTLA in England was acquired from the Office for National Statistics online, and is from the year 2020. The statistical release of the English indices of deprivation 2019 allowed the acquirement of income deprivation scores for the year 2019. Additionally, Asian and Black ethnic group data was acquired from the 2021 UK Census. Smoking prevalence data was also obtained from the Office for National Statistics. Keep note that due to the lack of public data online the data is not all from the same years, however, I tried to keep it as close as possible. The list of covariates in the final dataset for each cause of death can be described as follows:

- INCOME DERPIVATION SCORE: Measures the proportion of the population in an area experiencing deprivation relating to low income. The definition of low income includes both those people that are out-of-work, and those that are in work but who have low earnings.
- ASIAN ETHNICITY: Percentage of people within the LTLA who identify with the ethnic group “Asian, Asian British or Asian Welsh.” Includes subgroups Bangladeshi, Chinese, Indian, Pakistani and Other Asian.
- BLACK ETHNICITY: Percentage of people within the LTLA who identify with the ethnic group “Black, Black British, Black Welsh, Caribbean, or African.” Includes subgroups African, Caribbean, and other Black.
- SMOKING PREVALENCE: The percentage proportion of cigarette smokers by authority, as of the year 2020.

The shape file data is obtained from the Office for National Statistics and outlines all 309 LTLA borders on a map of England.

## Part Two: Exploratory Analysis

The R script creates:

- Maps of disease risk using SMR: Thematic maps of England and London, visualising SMR values for cancer, circulatory and heart diseases using the 'tmap' package.
- Boxplots: Three boxplots illustrating SMR variation by disease.
- An investigation into the top and bottom 5 LTLA areas in regards to SMR for cancer, circulatory and heart diseases.
- Scatterplots: Three scatterplots of SMR against each covariate to investigate early relationships between variables.

## Part Three: Formal Analysis

The R script follows a standard areal data spatial analysis, first by creating a neighbourhood matrix followed by completing a Moran's I test of spatial autocorrelation. Since the residuals of the simplest model (a Generalised Linear Model) show significant spatial autocorrelation, it is concluded that spatial modelling is needed for the data. Hence, univariate and multivariate spatial modelling is explored through the Leroux CAR model and the multivariate Leroux CAR model. Final estimates for disease risk is obtained for each disease, and hence the exploratory analysis can be repeated with a more accurate estimate to see if inference has changed.
