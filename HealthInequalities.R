######################################################
## Estimating changes in health inequalities across ##
## three diseases in England                        ##
## by Zoe Raffan (2417171R)                         ##               
######################################################

###################
## LOAD PACKAGES ##
###################

library(tidyverse)
library(sf)
library(tmap)
library(maps)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggthemes)
library(cowplot)
library(mgcv)
library(spdep)

#########################
## LOAD AND CLEAN DATA ##
#########################

# Set working directory to folder containing all required files and the R script.

shp_data <- st_read("shp_data_UK.geojson")
ethnicity <- read.csv("ethnicity.csv")
deprivation <- read.csv("income_dep.csv")
SMR <- read.csv("SMR.csv")
smokers <- read.csv("prevalence_smokers.csv")

SMR <- SMR %>%
  select(ltla, cancer_smr, circulatory_smr, heart_smr) %>%
  # find mean smr for each disease
  mutate(cancer_avg = mean(cancer_smr),
         circulatory_avg = mean(circulatory_smr),
         heart_avg = mean(heart_smr)) %>%
  # standardise smr rates by dividing by mean
  mutate(cancer_smr = cancer_smr/cancer_avg,
         circulatory_smr = circulatory_smr/circulatory_avg,
         heart_smr = heart_smr/heart_avg) %>%
  # remove old columns, not required
  select(ltla,cancer_smr,circulatory_smr,heart_smr) %>%
  mutate(cancer_smr = round(cancer_smr,2),
         circulatory_smr = round(circulatory_smr,2),
         heart_smr = round(heart_smr,2))

# join all datasets
SMR <- SMR %>%
  left_join(deprivation) %>%
  left_join(ethnicity) %>%
  left_join(smokers) %>%
  filter(ltla != "Isles of Scilly")

shp_data <- shp_data %>%
  filter(!grepl("^S|^W",LAD21CD)) %>%
  rename(ltla = LAD21NM) %>%
  filter(ltla != "Isles of Scilly") %>%
  left_join(SMR)

################################################
## DOES DISEASE RISK PATTERN VARY BY DISEASE? ##
################################################

## England Maps ##

tm_shape(shp_data) +
  tm_borders(lwd = 0.5) +
  tm_layout(main.title="Map of England",
            main.title.position = c("center"), main.title.size = 2, main.title.fontface = "bold")

scale <- c(0.4, 0.8, 1, 1.4, 1.6, 1.8) 

tm_shape(shp_data) + 
  tm_fill("circulatory_smr", id="ltla",
          palette = c("#ffffb2","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026"),
          breaks = scale,
          title = "") +
  tm_borders(lwd = 0.5) +
  tm_layout(main.title="Circulatory disease SMR for each\nLTLA in England, 2020", 
            legend.format = list(text.separator = "-"),
            main.title.position = c("center"), main.title.size = 2, main.title.fontface = "bold", 
            legend.text.size = 1.5)

tm_shape(shp_data) + 
  tm_fill("cancer_smr", id="ltla",
          palette = c("#ffffcc","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"),
          breaks = scale,
          title = "") + 
  tm_borders(lwd = 0.5) +
  tm_layout(main.title="Cancer disease SMR for each\nLTLA in England, 2020", 
            legend.format = list(text.separator = "-"),
            main.title.position = c("center"), main.title.size = 2, main.title.fontface = "bold", 
            legend.text.size = 1.5)

tm_shape(shp_data) +
  tm_fill("heart_smr", id="ltla",
          palette = c("#feebe2","#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177"),
          breaks = scale,
          title = "") +
  tm_borders(lwd = 0.5) +
  tm_layout(main.title="Heart disease SMR for each\nLTLA in England, 2020", 
            legend.format = list(text.separator = "-"),
            main.title.position = c("center"), main.title.size = 2, main.title.fontface = "bold", 
            legend.text.size = 1.5)

## London Maps ##

london_ltlas <- c("Barking and Dagenham","Barnet","Bexley","Brent","Bromley","Camden","Croydon","Ealing","Enfield","Greenwich",
                  "Hackney","Hammersmith and Fulham","Haringey","Harrow","Havering","Hillingdon","Hounslow","Islington","Kensington and Chelsea",
                  "Kingston upon Thames","Lambeth","Lewisham","Merton","Newham","Redbridge","Richmond upon Thames","Southwark","Sutton",
                  "Tower Hamlets","Waltham Forest","Wandsworth","Westminster","City of London")

londo_shp <- shp_data %>%
  filter(ltla %in% london_ltlas)

tm_shape(londo_shp) + 
  tm_fill("circulatory_smr", id="LAD21NM",
          palette = c("#ffffb2","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026"),
          breaks = scale,
          title = "") +
  tm_borders(lwd = 0.5) +
  tm_layout(main.title="Circulatory disease SMR for each\nLTLA in London, 2020", 
            legend.format = list(text.separator = "-"),
            main.title.position = c("center"), main.title.size = 1.9, main.title.fontface = "bold", 
            legend.text.size = 1.3)

tm_shape(londo_shp) + 
  tm_fill("cancer_smr", id="LAD21NM",
          palette = c("#ffffcc","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"),
          breaks = scale,
          title = "") +
  tm_borders(lwd = 0.5) +
  tm_layout(main.title="Cancer disease SMR for each\nLTLA in London, 2020", 
            legend.format = list(text.separator = "-"),
            main.title.position = c("center"), main.title.size = 1.9, main.title.fontface = "bold", 
            legend.text.size = 1.3)

tm_shape(londo_shp) + 
  tm_fill("heart_smr", id="LAD21NM",
          palette = c("#feebe2","#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177"),
          breaks = scale,
          title = "") +
  tm_borders(lwd = 0.5) +
  tm_layout(main.title="Heart disease SMR for each\nLTLA in London, 2020", 
            legend.format = list(text.separator = "-"),
            main.title.position = c("center"), main.title.size = 1.9, main.title.fontface = "bold", 
            legend.text.size = 1.3)

##########################################################
## HOW DOES INEQUALITY IN DISEASE RISK VARY BY DISEASE? ##
##########################################################

## Boxplots by disease ##

# calculate interquartile range for each disease
IQR_cancer <- IQR(SMR$cancer_smr)
IQR_heart <- IQR(SMR$heart_smr)
IQR_circulatory <- IQR(SMR$circulatory_smr)

# Determine the maximum range among all diseases
max_range <- max(max(SMR$cancer_smr), max(SMR$heart_smr), max(SMR$circulatory_smr))
min_range <- min(min(SMR$cancer_smr), min(SMR$heart_smr), min(SMR$circulatory_smr))

# create 3 boxplots of SMR for each disease
boxplot_cancer <- ggplot(SMR, aes(y = cancer_smr, fill = "Cancer")) +
  geom_boxplot() +
  labs(y = "Cancer SMR", subtitle = paste("IQR =", round(IQR_cancer, 2))) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_fill_manual(values = c("#225ea8")) +
  guides(fill = FALSE) +
  ylim(min_range, max_range)

boxplot_heart <- ggplot(SMR, aes(y = heart_smr, fill = "Heart")) +
  geom_boxplot() +
  labs(y = "Heart disease SMR", subtitle = paste("IQR =", round(IQR_heart, 2))) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_fill_manual(values = c("#ae017e")) +
  guides(fill = FALSE) +
  ylim(min_range, max_range)

boxplot_circulatory <- ggplot(SMR, aes(y = circulatory_smr, fill = "Circulatory")) +
  geom_boxplot() +
  labs(y = "Circulatory disease SMR", subtitle = paste("IQR =", round(IQR_circulatory, 2))) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_fill_manual(values = c("#e31a1c")) +
  guides(fill = FALSE) +
  ylim(min_range, max_range)

# Arrange plots horizontally
combined_plots <- arrangeGrob(
  boxplot_cancer,
  boxplot_circulatory,
  boxplot_heart,
  ncol = 3,
  top = textGrob("Boxplots Illustrating SMR Variation by Disease", gp = gpar(fontsize = 20, fontface = "bold")))

# Draw the combined plots
grid.draw(combined_plots)

##############################################################
## WHAT ARE THE AREAS WITH THE HIGHEST/LOWEST DISEASE RISK? ##
##############################################################

## lowest SMR for each disease ##
head(arrange(SMR,cancer_smr))
head(arrange(SMR,circulatory_smr))
head(arrange(SMR,heart_smr))

## highest smr for each disease ##
head(arrange(SMR,desc(cancer_smr)))
head(arrange(SMR,desc(circulatory_smr)))
head(arrange(SMR,desc(heart_smr)))

#######################################
## WHICH FACTORS DRIVE DISEASE RISK? ##
#######################################

## Income Deprivation Score ##

# Calculate the maximum and minimum values of y across all scatterplots
max_y <- max(max(SMR$cancer_smr), max(SMR$heart_smr), max(SMR$circulatory_smr))
min_y <- min(min(SMR$cancer_smr), min(SMR$heart_smr), min(SMR$circulatory_smr))

# Plot a scatterplot of deprivation vs cancer_smr
depriv_cancer <- ggplot(SMR, aes(x = income_score, y = cancer_smr)) +
  geom_point(color = "#225ea8", size = 2) +
  labs(x = "Mean Income Deprivation Score", y = "Cancer SMR") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 14)) +
  ylim(min_y, max_y)

# Plot a scatterplot of deprivation vs heart_smr
depriv_heart <- ggplot(SMR, aes(x = income_score, y = heart_smr)) +
  geom_point(color = "#ae017e", size = 2) +
  labs(x = "Mean Income Deprivation Score", y = "Heart disease SMR") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 14)) +
  ylim(min_y, max_y)

# Plot a scatterplot of deprivation vs circulatory_smr
depriv_circulatory <- ggplot(SMR, aes(x = income_score, y = circulatory_smr)) +
  geom_point(color = "#e31a1c", size = 2) +
  labs(x = "Mean Income Deprivation Score", y = "Circulatory disease SMR") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 14)) +
  ylim(min_y, max_y)

# Arrange scatterplots horizontally
grid.arrange(depriv_cancer, depriv_circulatory, depriv_heart, ncol = 3)

# Add a common title
grid.text("Scatterplots of each disease's SMR and Income Deprivation Score",
          x = 0.5, y = 0.98, just = "center", gp = gpar(fontsize = 20, fontface = "bold"))

## Smoking Prevalence ##

# Calculate the maximum and minimum values of y across all scatterplots
max_y <- max(max(SMR$cancer_smr), max(SMR$heart_smr), max(SMR$circulatory_smr))
min_y <- min(min(SMR$cancer_smr), min(SMR$heart_smr), min(SMR$circulatory_smr))

# Plot a scatterplot of smokers vs cancer_smr
smokers_cancer <- ggplot(SMR, aes(x = log(percentage_prevalence_smokers), y = cancer_smr)) +
  geom_point(color = "#225ea8", size = 2) +
  labs(x = "log(Proportion of Smokers (%))", y = "Cancer SMR") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 14)) +
  ylim(min_y, max_y)

# Plot a scatterplot of smokers vs heart_smr
smokers_heart <- ggplot(SMR, aes(x = log(percentage_prevalence_smokers), y = heart_smr)) +
  geom_point(color = "#ae017e", size = 2) +
  labs(x = "log(Proportion of Smokers (%))", y = "Heart disease SMR") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 14)) +
  ylim(min_y, max_y)

# Plot a scatterplot of smokers vs circulatory_smr
smokers_circulatory <- ggplot(SMR, aes(x = log(percentage_prevalence_smokers), y = circulatory_smr)) +
  geom_point(color = "#e31a1c", size = 2) +
  labs(x = "log(Proportion of Smokers (%))", y = "Circulatory disease SMR") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 14)) +
  ylim(min_y, max_y)

# Arrange scatterplots horizontally
grid.arrange(smokers_cancer, smokers_circulatory, smokers_heart, ncol = 3)

# Add a common title
grid.text("Scatterplots of each disease's SMR and log(Smoking Prevalence)",
          x = 0.5, y = 0.98, just = "center", gp = gpar(fontsize = 20, fontface = "bold"))

## Ethnicity ##

# Calculate the maximum and minimum values of y across all scatterplots
max_y <- max(max(SMR$cancer_smr), max(SMR$heart_smr), max(SMR$circulatory_smr))
min_y <- min(min(SMR$cancer_smr), min(SMR$heart_smr), min(SMR$circulatory_smr))

# Plot scatterplots for Asian ethnic groups
asian_cancer <- ggplot(SMR, aes(x = log(asian_ethnicity), y = cancer_smr)) +
  geom_point(color = "#0c2c84", size = 3, alpha = 0.7) +
  labs(x = "log(% of Population)",
       y = "Cancer SMR") +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 12)) +
  ylim(min_y, max_y)

asian_circulatory <- ggplot(SMR, aes(x = log(asian_ethnicity), y = circulatory_smr)) +
  geom_point(color = "#b10026", size = 3, alpha = 0.7) +
  labs(x = "log(% of Population)",
       y = "Circulatory Disease SMR") +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 12)) +
  ylim(min_y, max_y)

asian_heart <- ggplot(SMR, aes(x = log(asian_ethnicity), y = heart_smr)) +
  geom_point(color = "#7a0177", size = 3, alpha = 0.7) +
  labs(x = "log(% of Population)",
       y = "Heart Disease SMR") +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 12)) +
  ylim(min_y, max_y)

# Plot scatterplots for Black ethnic groups
black_cancer <- ggplot(SMR, aes(x = log(black_ethnicity), y = cancer_smr)) +
  geom_point(color = "#0c2c84", size = 3, alpha = 0.7) +
  labs(x = "log(% of Population)",
       y = "Cancer SMR") +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 12)) +
  ylim(min_y, max_y)

black_circulatory <- ggplot(SMR, aes(x = log(black_ethnicity), y = circulatory_smr)) +
  geom_point(color = "#b10026", size = 3, alpha = 0.7) +
  labs(x = "log(% of Population)",
       y = "Circulatory Disease SMR") +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 12)) +
  ylim(min_y, max_y)

black_heart <- ggplot(SMR, aes(x = log(black_ethnicity), y = heart_smr)) +
  geom_point(color = "#7a0177", size = 3, alpha = 0.7) +
  labs(x = "log(% of Population)",
       y = "Heart Disease SMR") +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 12)) +
  ylim(min_y, max_y)

## Scatterplots for Asian Ethnicity
grid.arrange(asian_cancer, asian_circulatory, asian_heart, ncol = 3)
grid.text("SMR vs the Population for the Asian Ethnic Group",
          x = 0.5, y = 0.98, just = "center", gp = gpar(fontsize = 18, fontface = "bold"))

## Scatterplots for Black Ethnicity
grid.arrange(black_cancer, black_circulatory, black_heart, ncol = 3)
grid.text("SMR vs the Population for the Black Ethnic Group",
          x = 0.5, y = 0.98, just = "center", gp = gpar(fontsize = 18, fontface = "bold"))

#####################
## FORMAL ANALYSIS ##
#####################

## Load in and clean new modelling dataset
SMR_model <- read.csv("SMR.csv")

SMR_model <- SMR_model %>%
  filter(ltla != "Isles of Scilly") %>%
  select(ltla, cancer_total_expected, circ_total_expected, heart_total_expected, cancer_observed, circulatory_observed, heart_observed) %>%
  left_join(deprivation) %>%
  left_join(ethnicity) %>%
  left_join(smokers) %>%
  mutate_if(is.integer, as.numeric)

## Neighbourhood Matrix: Common Border Spec ##
sf_use_s2(FALSE)
W.nb <- poly2nb(shp_data, row.names = shp_data$ltla)
W.nb[[44]] <- as.integer(113)
W.nb[[113]] <- as.integer(c(112, 44))
W <- nb2mat(W.nb, style = "B")
class(W)
dim(W)
W[1:308, 1:308]

## Average number of neighbours to use as k-nearest neighbours
mean(card(W.nb))

## Neighbourhood Matrix: K-Nearest neighbours using centroid distance ##
coords <- st_centroid(shp_data)$geometry
knn <- 5
nb.test <- knn2nb(knearneigh(coords, k=knn), row.names=shp_data$ltla)
W.nb <- make.sym.nb(nb.test)
summary(W.nb)
W <- nb2mat(W.nb, style = "B")
class(W)
dim(W)
W[1:5, 1:5]

## make symmetric for car modelling
diag(W) <- 0
W <- W + t(W)

## Plot the neighbourhood structure
#plot(shp_data$geometry)
#plot.nb(W.nb, coords=st_centroid(shp_data)$geometry, col="blue", add=TRUE)

## Compute Moran's I and test for significance (CHANGE FOR EACH DISEASE)
W.list <- nb2listw(W.nb, style = "B")
moran.mc(x = shp_data$heart_smr, listw = W.list, nsim = 10000) # significant spatial autocorrelation as p-value<0.05 

## Fit Simplest Model: GLM (CHANGE FOR EACH DISEASE!)
glm_model <- glm(heart_observed ~ income_score + asian_ethnicity + black_ethnicity + percentage_prevalence_smokers,
                 data = SMR_model, family = poisson(link = "log"), offset = log(heart_total_expected))
summary(glm_model)

## Interpretation of coefficients and their CIs ##

# Extract coefficients, standard errors, and confidence intervals from the summary
coef_summary <- summary(glm_model)$coefficients

# Format the results
formatted_results <- data.frame(
  Variable = rownames(coef_summary),
  Mean = coef_summary[, "Estimate"],
  SE = coef_summary[, "Std. Error"]
)

# Compute lower and upper bounds of 95% confidence intervals
z_value <- qnorm(0.975)  # for 95% confidence interval (two-tailed)
formatted_results$Lower <- formatted_results$Mean - z_value * formatted_results$SE
formatted_results$Upper <- formatted_results$Mean + z_value * formatted_results$SE

# Select only the columns needed
final_output <- formatted_results[, c("Variable", "Mean", "Lower", "Upper")]

sd(SMR_model$income_score)
sd(SMR_model$asian_ethnicity)
sd(SMR_model$black_ethnicity)
sd(SMR_model$percentage_prevalence_smokers)

multiplication_factors <- c(5, 9, 5, 7)  # For income_score, asian_ethnicity, black_ethnicity, percentage_prevalence_smokers respectively

# Multiply the mean, lower, and upper bounds by the multiplication factors
final_output[, c("Mean", "Lower", "Upper")] <- final_output[, c("Mean", "Lower", "Upper")] * multiplication_factors

# Exponentiate the mean and confidence intervals
final_output[, c("Mean", "Lower", "Upper")] <- exp(final_output[, c("Mean", "Lower", "Upper")])

# Print the final output
print(final_output)

## Check for Multicollinearity ##

## variance inflation factor
vif_values <- car::vif(glm_model) ## needs to use glm function to work
print(vif_values) # no large VIF values, do not exceed 2. Would be a sign of multicollinearity if they exceeded 5.

## correlation matrix
cor_matrix <- cor(SMR_model[, c("income_score", "asian_ethnicity", "black_ethnicity", "percentage_prevalence_smokers")])
print(cor_matrix) # no highly correlated variables.

eigenvalues <- eigen(cor_matrix)$values
print(eigenvalues) # no large eigenvalues

## Check if GLM model accounts for Spatial Autocorrelation ##
moran.mc(x = residuals(glm_model), listw = W.list, nsim = 10000) # significant spatial autocorrelation as p-value<0.05

## Fit CAR model using CARBayes package ##

## Fit Poisson log-linear ICAR model using CARBayes package (CHANGE FORM FOR EACH DISEASE!)
library(CARBayes)

form <- cancer_observed ~ income_score + asian_ethnicity + black_ethnicity + percentage_prevalence_smokers + 
  offset(log(cancer_total_expected))

leroux_model <- S.CARleroux(formula=form, family="poisson", data=SMR_model, W=W, burnin=100000, n.sample=500000, thin=40)
print(leroux_model)

## LEROUX - check trace and density of the parameters
plot(leroux_model$samples$beta)
plot(leroux_model$samples$tau2)
plot(leroux_model$samples$rho)

## Both plots seem to indicate that the MCMC algorithm used to estimate the parameters has
## successfully converged. This means the algorithm has reached a stable distribution and
## explores the sample space efficiently, so no changes need to be made to the algorithm.
## The Gewekes diagnostic values can also confirm good convergence. Though income_score has
## quite a high Geweke value, it is still within the acceptable range.

## check variance of the covariates
sd(SMR_model$income_score)
sd(SMR_model$asian_ethnicity)
sd(SMR_model$black_ethnicity)
sd(SMR_model$percentage_prevalence_smokers)

round(exp(5 * leroux_model$summary.results[2, 1:3]),3) # LEROUX - income_score
round(exp(9 * leroux_model$summary.results[3, 1:3]),3) # LEROUX - asian_ethnicity
round(exp(5 * leroux_model$summary.results[4, 1:3]),3) # LEROUX - black_ethnicity
round(exp(7 * leroux_model$summary.results[5, 1:3]),3) # LEROUX - percentage prev smokers

## Next fit a multivariate space model using the CARBayes package because of correlation
## between the observed deaths (repsponses) for the different diseases. 

## correlation between diseases
cor(SMR[, c("cancer_smr", "circulatory_smr", "heart_smr")])

## Reload RStudio - CARBayes package has an issue where select function doesn't work after loading it in.

observed_deaths <- SMR_model %>%
  select(cancer_observed, circulatory_observed, heart_observed) %>%
  as.matrix()

expected_deaths <- SMR_model %>%
  select(cancer_total_expected, circ_total_expected, heart_total_expected) %>%
  as.matrix()

form_multivariate <- observed_deaths ~ income_score + asian_ethnicity + black_ethnicity + percentage_prevalence_smokers + 
  offset(log(expected_deaths))

library(CARBayes)

MVS_model <- MVS.CARleroux(formula=form_multivariate, family="poisson", data=SMR_model, 
                           W=W, rho=1, burnin=100000, n.sample=500000, thin=40)
print(MVS_model)

## CANCER ESTIMATION
round(exp(5 * MVS_model$summary.results[2, 1:3]),3) # CANCER - income_score
round(exp(9 * MVS_model$summary.results[3, 1:3]),3) # CANCER - asian_ethnicity
round(exp(5 * MVS_model$summary.results[4, 1:3]),3) # CANCER - black_ethnicity
round(exp(7 * MVS_model$summary.results[5, 1:3]),3) # CANCER - percentage prev smokers

## CIRCULATORY ESTIMATION
round(exp(5 * MVS_model$summary.results[7, 1:3]),3) # CIRC - income_score
round(exp(9 * MVS_model$summary.results[8, 1:3]),3) # CIRC - asian_ethnicity
round(exp(5 * MVS_model$summary.results[9, 1:3]),3) # CIRC - black_ethnicity
round(exp(7 * MVS_model$summary.results[10, 1:3]),3) # CIRC - percentage prev smokers

## HEART ESTIMATION
round(exp(5 * MVS_model$summary.results[12, 1:3]),3) # HEART - income_score
round(exp(9 * MVS_model$summary.results[13, 1:3]),3) # HEART - asian_ethnicity
round(exp(5 * MVS_model$summary.results[14, 1:3]),3) # HEART - black_ethnicity
round(exp(7 * MVS_model$summary.results[15, 1:3]),3) # HEART - percentage prev smokers

plot(MVS_model$samples$beta)

## find estimated disease risk with fitted values/expected deaths
fitted_values <- fitted(MVS_model)
fitted_values <- as.data.frame(fitted_values)
fitted_values <- cbind(SMR_model, fitted_values)
fitted_values <- fitted_values %>%
  rename(cancer_fitted = V1,
         circulatory_fitted = V2,
         heart_fitted = V3) %>%
  mutate(cancer_risk = cancer_fitted / cancer_total_expected,
         circulatory_risk = circulatory_fitted / circ_total_expected,
         heart_risk = heart_fitted / heart_total_expected,
         cancer_risk = round(cancer_risk, 2),
         circulatory_risk = round(circulatory_risk, 2),
         heart_risk = round(heart_risk, 2)) %>%
  select(ltla, cancer_risk, circulatory_risk, heart_risk, income_score, asian_ethnicity, 
         black_ethnicity, percentage_prevalence_smokers)

fitted_values_shape <- fitted_values %>%
  select(ltla, cancer_risk, circulatory_risk, heart_risk)

shp_data_model <- st_read("shp_data_UK.geojson")
shp_data_model <- shp_data_model %>%
  filter(!grepl("^S|^W",LAD21CD)) %>%
  rename(ltla = LAD21NM) %>%
  filter(ltla != "Isles of Scilly") %>%
  left_join(fitted_values_shape) %>%
  select(-c(OBJECTID, LAD21CD, GlobalID))

#######################################################
## DOES DISEASE RISK PATTERN VARY BY DISEASE? PART 2 ##
#######################################################

## England Maps Redone for New estimated risk ##

scale <- c(0.4, 0.8, 1, 1.4, 1.6, 1.8)

tm_shape(shp_data_model) + 
  tm_fill("cancer_risk", id="ltla",
          palette = c("#ffffcc","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"),
          breaks = scale,
          title = "") + 
  tm_borders(lwd = 0.5) +
  tm_layout(main.title="Estimated Cancer Risk for each\nLTLA in England", 
            legend.format = list(text.separator = "-"),
            main.title.position = c("center"), main.title.size = 2, main.title.fontface = "bold", 
            legend.text.size = 1.5)

tm_shape(shp_data_model) +
  tm_fill("circulatory_risk", id="ltla",
          palette = c("#ffffb2","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026"),
          breaks = scale,
          title = "") + 
  tm_borders(lwd = 0.5) +
  tm_layout(main.title="Estimated Circulatory Disease \nRisk for each LTLA in England", 
            legend.format = list(text.separator = "-"),
            main.title.position = c("center"), main.title.size = 2, main.title.fontface = "bold", 
            legend.text.size = 1.5)

tm_shape(shp_data_model) +
  tm_fill("heart_risk", id="ltla",
          palette = c("#feebe2","#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177"),
          breaks = scale,
          title = "") + 
  tm_borders(lwd = 0.5) +
  tm_layout(main.title="Estimated Heart Disease Risk \nfor each LTLA in England", 
            legend.format = list(text.separator = "-"),
            main.title.position = c("center"), main.title.size = 2, main.title.fontface = "bold", 
            legend.text.size = 1.5)

## London Maps for new estimated risk ##

london_ltlas <- c("Barking and Dagenham","Barnet","Bexley","Brent","Bromley","Camden","Croydon","Ealing","Enfield","Greenwich",
                  "Hackney","Hammersmith and Fulham","Haringey","Harrow","Havering","Hillingdon","Hounslow","Islington","Kensington and Chelsea",
                  "Kingston upon Thames","Lambeth","Lewisham","Merton","Newham","Redbridge","Richmond upon Thames","Southwark","Sutton",
                  "Tower Hamlets","Waltham Forest","Wandsworth","Westminster","City of London")

londo_shp_model <- shp_data_model %>%
  filter(ltla %in% london_ltlas)

tm_shape(londo_shp_model) + 
  tm_fill("cancer_risk", id="ltla",
          palette = c("#ffffcc","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"),
          breaks = scale,
          title = "") +
  tm_borders(lwd = 0.5) +
  tm_layout(main.title="Estimated Cancer Risk for each\nLTLA in London", 
            legend.format = list(text.separator = "-"),
            main.title.position = c("center"), main.title.size = 1.9, main.title.fontface = "bold", 
            legend.text.size = 1.3)

tm_shape(londo_shp_model) + 
  tm_fill("circulatory_risk", id="ltla",
          palette = c("#ffffb2","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026"),
          breaks = scale,
          title = "") +
  tm_borders(lwd = 0.5) +
  tm_layout(main.title="Estimated Circulatory Disease \nRisk for each LTLA in London", 
            legend.format = list(text.separator = "-"),
            main.title.position = c("center"), main.title.size = 1.9, main.title.fontface = "bold", 
            legend.text.size = 1.3)

tm_shape(londo_shp_model) + 
  tm_fill("heart_risk", id="ltla",
          palette = c("#feebe2","#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177"),
          breaks = scale,
          title = "") +
  tm_borders(lwd = 0.5) +
  tm_layout(main.title="Estimated Heart Disease Risk \nfor each LTLA in London", 
            legend.format = list(text.separator = "-"),
            main.title.position = c("center"), main.title.size = 1.9, main.title.fontface = "bold", 
            legend.text.size = 1.3)

#################################################################
## HOW DOES INEQUALITY IN DISEASE RISK VARY BY DISEASE? PART 2 ##
#################################################################

## Boxplots ##

# calculate interquartile range for each disease
IQR_cancer <- IQR(fitted_values$cancer_risk)
IQR_heart <- IQR(fitted_values$heart_risk)
IQR_circulatory <- IQR(fitted_values$circulatory_risk)

# Determine the maximum range among all diseases
max_range <- max(max(fitted_values$cancer_risk), max(fitted_values$heart_risk), max(fitted_values$circulatory_risk))
min_range <- min(min(fitted_values$cancer_risk), min(fitted_values$heart_risk), min(fitted_values$circulatory_risk))

# create 3 boxplots of SMR for each disease
boxplot_cancer <- ggplot(fitted_values, aes(y = cancer_risk, fill = "Cancer")) +
  geom_boxplot() +
  labs(y = "Cancer Risk", subtitle = paste("IQR =", round(IQR_cancer, 2))) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_fill_manual(values = c("#225ea8")) +
  guides(fill = FALSE) +
  ylim(min_range, max_range)

boxplot_heart <- ggplot(fitted_values, aes(y = heart_risk, fill = "Heart")) +
  geom_boxplot() +
  labs(y = "Heart disease Risk", subtitle = paste("IQR =", round(IQR_heart, 2))) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_fill_manual(values = c("#ae017e")) +
  guides(fill = FALSE) +
  ylim(min_range, max_range)

boxplot_circulatory <- ggplot(fitted_values, aes(y = circulatory_risk, fill = "Circulatory")) +
  geom_boxplot() +
  labs(y = "Circulatory disease Risk", subtitle = paste("IQR =", round(IQR_circulatory, 2))) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5)) +
  scale_fill_manual(values = c("#e31a1c")) +
  guides(fill = FALSE) +
  ylim(min_range, max_range)

# Arrange plots horizontally
combined_plots <- arrangeGrob(
  boxplot_cancer,
  boxplot_circulatory,
  boxplot_heart,
  ncol = 3,
  top = textGrob("Boxplots Illustrating the Variation in Estimated Risk by Disease", gp = gpar(fontsize = 20, fontface = "bold")))

# Draw the combined plots
grid.draw(combined_plots)

#####################################################################
## WHAT ARE THE AREAS WITH THE HIGHEST/LOWEST DISEASE RISK? PART 2 ##
#####################################################################

# lowest estimated risk for each disease
head(arrange(fitted_values,cancer_risk))
head(arrange(fitted_values,circulatory_risk))
head(arrange(fitted_values,heart_risk))

## highest estimated risk for each disease
head(arrange(fitted_values,desc(cancer_risk)))
head(arrange(fitted_values,desc(circulatory_risk)))
head(arrange(fitted_values,desc(heart_risk)))

##############################################
## WHICH FACTORS DRIVE DISEASE RISK? PART 2 ##
##############################################

print(MVS_model)

## CANCER ESTIMATION
round(exp(5 * MVS_model$summary.results[2, 1:3]),3) # CANCER - income_score
round(exp(9 * MVS_model$summary.results[3, 1:3]),3) # CANCER - asian_ethnicity
round(exp(5 * MVS_model$summary.results[4, 1:3]),3) # CANCER - black_ethnicity
round(exp(7 * MVS_model$summary.results[5, 1:3]),3) # CANCER - percentage prev smokers

## CIRCULATORY ESTIMATION
round(exp(5 * MVS_model$summary.results[7, 1:3]),3) # CIRC - income_score
round(exp(9 * MVS_model$summary.results[8, 1:3]),3) # CIRC - asian_ethnicity
round(exp(5 * MVS_model$summary.results[9, 1:3]),3) # CIRC - black_ethnicity
round(exp(7 * MVS_model$summary.results[10, 1:3]),3) # CIRC - percentage prev smokers

## HEART ESTIMATION
round(exp(5 * MVS_model$summary.results[12, 1:3]),3) # HEART - income_score
round(exp(9 * MVS_model$summary.results[13, 1:3]),3) # HEART - asian_ethnicity
round(exp(5 * MVS_model$summary.results[14, 1:3]),3) # HEART - black_ethnicity
round(exp(7 * MVS_model$summary.results[15, 1:3]),3) # HEART - percentage prev smokers