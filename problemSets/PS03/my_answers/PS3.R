#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

# Run a regression where the outcome variable is voteshare and the explanatory variable is difflog
lm_model <- lm(voteshare ~ difflog, data = inc.sub)

# Make a scatterplot of the two variables and add the regression line
plot(inc.sub$difflog, inc.sub$voteshare, main="Scatterplot of difflog and voteshare with Regression Line")
abline(lm_model, col="red")

# Save the residuals of the model in a separate object
residuals_model <- resid(lm_model)

# Write the prediction equation
summary(lm_model)

# Dataset
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

# Run a regression where the outcome variable is presvote and the explanatory variable is difflog
lm_model <- lm(presvote ~ difflog, data = inc.sub)

# Make a scatterplot of the two variables and add the regression line
plot(inc.sub$difflog, inc.sub$presvote, main="Scatterplot of difflog and presvote with Regression Line")
abline(lm_model, col="red")

# Save the residuals of the model in a separate object
residuals_model <- resid(lm_model)

# Write the prediction equation
summary(lm_model)

# Dataset
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

# Run a regression where the outcome variable is voteshare and the explanatory variable is presvote
lm_model <- lm(voteshare ~ presvote, data = inc.sub)

# Make a scatterplot of the two variables and add the regression line
plot(inc.sub$presvote, inc.sub$voteshare, main="Scatterplot of presvote and voteshare with Regression Line")
abline(lm_model, col="red")

# Write the prediction equation
summary(lm_model)

# Load the residuals from Question 1 and Question 2
residuals_question1 <- c(-0.26832, -0.05345, -0.00377, 0.04780, 0.32749)  # Replace with actual residuals
residuals_question2 <- c(-0.32196, -0.07407, -0.00102, 0.07151, 0.42743)  # Replace with actual residuals

# Run a regression where the outcome variable is the residuals from Question 1 and the explanatory variable is the residuals from Question 2
lm_model_residuals <- lm(residuals_question1 ~ residuals_question2)

# Make a scatterplot of the two residuals and add the regression line
plot(residuals_question2, residuals_question1, main="Scatterplot of Residuals from Question 2 and Question 1 with Regression Line")
abline(lm_model_residuals, col="red")

# Write the prediction equation
summary(lm_model_residuals)

# Dataset
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

# Run a regression where the outcome variable is the incumbent's voteshare and the explanatory variables are difflog and presvote
lm_model <- lm(voteshare ~ difflog + presvote, data = inc.sub)

# Write the prediction equation
summary(lm_model)

# Identify the similarity with the output in Question 4