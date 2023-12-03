# Install and load the car package
install.packages("car")
library(car)

# Load the Prestige dataset
data(Prestige)

# Create a new variable professional by recoding the variable type
Prestige$professional <- ifelse(Prestige$type == "professional", 1, 0)

# Run a linear model with prestige as the outcome and income, professional, and the interaction of the two as predictors
model <- lm(prestige ~ income * professional, data = Prestige)

# Write the prediction equation based on the result
summary(model)

# Interpret the coefficient for income
coefficients(summary(model))["income"]

# Interpret the coefficient for professional
coefficients(summary(model))["professional"]

# Calculate the effect of a $1,000 increase in income on prestige score for professional occupations
fitted_value <- fitted(model)
new_income <- 1000
new_prestige <- predict(model, newdata = data.frame(income = new_income, professional = 1))
effect <- new_prestige - fitted_value
effect

# Calculate the effect of changing one’s occupations from non-professional to professional when her income is $6,000
new_income <- 6000
new_prestige <- predict(model, newdata = data.frame(income = new_income, professional = 1))
old_prestige <- predict(model, newdata = data.frame(income = new_income, professional = 0))
effect <- new_prestige - old_prestige
effect

# Load the data
data <- data.frame(
  group = factor(c(rep("treatment", 30), rep("control", 76))),
  vote_share = c(0.042, 0.042, rep(NA, 104)),
  adjacent = c(rep(NA, 30), rep(1, 76))
)

# Run a linear regression
model <- lm(vote_share ~ group + adjacent, data = data)

# Summary of the regression
summary(model)

# Load the data
data <- data.frame(
  group = factor(c(rep("treatment", 30), rep("control", 76))),
  vote_share = c(0.042, 0.042, rep(NA, 104)),
  adjacent = c(rep(NA, 30), rep(1, 76))
)

# Run a linear regression
model <- lm(vote_share ~ group + adjacent, data = data)

# Summary of the regression
summary(model)

# Load the data
data <- data.frame(
  group = factor(c(rep("treatment", 30), rep("control", 76))),
  vote_share = c(0.042, 0.042, rep(NA, 104)),
  adjacent = c(rep(NA, 30), rep(1, 76))
)

# Run a linear regression
model <- lm(vote_share ~ group + adjacent, data = data)

# Summary of the regression
summary(model)

# Load the data
data <- data.frame(
  group = factor(c(rep("treatment", 30), rep("control", 76))),
  vote_share = c(0.042, 0.042, rep(NA, 104)),
  adjacent = c(rep(NA, 30), rep(1, 76))
)

# Run a linear regression
model <- lm(vote_share ~ group + adjacent, data = data)

# Evaluate the model fit
summary(model)