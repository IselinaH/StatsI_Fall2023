# Create a matrix for the observed frequencies
observed <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)

# Calculate the expected frequencies
row_totals <- apply(observed, 1, sum)
col_totals <- apply(observed, 2, sum)
grand_total <- sum(observed)
expected <- outer(row_totals, col_totals) / grand_total

# Calculate the chi-squared test statistic
chi_sq <- sum((observed - expected)^2 / expected)
chi_sq

# Calculate the p-value using the chi-squared distribution
p_value <- 1 - pchisq(chi_sq, df = 2)
p_value

# Calculate the standardized residuals
std_resid <- (observed - expected) / sqrt(expected * (1 - row_totals/grand_total) * (1 - col_totals/grand_total))

# Create a matrix for the standardized residuals
std_resid_mat <- matrix(std_resid, nrow = 2, byrow = TRUE)

# Add row and column names to the matrix
rownames(std_resid_mat) <- c("Upper class", "Lower class")
colnames(std_resid_mat) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")

# Print the matrix of standardized residuals
std_resid_mat

# Load the data
women <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

# Check for missing values in the water and reservation variables
sum(is.na(women$water))
sum(is.na(women$reservation))

# Remove rows with missing values
women <- na.omit(women)

# Calculate the mean number of new or repaired drinking water facilities for villages with female council heads
mean_female <- mean(women$water[women$reservation == 1])
print(mean_female)

# Calculate the mean number of new or repaired drinking water facilities for villages with male council heads
mean_male <- mean(women$water[women$reservation == 0])
print(mean_male)

# Calculate the difference in means
diff_means <- mean_female - mean_male
print(diff_means) 

# Check the column names of the women data frame
names(women)

# Check for missing values in the water and reservation variables
sum(is.na(women$water))
sum(is.na(women$reservation))

# Remove rows with missing values
women <- na.omit(women)

# Run a bivariate regression to test the hypothesis
reg <- lm(water ~ reservation, data = women)
summary(reg)

# Print the coefficient estimate for reservation policy
coef(reg)["reservation"]

