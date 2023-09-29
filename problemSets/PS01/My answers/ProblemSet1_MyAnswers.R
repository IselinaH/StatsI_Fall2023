# Question 1:
1.
# Given data
data <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
n <- length(data)
confidence_level <- 0.90

# Calculate mean and standard deviation
mean_value <- mean(data)
std_dev <- sd(data)

# Determine t-critical value
t_critical <- qt((1 + confidence_level) / 2, df = n - 1)

# Calculate Margin of Error
margin_error <- t_critical * (std_dev / sqrt(n))

# Calculate Confidence Interval
confidence_interval <- c(mean_value - margin_error, mean_value + margin_error)

# Print the results
cat("Sample Mean:", mean_value, "\n")
cat("Margin of Error:", margin_error, "\n")
cat("90% Confidence Interval:", confidence_interval[1], "to", confidence_interval[2], "\n")

2. 
# Given data
data <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
national_average <- 100  # Average IQ score for all schools in the country
confidence_level <- 0.95  # Significance level

# Perform one-sample t-test
t_test_result <- t.test(data, mu = national_average, alternative = "greater", conf.level = confidence_level)

# Print the results
cat("Test Statistic:", t_test_result$statistic, "\n")
cat("P-value:", t_test_result$p.value, "\n")
cat("Degrees of Freedom:", t_test_result$parameter, "\n")
cat("95% Confidence Interval:", t_test_result$conf.int[1], "to", t_test_result$conf.int[2], "\n")

# Check the hypothesis based on the p-value
if (t_test_result$p.value < 0.05) {
  cat("Reject the null hypothesis. There is evidence that the average IQ is higher than 100.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence that the average IQ is higher than 100.\n")
}


# Question 2:
1. Import
# Load the data from the URL
url <- "https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt"
data <- read.table(url, header = TRUE)

# Display the first few rows of the dataset
head(data)

2. Explore
# Display the structure of the dataset
str(data)

# Display summary statistics
summary(data)

# Display the first few rows of the dataset
head(data)

# Display the last few rows of the dataset
tail(data)

3.Plot relationships among Y, X1, X2 and X3
# Scatter plot for Y and X1
plot(data$Y, data$X1, xlab = "Y (Expenditure)", ylab = "X1 (Personal Income)", main = "Scatter Plot: Y vs X1")

# Scatter plot for Y and X2
plot(data$Y, data$X2, xlab = "Y (Expenditure)", ylab = "X2 (Financially Insecure)", main = "Scatter Plot: Y vs X2")

# Scatter plot for Y and X3
plot(data$Y, data$X3, xlab = "Y (Expenditure)", ylab = "X3 (Urban Population)", main = "Scatter Plot: Y vs X3")

# Scatter plot for X1 and X2
plot(data$X1, data$X2, xlab = "X1 (Personal Income)", ylab = "X2 (Financially Insecure)", main = "Scatter Plot: X1 vs X2")

# Scatter plot for X1 and X3
plot(data$X1, data$X3, xlab = "X1 (Personal Income)", ylab = "X3 (Urban Population)", main = "Scatter Plot: X1 vs X3")

# Scatter plot for X2 and X3
plot(data$X2, data$X3, xlab = "X2 (Financially Insecure)", ylab = "X3 (Urban Population)", main = "Scatter Plot: X2 vs X3")

4.Correlations
# Calculate correlation matrix
cor_matrix <- cor(data[, c("Y", "X1", "X2", "X3")])

# Print correlation matrix
print(cor_matrix)

5. Plot relationship between Y and Region
# Boxplot for Y and Region
boxplot(data$Y ~ data$Region, xlab = "Region", ylab = "Y (Expenditure)", main = "Boxplot: Y vs Region")

# Calculate average per capita expenditure for each region
avg_expenditure_by_region <- tapply(data$Y, data$Region, mean)

# Print the average expenditures by region
cat("Average Per Capita Expenditure by Region:\n")
print(avg_expenditure_by_region)

6. Highest per capita expenditure on housing assistance
# Identify the region with the highest average expenditure
max_region <- names(avg_expenditure_by_region[which.max(avg_expenditure_by_region)])
cat("Region with the Highest Average Expenditure:", max_region, "\n")

7. Relationship between Y and X1 (AGAIN Ref.3)
# Scatter plot for Y and X1
plot(data$X1, data$Y, xlab = "X1 (Personal Income)", ylab = "Y (Expenditure)", main = "Scatter Plot: Y vs X1")

8. Include one more variable Region and display different regions with different types of symbols and colors.
# Scatter plot for Y, X1, and Region
plot(data$X1, data$Y, xlab = "X1 (Personal Income)", ylab = "Y (Expenditure)", 
     main = "Scatter Plot: Y vs X1 with Region", col = data$Region, pch = data$Region)

# Add legend
legend("topright", legend = levels(factor(data$Region)), col = 1:4, pch = 1:4, title = "Region")

