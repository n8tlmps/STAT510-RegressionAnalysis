# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming the data is stored in a CSV file, load the dataset
data <- read.csv("salary_data.csv")

data$education <-as.factor(data$education)
# Convert education to a factor and set "highschool" as the reference level
data$education <- factor(data$education, levels = c("bachelor", "advanced_degree","highschool"))

# Perform linear regression
model <- lm(salary ~ age + education, data = data)

# View the summary of the model
summary(model)

# Get the variance-covariance matrix for the model coefficients
vcov_matrix <- vcov(model)

# Print the variance-covariance matrix
print("Variance-Covariance Matrix of Coefficients:")
print(vcov_matrix)

# Predicting on the dataset (if required)
predictions <- predict(model, newdata = data)

# Create a scatter plot with regression lines for each education category
ggplot(data, aes(x = age, y = salary, color = education)) +
  geom_point() +
  geom_smooth(method = "lm", aes(fill = education), se = TRUE) +
  labs(title = "Salary vs Age with Regression Lines by Education Level",
       x = "Age", y = "Salary") +
  theme_minimal() +
  scale_color_manual(values = c("highschool" = "blue", "bachelor" = "green", "advanced_degree" = "red")) +
  scale_fill_manual(values = c("highschool" = "blue", "bachelor" = "green", "advanced_degree" = "red"))