# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming the data is stored in a CSV file, load the dataset
data <- read.csv("salary_data.csv")

# Convert education levels to numeric values
data <- data %>%
  mutate(education_numeric = case_when(
    education == "highschool" ~ 1,
    education == "bachelor" ~ 2,
    education == "advanced_degree" ~ 3
  ))

# View the updated dataset
head(data)

# Perform linear regression using the numeric education variable
model_numeric <- lm(salary ~ age + education_numeric, data = data)

# View the summary of the model
summary(model_numeric)

# Get the variance-covariance matrix for the model coefficients
vcov_matrix_numeric <- vcov(model_numeric)

# Print the variance-covariance matrix
print("Variance-Covariance Matrix of Coefficients (Numeric Education):")
print(vcov_matrix_numeric)


# Create a scatter plot of salary vs. age with the numeric education variable
ggplot(data, aes(x = age, y = salary)) +
  geom_point(aes(color = as.factor(education_numeric))) +  # Color by numeric education levels
  geom_smooth(method = "lm", se = TRUE) +  # Add regression line
  labs(title = "Salary vs Age with Numeric Education Levels",
       x = "Age", y = "Salary") +
  scale_color_manual(values = c("1" = "blue", "2" = "green", "3" = "red"),
                     labels = c("Highschool", "Bachelor", "Advanced Degree"),
                     name = "Education Level") +  # Custom labels for education levels
  theme_minimal()