# Load necessary libraries
library(car)  # For VIF calculation
library(readr)

# Load the dataset
data <- read_csv("housing_hw.csv")

# Fit a linear model using all predictors except the target variable (MEDV)
lm_model <- lm(MEDV ~ ., data = data)

# Calculate Variance Inflation Factor (VIF)
vif_values <- vif(lm_model)

# Print VIF values
print(vif_values)

# Identify predictors with high multicollinearity (VIF > 5 or 10 as thresholds)
high_vif <- vif_values[vif_values > 5]
print(high_vif)
