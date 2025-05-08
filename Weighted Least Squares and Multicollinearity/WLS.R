library(tidyverse)
library(tidyr)
library(car)

df<- read.csv("housing_hw.csv") %>%
  na.omit()

# Fit initial OLS model
ols_model <- lm(MEDV ~ AGE + RM + TAX + INDUS, data =df)
df$residuals <- residuals(ols_model)
summary(ols_model)

par(mfrow = c(2, 2))
plot(ols_model)
par(mfrow = c(1, 1))  # Reset plot layout

# Calculate weights as the inverse of the squared residuals
df$weights <- 1 / (df$residuals^2)

# Fit the WLS model
wls_model <- lm(MEDV ~ AGE + RM + TAX + INDUS, data = df, weights = weights)

summary(wls_model)
par(mfrow = c(2, 2))
plot(wls_model)
par(mfrow = c(1, 1))  # Reset plot layout

# Extract standard errors of each coefficient

summary(ols_model)$coefficients[, "Std. Error"]
summary(wls_model)$coefficients[, "Std. Error"]
