# Load necessary libraries
library(tidyverse)
library(car)

# Load and clean data
house <- read_csv("housing_hw.csv") %>%
  na.omit()

# Full Model with all features
model_full <- lm(MEDV ~ ., data = house)
summary(model_full)

# Backward Elimination
model_backward <- step(lm(MEDV ~ ., data = house), direction = "backward")
summary(model_backward)

# Forward Selection
full_scope <- formula(lm(MEDV ~ ., data = house))
model_forward <- step(lm(MEDV ~ 1, data = house), scope = full_scope, direction = "forward")
summary(model_forward)

# Stepwise Selection (both directions)
model_stepwise <- step(lm(MEDV ~ 1, data = house), scope = full_scope, direction = "both")
summary(model_stepwise)
