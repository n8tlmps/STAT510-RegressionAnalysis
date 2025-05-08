library(tidyverse)
library(tidyr)
library(car)
library(MASS)  # Added for Box-Cox transformation

### Log Transformation for Linearity (survival.csv)
# Load data
sur <- read_csv("survival.csv") %>% na.omit()

# Initial Plot
ggplot(sur, aes(x = time, y = survival)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle("Initial Plot of Survival vs Time")

# Model with original data
model0 <- lm(survival ~ time, data = sur)
summary(model0)

# Plot diagnostics for model0
par(mfrow = c(2, 2))
plot(model0)
par(mfrow = c(1, 1))  # Reset plot layout

# Apply Box-Cox transformation
bc <- boxcox(lm(survival ~ time, data = sur))
lambda <- bc$x[which.max(bc$y)]
cat("Optimal lambda for survival:", lambda, "\n")

# Log-transformed model
model1 <- lm(log(survival) ~ time, data = sur)
summary(model1)

# Plot diagnostics for model1
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

# Compare AIC values between original and log-transformed model
cat("AIC Comparison for Survival Models:\n")
print(AIC(model0, model1))


### Skewness Adjustment (mamal.csv)
# Load data
mam <- read_csv("mamal.csv") %>% na.omit()

# Initial Plot
ggplot(mam, aes(x = body_weight, y = brain_weight)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle("Initial Plot of Brain Weight vs Body Weight")

# Model with original data
model0_mam <- lm(brain_weight ~ body_weight, data = mam)
summary(model0_mam)
# Plot diagnostics for model1
par(mfrow = c(2, 2))
plot(model0_mam)
par(mfrow = c(1, 1))


# Apply log-log transformation for skewness
model1_mam <- lm(log(brain_weight) ~ log(body_weight), data = mam)
summary(model1_mam)

# Plot diagnostics for log-log transformed model
par(mfrow = c(2, 2))
plot(model1_mam)
par(mfrow = c(1, 1))

# Apply Box-Cox transformation
bc_mam <- boxcox(lm(brain_weight ~ body_weight, data = mam))
lambda_mam <- bc_mam$x[which.max(bc_mam$y)]
cat("Optimal lambda for brain_weight:", lambda_mam, "\n")

# Compare AIC values between original and log-log model
cat("AIC Comparison for Brain Weight Models:\n")
print(AIC(model0_mam, model1_mam))


### Box-Cox Transformations (supervisor.csv)
# Load data
sup <- read_csv("supervisor.csv") %>% na.omit()

# Initial Plot
ggplot(sup, aes(x = W, y = S)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle("Initial Plot of S vs W")

# Model with original data
model0_sup <- lm(S ~ W, data = sup)
summary(model0_sup)

# Plot diagnostics for model0_sup
par(mfrow = c(2, 2))
plot(model0_sup)
par(mfrow = c(1, 1))

# Apply Box-Cox transformation
bc_sup <- boxcox(lm(S ~ W, data = sup))
lambda_sup <- bc_sup$x[which.max(bc_sup$y)]
cat("Optimal lambda for S:", lambda_sup, "\n")

# Model with Box-Cox transformed S
model1_sup <- lm(((S^lambda_sup - 1) / lambda_sup) ~ W, data = sup)
summary(model1_sup)
par(mfrow = c(2, 2))
plot(model1_sup)
par(mfrow = c(1, 1))

# Log-log transformed model
model2_sup <- lm(log(S) ~ log(W), data = sup)
summary(model2_sup)
par(mfrow = c(2, 2))
plot(model2_sup)
par(mfrow = c(1, 1))

# Quadratic model with Box-Cox transformation
model3_sup <- lm(log(S) ~ W + I(W^2), data = sup)
summary(model3_sup)
par(mfrow = c(2, 2))
plot(model3_sup)
par(mfrow = c(1, 1))

# Compare AIC values across all models
cat("AIC Comparison for Supervisor Models:\n")
print(AIC(model0_sup, model1_sup, model2_sup, model3_sup))
