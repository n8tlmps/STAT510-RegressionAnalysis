# Load necessary libraries
library(ggplot2)
library(skimr)
# Load csv file
md <- read.csv("md.csv")
summary(md)
skim(md)
# Fit the linear regression model
model_youtube <- lm(sales ~ youtube, data = md)

# View the model summary
summary(model_youtube)
new_data <- data.frame(youtube=85.00)
pred_value <- predict(model_youtube, newdata = new_data) 


# Create a new data frame for prediction with unique column names
pred_conf <- predict(model_youtube, newdata = md, interval = "confidence", level = 0.95)
pred_pred <- predict(model_youtube, newdata = md, interval = "prediction", level = 0.95)

# Combine the predictions into the original dataset with unique names
md$fit <- pred_conf[, "fit"]
md$conf_lower <- pred_conf[, "lwr"]
md$conf_upper <- pred_conf[, "upr"]
md$pred_lower <- pred_pred[, "lwr"]
md$pred_upper <- pred_pred[, "upr"]

# Plot the scatter plot with regression line and confidence intervals
ggplot(md, aes(x = youtube, y = sales)) +
  geom_point() +  # Scatter plot
  geom_line(aes(y = fit), color = "red") +  # Regression line
  geom_line(aes(y = conf_lower), color = "blue", linetype = "dashed") +  # 95% CI Lower
  geom_line(aes(y = conf_upper), color = "blue", linetype = "dashed") +  # 95% CI Upper
  geom_line(aes(y = pred_lower), color = "green", linetype = "dotdash") +  #95% PRED Lower
  geom_line(aes(y = pred_upper), color = "green", linetype = "dotdash") +  #95% PRED Upper
  labs(title = "Linear Regression of Sales by Youtube",
       x = "Youtube",
       y = "Sales") +
  theme_minimal()

