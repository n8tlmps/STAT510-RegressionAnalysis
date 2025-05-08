# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming the data is stored in a CSV file, load the dataset
data <- read.csv("salary_data.csv")

# Convert education to a factor if it's not already
data$education <- factor(data$education, levels = c("highschool", "bachelor", "advanced_degree"))

# Subset data by education level
data_highschool <- filter(data, education == "highschool")
data_bachelor <- filter(data, education == "bachelor")
data_advanced <- filter(data, education == "advanced_degree")

# Perform separate linear regressions for each education level
model_highschool <- lm(salary ~ age, data = data_highschool)
model_bachelor <- lm(salary ~ age, data = data_bachelor)
model_advanced <- lm(salary ~ age, data = data_advanced)

# View the summary of each model
summary(model_highschool)
summary(model_bachelor)
summary(model_advanced)

# Plot the data and regression lines for each education level
ggplot(data, aes(x = age, y = salary, color = education)) +
  geom_point() +
  geom_smooth(data = data_highschool, method = "lm", se = TRUE, color = "blue") +
  geom_smooth(data = data_bachelor, method = "lm", se = TRUE, color = "green") +
  geom_smooth(data = data_advanced, method = "lm", se = TRUE, color = "red") +
  labs(title = "Salary vs Age with Separate Regression Lines by Education Level",
       x = "Age", y = "Salary") +
  scale_color_manual(values = c("highschool" = "blue", "bachelor" = "green", "advanced_degree" = "red"),
                     labels = c("Highschool", "Bachelor", "Advanced Degree"),
                     name = "Education Level") +
  theme_minimal()
