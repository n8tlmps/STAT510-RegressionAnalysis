# Load necessary libraries
library(readr)
library(plotly)

# Step 1: Load the data
data <- read_csv("poisson_projects.csv")

# Step 2: Fit a Poisson Regression Model
model <- glm(num_projects ~ hours_spent + num_courses, data = data, family = poisson(link = "log"))

# Step 3: Make predictions
data$predicted_projects <- predict(model, type = "response")

# Step 4: Create one combined interactive 3D scatter plot
fig <- plot_ly() %>%
  add_markers(data = data, x = ~hours_spent, y = ~num_courses, z = ~num_projects,
              marker = list(color = 'blue'),
              name = 'Observed') %>%
  add_markers(data = data, x = ~hours_spent, y = ~num_courses, z = ~predicted_projects,
              marker = list(color = 'red', symbol = 'diamond'),
              name = 'Predicted') %>%
  layout(scene = list(
    xaxis = list(title = "Hours Spent"),
    yaxis = list(title = "Number of Courses"),
    zaxis = list(title = "Number of Projects")
  ),
  title = "Observed vs Predicted Projects (3D Interactive Plot)")

# Step 5: Show the plot
fig
