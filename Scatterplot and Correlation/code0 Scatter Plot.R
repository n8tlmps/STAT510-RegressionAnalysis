### Load necessary libraries
# Load the ggplot2 library for creating graphics and visualizations.
# Load the tidyverse library, which includes ggplot2 and other useful packages for data manipulation and visualization.
library(ggplot2)
library(tidyverse)
library(skimr)

# Load the HistData library, which contains historical datasets including Galton's Families dataset.
# install.packages("HistData")  # Uncomment this line if HistData is not installed.
library(HistData)

### Load data from HistData Package
# Load the GaltonFamilies dataset from the HistData package into the workspace.
data(GaltonFamilies)

# Assign the GaltonFamilies dataset to a new variable 'df' for easier reference.
df <- GaltonFamilies
skim(df)
# Save the dataset to a CSV file named "GaltonFamilies.csv" in the working directory.
write.csv(df, "GaltonFamilies.csv")

### Scatter plot via ggplot
# 'aes' defines the aesthetic mappings, specifying mother as the x-axis, childHeight as the y-axis, 
# and using different colors for gender.
ggplot(data = df, aes(x = mother, y = childHeight, color = gender)) + 
  geom_point() +  # Add points to the scatter plot.
  labs(x = "Mother's Height", y = "Child's Height") +  # Label the x and y axes.
  geom_smooth(method = "lm")  # Add a smooth line using local regression (loess) to show the trend.

# Filter the dataset to only include female children by subsetting and dropping unused factor levels.
df_F <- droplevels(df[df$gender == 'female',])
# or you can use filter
# df_F <- GaltonFamilies %>% filter(gender == "female")

# Create a scatter plot for only female children to visualize the relationship between mother's height and daughter's height.
ggplot(data = df_F, aes(x = mother, y = childHeight, color = gender)) + 
  geom_point() +  # Add points to the scatter plot.
  labs(x = "Mother's Height", y = "Child's Height") +  # Label the x and y axes.
  geom_smooth(method = "lm")  # Add a smooth line using local regression (loess) to show the trend.

### Correlation analysis
cor_midparent_child <- cor(df$midparentHeight, df$childHeight)

# Calculate and display the correlation between mother's height and daughter's height (filtered for females).
cor_mother_daughter <- cor(df_F$mother, df_F$childHeight)

# Calculate and display the correlation between father's height and son's height (filtered for males).
# This helps in understanding the relationship between father's height and male offspring height.
cor_father_son <- cor(df[df$gender == 'male',]$father, df[df$gender == 'male',]$childHeight)
cor_father_son <- cor(GaltonFamilies %>% filter(gender == "male") %>% pull(father),
                      GaltonFamilies %>% filter(gender == "male") %>% pull(childHeight))

# Display the calculated correlation coefficients for midparent-child, mother-daughter, and father-son height relationships
cat("Correlation between midparentHeight and childHeight:", cor_midparent_child, "\n")
cat("Correlation between mother and daughter height:", cor_mother_daughter, "\n")
cat("Correlation between father and son height:", cor_father_son, "\n")
