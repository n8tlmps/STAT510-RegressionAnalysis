# Load necessary library
library(readr)

# Read the data
data <- read_csv('salary_data_new.csv')
data$education <- as.factor(data$education)
# (1) Use all features
model1 <- lm(salary ~ ., data = data)
summary(model1)

# (2) Use only age with 2nd order polynomial
model2 <- lm(salary ~ poly(age, 2,raw = TRUE), data = data)
summary(model2)

# (3) Use only Midterm with 2nd order polynomial
model3 <- lm(salary ~ poly(Midterm, 3,raw = TRUE), data = data)
summary(model3)

# (4) Use both age and Midterm with 2nd order polynomial
model4 <- lm(salary ~ poly(age, 2, raw = TRUE) + poly(Midterm, 2, raw = TRUE), data = data)
summary(model4)

# (4-1) Use both age and Midterm with 2nd order polynomial with interaction
model4 <- lm(salary ~ poly(age, 2, raw = TRUE)*poly(Midterm, 2, raw = TRUE), data = data)
summary(model4)

# (5) Use both age and Midterm with 2nd order polynomial and education
data$education <- as.factor(data$education)
model5 <- lm(salary ~ poly(age, 2,raw = TRUE) + poly(Midterm, 2,raw = TRUE) + education, data = data)
summary(model5)
