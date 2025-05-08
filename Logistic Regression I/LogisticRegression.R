# Load necessary libraries
library(caret)

# Read and preprocess the data
bank <- read.csv("UniversalBank.csv")
bank <- na.omit(bank)
bank <- subset(bank, select = -c(ID, ZIPCode))
bank$Education <- as.factor(bank$Education)

# Linear regression model
model0 <- lm(PersonalLoan ~ ., data = bank)
summary(model0)

# Logistic regression model
model1 <- glm(PersonalLoan ~ ., data = bank, family = "binomial")
summary(model1)
data.frame(summary(model1)$coefficients, odds = exp(coef(model1)))

# Predictions and confusion matrix for model1
pred <- predict(model1, bank, type = "response")
confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), as.factor(bank$PersonalLoan))

# Stepwise logistic regression model
model2 <- step(model1)
summary(model2)

# Predictions and confusion matrix for model2
pred2 <- predict(model2, bank, type = "response")
confusionMatrix(as.factor(ifelse(pred2 > 0.5, 1, 0)), as.factor(bank$PersonalLoan))
