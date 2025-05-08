## install.packages('glmnet', dependencies=TRUE, type="binary")
## alpha=0 Ridge, alpha=1 Lasso (default)
library(glmnet)
library(tidyverse)
library(tidymodels)
library(tidyr)
library(car)
library(caret)



## Multiple Linear Regression
house <- read_csv("housing_hw.csv") %>% na.omit()

## Split Training/Test Data
house_sp <- house %>% initial_split(prop = 0.8, strata=MEDV)
rtrain <- house_sp %>% training()
rtest <- house_sp %>% testing()

model0 <- lm(MEDV ~ ., data=rtrain)
summary(model0)

## Regularization
xrtrain <- rtrain %>% dplyr::select(-MEDV) %>% as.matrix()
yrtrain <- rtrain$MEDV

xrtest  <- rtest %>% dplyr::select(-MEDV) %>% as.matrix()
yrtest  <- rtest$MEDV


###########################################################################
## LASSO: alpha = 1 (Default) --alpha = portion of L1 norm
###########################################################################
modelL <- glmnet(xrtrain, yrtrain)
plot(modelL,label = TRUE)
## Df=# of nonzero coefficients, %Dev = % of Deviance explained (like SSR)
print(modelL)
## Coefficient for a certain lambda
coef.glmnet(modelL, s= 0.1)


## Cross-Validation
## nfolds = # of groups ex) 20
## Regression with 19 groups and test with 1 group
cvmodelL <- cv.glmnet(xrtrain, yrtrain, type.measure = "mse", nfolds = 20)
coef.glmnet(modelL, s= cvmodelL$lambda.min)
plot(cvmodelL)



###########################################################################
## Ridge: alpha = 0 
###########################################################################
modelR <- glmnet(xrtrain, yrtrain, alpha=0)
plot(modelR,label = TRUE)
## Df=# of nonzero coefficients, %Dev = % of Deviance explained (like SSR)
print(modelR)
## Coefficient for a certain lambda
coef.glmnet(modelR, s= 0.1) ## s= lambda value (any)

## CrossValidation
cvmodelR <- cv.glmnet(xrtrain, yrtrain, alpha=0, type.measure = "mse", nfolds = 20)
plot(cvmodelR)
log(cvmodelR$lambda.min)



####### Test
pred0 <- predict(model0, newdata = rtest ,interval="confidence")
MSE0  <- mean((rtest$MEDV - pred0)^2)
predL <- predict(modelL, newx = xrtest, s=cvmodelL$lambda.min)
MSEL  <- mean((rtest$MEDV - predL)^2)
predR <- predict(modelR, newx = xrtest, s=cvmodelR$lambda.min)
MSER  <- mean((rtest$MEDV - predR)^2)



###########################################################################
## MultiCollinearity
###########################################################################
multi <- read_csv("Multi.csv") %>% na.omit()
multi_sp <- multi %>% initial_split(prop = 0.8, strata=Target)
mtrain <- multi_sp %>% training()
mtest <- multi_sp %>% testing()

xmtrain <- mtrain %>% dplyr::select(-Target) %>% as.matrix()
ymtrain <- mtrain$Target
xmtest  <- mtest %>% dplyr::select(-Target) %>% as.matrix()
ymtest  <- mtest$Target

modelM0 <- lm(Target ~ ., data=mtrain)
modelM0$coefficients
predM0 <- predict(modelM0, newdata = mtest ,interval="confidence")
MSEM0 <- mean((mtest$Target - predM0)^2)


modelMR <- glmnet(xmtrain, ymtrain, alpha=0)
cvmodelMR <- cv.glmnet(xmtrain, ymtrain, alpha=0, type.measure = "mse", nfolds = 5)

predMR <- predict(modelMR, newx = xmtest, s=cvmodelMR$lambda.min)
MSEMR <- mean((ymtest - predMR)^2)
coef.glmnet(modelMR, s= cvmodelMR$lambda.min) ## s= lambda value (any)

modelML <- glmnet(xmtrain, ymtrain, alpha=1)
cvmodelML <- cv.glmnet(xmtrain, ymtrain, alpha=1, type.measure = "mse", nfolds = 5)

predML <- predict(modelML, newx = xmtest, s=cvmodelML$lambda.min)
MSEML <- mean((ymtest - predML)^2)
coef.glmnet(modelML, s= cvmodelML$lambda.min) ## s= lambda value (any)


###########################################################################
## Logistic Regression
###########################################################################
bank <- read_csv("UniversalBank.csv") %>% na.omit()
bank <- bank %>% dplyr::select(-ID,-ZIPCode)
bank_sp <- bank %>% initial_split(prop = 0.8, strata=PersonalLoan)
ltrain <- bank_sp %>% training()
ltest <- bank_sp %>% testing()
xltrain <- ltrain %>% dplyr::select(-PersonalLoan) %>% as.matrix()
yltrain <- ltrain$PersonalLoan
xltest  <- ltest %>% dplyr::select(-PersonalLoan) %>% as.matrix()
yltest  <- ltest$PersonalLoan

### General Logistic Regression
modelL0 <- glm(PersonalLoan ~ ., data=ltrain, family="binomial")
summary(modelL0)
predL0 <- predict(modelL0, newdata=ltest, type = "response")
confusionMatrix(as.factor(ifelse(predL0 > 0.5, 1, 0)),as.factor(yltest))

#### Logistic Regression + Lasso
cvmodelLL <- cv.glmnet(xltrain, yltrain, alpha=1, family="binomial",type.measure = "deviance", nfolds = 10)
modelLL <- glmnet(xltrain, yltrain, alpha=1,family="binomial")
predLL <- predict(modelLL, newx = xltest, type = "response", s = cvmodelLL$lambda.min)
confusionMatrix(as.factor(ifelse(predLL > 0.5, 1, 0)),as.factor(yltest))

#### Logistic Regression + Ridge
cvmodelLR <- cv.glmnet(xltrain, yltrain, alpha=0, family="binomial",type.measure = "deviance", nfolds = 10)
modelLR <- glmnet(xltrain, yltrain, alpha=0,family="binomial")
predLR <- predict(modelLR, newx = xltest, type = "response", s = cvmodelLR$lambda.min)
confusionMatrix(as.factor(ifelse(predLR > 0.5, 1, 0)),as.factor(yltest))

