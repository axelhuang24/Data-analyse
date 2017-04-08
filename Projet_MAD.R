# Read the data 
datahousing <- read.table("housingdata.txt")
colnames(datahousing) <- c("crim", "zn", "indus", "chas", "nox", "rm", "age", "dis", 
                           "rad", "tax", "ptratio", "b", "lstat","medv" )

# We will create two matrix , one of them is the training data(3/4) , and the other one is 
# the validation data(1/4)
# n = nrow(datahousing)
# test <- sample(1:n, round(n)/4)
# datahousing.train <- datahousing[-test, ]
# datahousing.test <- datahousing[test, ]



# 1.2 Try to describe the tendence of the output(medev) 
mean(datahousing$medv)
median(datahousing$medv)
var(datahousing$medv)

par(mfrow = c(1,2))
hist(datahousing$medv, freq=FALSE, col="blue", main="Histogram",xlab="")
lines(density(datahousing$medv), col="red")
plot(ecdf(datahousing$medv), main="Cumulative distribution function",xlab="")
title(outer=TRUE, main="\n Distribution of medv")

# Qualititive variable
factor_chas <- factor(datahousing$chas)
plot(factor_chas, datahousing$medv, col=c('red', 'blue'))
title("Distribution de medv en fonction de chas")

# Analyse descriptive univariee

summary(datahousing)
boxplot(datahousing[, c(-4,-7, -10, -12)])
boxplot(datahousing$age, main = "Boite de moustache d'âge")
boxplot(datahousing$tax, main = "Boite de moustache de tax")
boxplot(datahousing$b, main = "Boite de moustache de b")

# Quantatitive variables Analyse descriptive univariee
library(GGally)
# ggpairs(datahousing[, -datahousing$chas])


# Modelisation(regression)

## Regression lineaire
null <- lm(medv ~ 1, datahousing)
full <- lm(medv ~ ., datahousing)

anova(null, full)

summary(full)
par(mfrow=c(2,2))
plot(full, which=c(1,2,4,5))

datahousing=datahousing[c(-365, -369, -373),]
# datahousing$medv <- log((datahousing$medv))

null <- lm( log(medv) ~ 1, datahousing)
full <- lm( log(medv) ~ ., datahousing)

anova(null, full)

summary(full)
par(mfrow=c(2,2))
plot(full, which=c(1,2,4,5))

## Selection de stepwise

# We will create two matrix , one of them is the training data(4/5) , and the other one is 
# the validation data(1/5)
n = nrow(datahousing)
test <- sample(1:n, round(n)/5)
datahousing.train <- datahousing[-test, ]
datahousing.test <- datahousing[test, ]

m <- nrow(datahousing.train)
lower = terms(medv ~ 1, data=datahousing.train)
upper = terms(medv ~ ., data=datahousing.train)
              
scope <- list(lower, upper)
model.AIC <- step(full, scope, direction='both', trace=TRUE)
model.BIC <- step(full, scope, direction='both', k=log(m), trace=TRUE)
model.AIC
model.BIC

# par(mfrow=c(2,2))
# plot(model.AIC, which=c(1,2,4,5))
summary(model.AIC)

# par(mfrow=c(2,2))
# plot(model.BIC, which=c(1,2,4,5))
summary(model.BIC)

## Methode de penalisation

### Ridge regression


library(Matrix)
library(foreach)
library(glmnet)
x <- as.matrix(datahousing.train[, -14])
y <- log(datahousing.train$medv)
ridge <- glmnet(x,y,alpha=0)

par(mfrow=c(1,3))
plot(ridge, xvar="lambda",label=TRUE)
# plot(ridge, xvar="norm",label=TRUE)
# plot(ridge, xvar="dev",label=TRUE)
# ridge$dev.ratio

ridge.10cv <- cv.glmnet(x,y,nfolds=10, alpha=0, grouped=FALSE)
ridge.loo <- cv.glmnet(x,y,nfolds=n, alpha=0, grouped=FALSE)
par(mfrow=c(1,2))
plot(ridge.10cv)
plot(ridge.loo)
testx <- as.matrix(datahousing.train[1:10 ,c(-14)])
predict(ridge, newx=testx, s=ridge.10cv$lambda.min)
predict(ridge, newx=testx, s=ridge.10cv$lambda.1se)


### lasoo regression

lasso <- glmnet(x, y, alpha = 1)
par(mfrow=c(1,3))
plot(lasso, xvar="lambda")
# plot(lasso, xvar="norm")
# plot(lasso, xvar="dev")
# lasso$dev.ratio


lasso.10cv <- cv.glmnet(x,y,nfolds=10, grouped=FALSE)
lasso.loo <- cv.glmnet(x,y,nfolds=n , grouped=FALSE)
par(mfrow=c(1,2))
plot(lasso.10cv)
plot(lasso.loo)
#testx <- as.matrix(datahousing.train[1:10 ,-14])
predict(lasso, newx=testx, s=lasso.10cv$lambda.min)
predict(lasso, newx=testx, s=lasso.10cv$lambda.1se)

n <- nrow(datahousing.train)
p <- ncol(datahousing.train)
AIC <- n*log(colMeans((y - predict(lasso, x))^2)) + 2 * lasso$df
BIC <- n*log(colMeans((y - predict(lasso, x))^2)) + log(n) * lasso$df

library(ggplot2)
d <- data.frame(lambda = rep(lasso$lambda, 2),
                value = c(AIC, BIC),
                critere = factor(rep(c("AIC","BIC"), each=length(lasso$lambda))))
ggplot(d, aes(x=lambda,y=value,colour=critere,group=critere)) + geom_line() + scale_x_log10()

## Evaluation des modeles

test_x <- as.matrix(datahousing.test[, -14])
test_y <- log(datahousing.test$medv)

### full Regression lineaire
error_full <- test_y - predict(full, datahousing.test)
rmse <- function(error_full)
  sqrt(mean((error_full)^2))
rmse(error_full) 

### stepwise

#### AIC

error_AIC <- test_y - predict(model.AIC, datahousing.test)
rmse <- function(error_AIC)
  sqrt(mean((error_AIC)^2))
rmse(error_AIC) 

#### BIC

error_BIC <- test_y - predict(model.BIC, datahousing.test)
rmse <- function(error_BIC)
  sqrt(mean((error_BIC)^2))
rmse(error_BIC) 

### Methode penalisation


mse_ridge_min <- mean((test_y - predict(ridge, newx=test_x, s=ridge.10cv$lambda.min))^2)
rmse_ridge_min <- sqrt(mse_ridge_min)

mse_ridge_1se <- mean((test_y - predict(ridge, newx=test_x, s=ridge.10cv$lambda.1se))^2)
rmse_ridge_1se <- sqrt(mse_ridge_1se)

mse_lasso_min <- mean((test_y - predict(lasso, newx=test_x, s=lasso.10cv$lambda.min))^2)
rmse_lasso_min <- sqrt(mse_lasso_min)

mse_lasso_1se <- mean((test_y - predict(lasso, newx=test_x, s=lasso.10cv$lambda.1se))^2)
rmse_lasso_1se <- sqrt(mse_lasso_1se)






