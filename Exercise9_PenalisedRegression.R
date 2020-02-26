###################################PENALISED REGRESSION

setwd("C:/Users/Admin/Desktop/SLC I 2019/Datasets")
library(dplyr)
library(ggplot2)
library(glmnet)

##Loading data  and omitting the players with incomplete data
data = read.table("hitters.dat")
hitters = na.omit(data)

##The condition number 
y = hitters$Salary
hitters_subset = hitters[,-19]      ##Take out the Salary column
x = data.matrix(hitters_subset)     ##Make matrix of the subset of the data
kappa(t(x) %*% x)

##The command "scale" is used to center the columns of the matrix t(x) %*% x
kappa(t(scale((x))) %*% scale(x)) 

##Fitting a standard linear model with no regularisation
model_linearModel= lm( Salary ~ . , data = hitters)
summary(model_linearModel)

##Fitting a ridge regression with lamda = 70
model_ridge= glmnet(x, y, alpha = 0, lambda = 70)
coef(model_ridge)

##spliting the data into training and test sets to enable us pick a data-driven lambda
set.seed(1122)
ind = sample(2, nrow(hitters), replace = T, prob = c(0.5, 0.5) )
training_set = hitters[ind == 1,]
test_set = hitters[ind == 2,]

##Writing a function that takes lambda as argument, fits a ridge regression on the training
##sets and calculates the squared prediction error on the test set
df1 = training_set[,-19]
df2 = test_set[,-19]
n = data.matrix(df1)
m = training_set$Salary
z = data.matrix(df2)

##We use a for loop in the this function because we would like to use lambda as vector
MSE_find = function(lambda){
  MSPE = numeric(length(lambda))
  for(i in 1:length(lambda)){
      model_ridge = glmnet(n, m, alpha = 0, lambda = lambda[i])
      prediction = predict(model_ridge, newx = z)
      MSPE[i] = mean((test_set$Salary - prediction)**2)
    }
  return(MSPE)
}

##Running this function on a logarithmic grid (e.g., 10^seq(from = 10, to = -2, length = 100))
lambda =10^seq(from = 10, to = -2, length = 100)
MSE_find(lambda)

##Plot of log(lambda) vs MSE
a = as.data.frame(lambda)
b = as.data.frame(MSE_find(lambda))
c = cbind(a,b)
ggplot(c, aes(lambda,MSE_find(lambda))) +
  geom_point() +
  scale_x_log10() +
  labs(x = "Lambda", y = "MSE") +
  theme_classic() +
  ggtitle("Plot of MSE against Log(lambda)")

 
##Fitting a ridge regression with lambda_opt on all the data, and interpret some of the coefficients.
lambda_opt= 10^2
model_ridge_opt= glmnet(x, y, alpha = 0, lambda = lambda_opt)
coef(model_ridge_opt)


##Repeating the above procedure replacing ridge with lasso
df3 = training_set[,-19]
df4 = test_set[,-19]
n = data.matrix(df3)
m = training_set$Salary
z = data.matrix(df4)


MSE_find = function(lambda){
  MSPE = numeric(length(lambda))
  for(i in 1:length(lambda)){
    model_lasso = glmnet(n, m, alpha = 1, lambda = lambda[i])
    prediction = predict(model_lasso, newx = z)
    MSPE[i] = mean((test_set$Salary - prediction)**2)
  }
  return(MSPE)
}

e = as.data.frame(lambda)
f = as.data.frame(MSE_find(lambda))
g = cbind(e,f)
ggplot(g, aes(lambda, MSE_find(lambda))) +
    geom_point() +
    scale_x_log10() +
    labs(x = "Lambda", y = "MSE") +
    theme_classic() +
    ggtitle("Plot of MSE against Log(lambda)")

lambda_opt2 = 10**(1.4)
model_lasso_opt= glmnet(x, y, alpha = 1, lambda = lambda_opt2)
coef(model_lasso_opt)