###################################LINEAR REGRESSION

setwd("C:/Users/Admin/Desktop/SLC I 2019/Datasets")
library(dplyr)  
library(tidyr)   
library(ggplot2)
library(gridExtra)
set.seed(1122)

housing_data = read.csv("kc_house_data.csv")
house_data = select(housing_data, c(price, bedrooms, bathrooms, sqft_living, floors, view, condition, grade, yr_built))

##Linear model with price as response variable and all remaining variables as covariates.
house_model= lm(price ~ bedrooms + bathrooms + sqft_living + floors + view + condition + grade + yr_built, data = house_data)
summary(house_model)


##Residual analysis to validate the Regression model.
par(mfrow = c(2,2))
plot(house_model)

##Histogram and a QQ-plot of the response variable 'price'
p1 = ggplot(house_data, aes(price)) +
        geom_histogram(color = 'white') +
        ggtitle("Histogram of Variable, Price") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme_classic() +
        labs(x = "Price", y = "Price")
        

p2 = ggplot(house_data, aes(sample = price)) +
        stat_qq() +
        stat_qq_line() +
        ggtitle("QQ-Plot of Variable, Price") +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
        theme_classic()


##Histogram and a QQ-plot of the response variable log(price)
house_data$log_price = log(house_data$price)

p3 = ggplot(house_data, aes(log_price)) +
        geom_histogram(color = 'white') +
        ggtitle("Histogram of Log of Price") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme_classic() +
        labs(x = "Price", y = "Price")

p4 = ggplot(house_data, aes(sample = log_price)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle("QQ-Plot of Log of Price")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_classic()

grid.arrange(p1, p2, p3, p4, nrow = 2)

##Linear model with log(price) as response variable 
house_model2 = lm(log_price ~ bedrooms + bathrooms + sqft_living + floors + view + condition + grade + yr_built, data = house_data)
summary(house_model2)

par(mfrow = c(2,2))
##Residual analysis with log(price) as response variable
plot(house_model2)

##Each covariate against log(price)
house_data %>%
  gather(-log_price, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = log_price)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

##Creating and adding two variables to the house_model
house_data$sqr_yr_built = (house_data$yr_built)**2
house_data$sqr_sqft_living = (house_data$sqft_living)**2
house_model3 = lm(log_price ~ bedrooms + bathrooms + sqft_living + floors + view + condition + grade + yr_built + sqft_living + sqr_sqft_living, data = house_data)
summary(house_model3)

##Comparing the two models by sampling for the training and test data
set.seed(1122)
training_set = house_data[sample(nrow(house_data), 10806), ]

##We use the anti-join function, in dplyr, to create the data set test_set
test_set = anti_join(house_data, training_set, by = c("price", "bedrooms", "bathrooms", "sqft_living", "floors", "view", "condition", "grade", "yr_built", "log_price"))

##We create the respective models for the predictions we want
predict_house_model2 = lm(log_price ~ bedrooms + bathrooms + sqft_living + floors + view + condition + grade + yr_built, data = training_set)
predict_house_model3 = lm(log_price ~ bedrooms + bathrooms + sqft_living + floors + view + condition + grade + yr_built  + sqr_yr_built + sqr_sqft_living, data = training_set)

prediction_a = predict(predict_house_model2, newdata = test_set)
prediction_b = predict(predict_house_model3, newdata = test_set)

##Mean squared difference (MSD) between predicted values and values of log(price) for each model
mean((test_set$log_price - prediction_a)**2)
mean((test_set$log_price - prediction_b)**2)

##Extending the model
house_data$ext_yr_built = (house_data$yr_built)**10
house_data$ext_sqft_living = (house_data$sqft_living)**2
house_model4 = lm(price ~ bedrooms + bathrooms + sqft_living + floors + view + condition + grade + yr_built + ext_yr_built + ext_sqft_living, data = house_data)

training_set2 = house_data[sample(nrow(house_data), 10806), ]
test_set2 = anti_join(house_data, training_set2, by = c("price", "bedrooms", "bathrooms", "sqft_living", "floors", "view", "condition", "grade", "yr_built", "log_price"))

##The prediction now
prediction_house_model4 = lm(log_price ~ bedrooms + bathrooms + sqft_living + floors + view + condition + grade + yr_built  + ext_yr_built + ext_sqft_living, data = training_set2)

prediction_c = predict(prediction_house_model4, newdata = test_set)

##MSD between predicted values and values of log(price) 
mean((test_set$log_price - prediction_c)**2)