###################################LOGISTIC REGRESSION

setwd("C:/Users/Admin/Desktop/SLC I 2019/Datasets")
library(dplyr)  
library(tidyr)   
library(ggplot2)
library(caret)

donor_data = read.csv("Donors.txt")
colnames(donor_data) = c("recency", "frequency", "amount", "time", "donation")

##Fitting a GLM with the binary response donation and covariate frequency using the canonical link function
model_frequency = glm(donor_data$donation ~ donor_data$frequency, family = binomial)
summary(model_frequency)

##Fitting a GLM with the binary response donation and covariate frequency amount
model_amount = glm(donor_data$donation ~ donor_data$amount, family = binomial)
summary(model_amount)



##Plot of frequency against amount
ggplot(donor_data, aes(amount, frequency))+
  geom_point() +
  ggtitle("Plot of Frequency against Amount") +
  labs(x = "Amount", y = "Frequency") +
  theme_classic()


##GLM models with the response donation and covariate recency using all glm functions
glm_logit = glm(donation~recency, data = donor_data, family = binomial)
summary(glm_logit)

glm_gauss = glm(donation~recency, data = donor_data, family = gaussian)
summary(glm_gauss)

glm_gamma = glm(donation~recency, data = donor_data, family = Gamma())
summary(glm_gamma)

summary(donor_data)

glm_inverse_gauss = glm(donation~recency, data = donor_data, family = inverse.gaussian)
summary(glm_inverse_gauss)

glm_poisson = glm(donation~recency, data=donor_data, family = poisson)
summary(glm_poisson)

glm_quasi = glm(donation~recency, data=donor_data, family = quasi)
summary(glm_quasi)

glm_quasi_bin = glm(donation~recency, data=donor_data, family = quasibinomial)
summary(glm_quasi_bin)

glm_quasi_poi = glm(donation~recency, data=donor_data, family = quasipoisson)
summary(glm_quasi_poi)

##Assessment of the best covariate(s)
ggplot(donor_data)+
  geom_line(aes(x = recency, y = fitted(glm_logit), color = "fitted(glm_logit)")) +
  geom_line(aes(x = recency, y = fitted(glm_gauss), color = "fitted(glm_gauss)")) +
  geom_line(aes(x = recency, y = fitted(glm_poisson), color = "fitted(glm_poisson)")) +
  geom_line(aes(x = recency, y = fitted(glm_quasi), color = "fitted(glm_quasi)")) +
  geom_line(aes(x = recency, y = fitted(glm_quasi_bin), color = "fitted(glm_quasi_bin)")) +
  geom_line(aes(x = recency, y = fitted(glm_quasi_poi), color = "fitted(glm_quasi_poi)")) +
  scale_colour_manual(values=c("red", "cyan", "purple", "pink", "green", "yellow"))+
  labs(y = "Fits", x = "Recency") +
  theme(legend.title = element_text(color = "blue", size = 12))

##Fit a GLM model with the response donation and canonical link on the training set, choosing appropriate covariates
set.seed(1122)
##We add a new column for effective categorization into training and test sets
donor_data$index = 1:748 

##We sample 374 rows for the training dataset
training_set = donor_data[sample(nrow(donor_data), 374), ]
test_set = anti_join(donor_data, training_set, by = c("recency", "frequency", "amount", "time", "donation", "index"))

##Plot each covariate against donation to determine the appropriate covariates
training_set %>%
  gather(-donation, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = donation)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

##The GLM for the prediction
glm_predict = glm(training_set$donation ~ training_set$amount, family = binomial(link = "logit")) 
summary(glm_predict)

prediction = predict.glm(glm_predict, newdata = test_set, type = "response")

##Now we classify the predictions based on predicted probabilities 
for(i in 1:374){
  ifelse(prediction[i] < 0.5, prediction[i] =  0, prediction[i] =  1)
}

##Classification error
classification_error = sum(abs(test_set$donation - prediction))/374
classification_error


training_set$extend_freq = (training_set$frequency)
test_set2 = anti_join(donor_data, training_set, by = c("recency", "frequency", "amount", "time", "donation", "index", "extend_freq"))


##An improved GLM for the prediction
glm_predict2 = glm(training_set$donation ~ training_set$extend_freq, family = binomial(link = "logit")) 
summary(glm_predict2)

prediction2 = predict.glm(glm_predict2, newdata = test_set2, type = "response")

##Now we classify the predictions based on predicted probabilities 
for(i in 1:374){
  ifelse(prediction2[i] < 0.5, prediction2[i] =  0, prediction2[i] =  1)
}

##Classification error
classification_error2 = sum(abs(test_set2$donation - prediction2))/374
classification_error2



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