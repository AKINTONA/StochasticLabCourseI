###################################CATEGORICAL DATA TEST
library(ggplot2)
library(dplyr)
library(ggpubr)

##Data Processing in tabular form
airport = c("Los Angeles", "Phoenix", "San Diego", "San Francisco", "Seattle")
ontime = c(497, 221, 212, 503, 1841, 694, 4840, 383, 320, 201)
delayed = c(62, 12, 20, 102, 305, 117, 415, 65, 129, 61)
airports = rep(airport, 2)
airline = c("Alaska Airlines", "America West")
airlines = rep(airline, c(5,5))
flight_data = data.frame(airports, ontime, delayed, airlines)

##Individual airports extract
alaska_data = subset(flight_data, airlines == "Alaska Airlines")
alaska_data = subset(alaska_data, select = c(airports, ontime, delayed))
america_west_data = subset(flight_data, airlines == "America West")
america_west_data = subset(america_west_data, select = c(airports, ontime, delayed))
alaska_data2 = subset(alaska_data, select = c(ontime, delayed))
america_west_data2 = subset(america_west_data, select = c(ontime, delayed))

##Chi-Squared Test on the Cities for the Two Airlines
chi_square_alaska = chisq.test(alaska_data2)
chi_square_alaska
chi_square_alaska$p.value

chi_square_america = chisq.test(america_west_data2)
chi_square_america
chi_square_america$p.value

##Alternatively
total_alaska = sum(alaska_data2$ontime, alaska_data2$delayed)
test_alaska = 0
for(i in 1:5){
  for(j in 1:2){
    test_alaska = test_alaska + (((alaska_data2[i,j]-(sum(alaska_data2[i, ])*sum(alaska_data2[ ,j]))/total_alaska)
                                  ^2)/((sum(alaska_data2[i, ])*sum(alaska_data2[ ,j]))/total_alaska))}
}

total_america = sum(america_west_data2$ontime, america_west_data2$delayed)
test_america = 0
for(i in 1:5){
  for(j in 1:2){
    test_america = test_america + (((america_west_data2[i,j]-(sum(america_west_data2[i, ])
                  *sum(america_west_data2[ ,j]))/total_america)^2)/((sum(america_west_data2[i, ])
                  *sum(america_west_data2[ ,j]))/total_america))}
}

alpha = 0.05
chi_square_quantile = qchisq(1 - alpha, 4)
chi_square_quantile

##Test if the delay is independent on the airline.
flight_data_subtotal = matrix(c(sum(flight_data[1:5, 2]), sum(flight_data[1:5, 3]), sum(flight_data[6:10, 2]), 
                                sum(flight_data[6:10, 3])), nrow = 2)
colnames(flight_data_subtotal) = c("Alaska Airline", "America West Airline")
rownames(flight_data_subtotal) = c("Ontime", "Delayed")
flight_data_subtotal = as.table(flight_data_subtotal)
flight_data_subtotal

chi_square_flight_data = chisq.test(flight_data_subtotal)
chi_square_flight_data

##Binomial Test
binomial_test_alaska1 = binom.test(flight_data_subtotal["Delayed", "Alaska Airline"], 
                                  sum(flight_data_subtotal[ , "Alaska Airline"]), p = 0.14, conf.level = 0.95)
binomial_test_alaska1
binomial_test_alaska1$conf.int
binomial_test_alaska1$p.value

binomial_test_alaska2 = binom.test(flight_data_subtotal["Delayed", "Alaska Airline"], 
                        sum(flight_data_subtotal[ , "Alaska Airline"]), p = 0.14, alternative = c("greater"), 
                        conf.level = 0.95)
binomial_test_alaska2
binomial_test_americaWest1 = binom.test(flight_data_subtotal["Delayed", "America West Airline"], 
                                       sum(flight_data_subtotal[ , "America West Airline"]), p = 0.14, 
                                       conf.level = 0.95)
binomial_test_americaWest1

binomial_test_americaWest2 = binom.test(flight_data_subtotal["Delayed", "America West Airline"], 
                                       sum(flight_data_subtotal[ , "America West Airline"]), p = 0.14, 
                                       alternative = c("greater"), conf.level = 0.95)
binomial_test_americaWest2