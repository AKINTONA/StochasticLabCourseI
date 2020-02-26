###################################Data Distribution
library(qqplotr)
library(ggpubr)

setwd("C:/Users/Admin/Desktop/SLC I 2019/Datasets")

##Loading the data
wine_data_file = "winequality-white.csv"
wine_data = read.csv(wine_data_file, header = TRUE, sep = ";")

##Extract the columns relevant for the analysis
wine_data_subset = wine_data[,c(2,4,9,12)]
#head(wine_data_subset)

##Categorize the Quality of the wine into good - '1' and bad - '0'
wine_data_subset$category = cut(wine_data_subset$quality, breaks = c(0,5,10), labels = c(0, 1))
wine_data_split = split(wine_data_subset, wine_data_subset$category)
Map(function(x,y) assign(x,y, envir = .GlobalEnv), x = letters[1:2], y = split(wine_data_subset, wine_data_subset$category, drop = TRUE))
wine_data_bad = a
wine_data_good = b

wine_data_pH = wine_data$pH

##Histogram with Density plot for the whole wine dataset
mean = mean(wine_data_pH)
sd = sd(wine_data_pH)
ggplot(wine_data_subset, aes(x = pH, mean = mean, sd = sd)) +
  geom_histogram(aes(y=..density..), colour = "white", fill = "lightblue") +
  stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd),
              color = "maroon", size = 1) +
  theme_classic() +
  labs(y = "Density") +
  ggtitle("Histogram with Density Plot for the White Wine Dataset")
summary(wine_data_pH)

##Histogram and Density Plot for the Good Wine
par(mfrow=c(1,2))
mean= mean(wine_data_good$pH)
sd = sd(wine_data_good$pH)
g1 = ggplot(wine_data_good, aes(x = pH, mean = mean, sd = sd)) +
  geom_histogram(aes(y=..density..), colour = "black", fill = "maroon") +
  stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd),
  color = "blue", size = 1) +
  theme_classic() +
  labs(y = "Density") +
  ggtitle("Histogram with Density Plot - Good Wine")

##Histogram and Density Plot for the Bad Wine
mean= mean(wine_data_bad$pH)
sd = sd(wine_data_bad$pH)
g2 = ggplot(wine_data_bad, aes(x = pH, mean = mean, sd = sd)) +
  geom_histogram(aes(y=..density..), colour = "black", fill = "maroon") +
  stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd), 
  color = "blue", size = 1) + 
  theme_classic() +
  labs(y = "Density") +
  ggtitle("Histogram with Density Plot - Bad Wine")
ggarrange(g1, g2)


##QQ-plot for Good Quaity Wine Samples
par(mfrow=c(1,2))
qq_good_wine = ggplot(data = wine_data_good, mapping = aes(sample = pH)) +
                  stat_qq_band() +
                  stat_qq_line() +
                  stat_qq_point() +
                  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + 
                  theme_classic() +
                  ggtitle("pH for the Good Wine")

##QQ-plot for Bad Wine
qq_bad_wine = ggplot(data = wine_data_bad, mapping = aes(sample = pH)) +
                  stat_qq_band() +
                  stat_qq_line() +
                  stat_qq_point() +
                  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + 
                  theme_classic() +
                  ggtitle("pH for the Bad Wine")
ggarrange(qq_good_wine, qq_bad_wine)


##QQ-plot for the White Wine Samples
qq_white_whine = ggplot(data = wine_data_subset, mapping = aes(sample = pH)) +
                 stat_qq_band() +
                 stat_qq_line() +
                 stat_qq_point() +
                 labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + 
                 theme_classic() +
                 ggtitle("Theoretical Vs Sample Quantiles of pH for the White Wine Samples")
ggarrange(qq_white_whine)

##PP Plot for pH of the Good Wine
pp_good_wine = ggplot(data = wine_data_good, mapping = aes(sample = pH)) +
                    stat_pp_band() +
                    stat_pp_line() +
                    stat_pp_point() +
                    labs(x = "Empirical Cumulative Distribution", y = "Theoretical Cumulative Distribution") + 
                    theme_classic() +
                    ggtitle("PP Plot for pH of the Good Wine")

##PP Plot for pH of the Bad Wine
pp_bad_wine = ggplot(data = wine_data_bad, mapping = aes(sample = pH)) +
                    stat_pp_band() +
                    stat_pp_line() +
                    stat_pp_point() +
                    labs(x = "Empirical Cumulative Distribution", y = "Theoretical Cumulative Distribution") + 
                    theme_classic() +
                    ggtitle("PP Plot for pH of the Bad Wine")
ggarrange(pp_good_wine, pp_bad_wine)

##PP Plot for pH of the White Wine Samples
pp_white_whine = ggplot(data = wine_data_subset, mapping = aes(sample = pH)) +
                    stat_pp_band() +
                    stat_pp_line() +
                    stat_pp_point() +
                    labs(x = "Empirical Cumulative Distribution", y = "Theoretical Cumulative Distribution") + 
                    theme_classic() +
                    ggtitle("PP Plot for pH of the White Wine Samples")
ggarrange(pp_white_whine)

##Empirical Distribution Functions F_n for the Three Datasets
emp_func_pH = ecdf(wine_data_pH)               ##emp_func_pH
emp_func_good_pH = ecdf(wine_data_good$pH)     ##emp_func_good_pH
emp_func_bad_pH = ecdf(wine_data_bad$pH)       ##emp_func_bad_pH
par(mfrow = c(1,3))
plot(emp_func_pH, col = "red", pch = c(1,12), xlab = "pH of White Wine", main = "Empirical Distribution Function of pH of White Wines", cex = 0.5)
plot(emp_func_good_pH, col = "red", pch = c(1,12), xlab = "pH of Good Wine", main = "Empirical Distribution Function of pH of Good Wine", cex = 0.5)
plot(emp_func_bad_pH, col = "red", pch = c(1,12), xlab = "pH of Bad Wine", main = "Empirical Distribution Function of pH of Bad Wine", cex = 0.5)

##Empirical Distribution Functions F_n with Pointwise Confidence bands for the Three Datasets
wine_data_pH = wine_data[ , "pH"]
wine_data_good_pH = wine_data_good[ ,"pH"]
wine_data_bad_pH = wine_data_bad[ ,"pH"]


par(mfrow = c(1,3))
ordered_wine_data_pH = order(wine_data_pH)
plot(wine_data_pH[ordered_wine_data_pH], emp_func_pH(wine_data_pH[ordered_wine_data_pH]), col = "red", ylab = "F_n", xlab = "pH of White Wine", main = "Empirical Distribution Function of pH of White Wine")
upper_limit = emp_func_pH(wine_data_pH) + emp_func_pH(pH) * (1 - emp_func_pH(wine_data_pH)) * qnorm(1-0.05/2) / sqrt(length(wine_data_pH))
lower_limit = emp_func_pH(wine_data_pH) - emp_func_pH(pH) * (1 - emp_func_pH(wine_data_pH)) * qnorm(1-0.05/2) / sqrt(length(wine_data_pH))
lines(wine_data_pH[ordered_wine_data_pH], pmin(upper_limit[ordered_wine_data_pH], 1), col = "green")
lines(wine_data_pH[ordered_wine_data_pH], pmax(lower_limit[ordered_wine_data_pH], 0), col = "green")


ordered_wine_data_good_pH = order(wine_data_good_pH)
plot(wine_data_good_pH[ordered_wine_data_good_pH], emp_func_good_pH(wine_data_good_pH[ordered_wine_data_good_pH]), col = "red", ylab = "F_n", xlab = "pH of Good Wines", main = "Empirical Distribution Function of pH of Good Wine")
upper_limit_good = emp_func_good_pH(wine_data_good_pH) + emp_func_good_pH(wine_data_good_pH) * (1 - emp_func_good_pH(wine_data_good_pH)) * qnorm(1-0.05/2) / sqrt(length(wine_data_good_pH))
lower_limit_bad = emp_func_good_pH(wine_data_good_pH) - emp_func_good_pH(wine_data_good_pH) * (1 - emp_func_good_pH(wine_data_good_pH)) * qnorm(1-0.05/2) / sqrt(length(wine_data_good_pH))
lines(wine_data_good_pH[ordered_wine_data_good_pH], pmin(upper_limit_good[ordered_wine_data_good_pH], 1), col = "green")
lines(wine_data_good_pH[ordered_wine_data_good_pH], pmax(lower_limit_bad[ordered_wine_data_good_pH], 0), col = "green")

ordered_wine_data_bad_pH = order(wine_data_bad_pH)
plot(wine_data_bad_pH[ordered_wine_data_bad_pH], emp_func_bad_pH(wine_data_bad_pH[ordered_wine_data_bad_pH]), col = "red", ylab = "F_n", xlab = "pH of Bad Wine", main = "Empirical Distribution Function of pH of Bad Wines")
upper_limit_bad = emp_func_bad_pH(wine_data_bad_pH) + emp_func_bad_pH(wine_data_bad_pH) * (1 - emp_func_bad_pH(wine_data_bad_pH)) * qnorm(1-0.05/2) / sqrt(length(wine_data_bad_pH))
lower_limit_bad = emp_func_bad_pH(wine_data_bad_pH) - emp_func_bad_pH(wine_data_bad_pH) * (1 - emp_func_bad_pH(wine_data_bad_pH)) * qnorm(1-0.05/2) / sqrt(length(wine_data_bad_pH))
lines(wine_data_bad_pH[ordered_wine_data_bad_pH], pmin(upper_limit_bad[ordered_wine_data_bad_pH], 1), col = "green")
lines(wine_data_bad_pH[ordered_wine_data_bad_pH], pmax(lower_limit_bad[ordered_wine_data_bad_pH], 0), col = "green")

##Empirical Distribution Functions F_n with Uniform Confidence Bands for the Three Datasets
par(mfrow = c(3,1))
N = 2000
vect = vector( )
for(i in 1:N){
  uniform_rand = runif(100, 0, 1)
  ecdf_uniform_rand = ecdf(uniform_rand)(uniform_rand)
  distribution_uniform_rand = punif(uniform_rand, 0, 1)
  d = max(abs(ecdf_uniform_rand - distribution_uniform_rand))
  vect = c(vect,d)
}

quantile_value = quantile(vect, probs = 0.975)

par(mfrow=c(3,1))
plot(wine_data_pH[ordered_wine_data_pH], emp_func_pH(wine_data_pH[ordered_wine_data_pH]), col = "red", ylab = "F_n", xlab = "pH of White Wine", main = "Empirical Distribution Function of pH of White Wine")
uniform_UL = emp_func_pH(wine_data_pH) + quantile_value
uniform_LL = emp_func_pH(wine_data_pH) - quantile_value
lines(wine_data_pH[ordered_wine_data_pH], pmin(uniform_UL[ordered_wine_data_pH],1), col = "green")
lines(wine_data_pH[ordered_wine_data_pH], pmax(uniform_LL[ordered_wine_data_pH],0), col = "green")
legend("bottomright", 0, 0, legend = c("Confidence Band", "White Wine"), col = c("green", "red"), pch = c(5, 1))

plot(wine_data_good_pH[ordered_wine_data_good_pH], emp_func_good_pH(wine_data_good_pH[ordered_wine_data_good_pH]), col="red", ylab = "F_n", xlab = "pH of Good Wine", main = "Empirical Distribution Function of pH of Good Wines")
uniform_good_UL = emp_func_good_pH(wine_data_good_pH) + quantile_value
uniform_good_LL = emp_func_good_pH(wine_data_good_pH) - quantile_value
lines(wine_data_good_pH[ordered_wine_data_good_pH], pmin(uniform_good_UL[ordered_wine_data_good_pH],1), col = "green")
lines(wine_data_good_pH[ordered_wine_data_good_pH], pmax(uniform_good_LL[ordered_wine_data_good_pH],0), col = "green")
legend("bottomright", 0, 0, legend = c("confidence band", "Good Wine"), col=c("green", "red"), pch=c(5,1))

plot(wine_data_bad_pH[ordered_wine_data_bad_pH], emp_func_bad_pH(wine_data_bad_pH[ordered_wine_data_bad_pH]), col = "red", ylab = "F_n", xlab = "pH of Bad Wine", main = "Empirical Distribution Function of pH of Bad Wine")
uniform_bad_UL = emp_func_bad_pH(wine_data_bad_pH) + quantile_value
uniform_bad_LL = emp_func_bad_pH(wine_data_bad_pH) - quantile_value
lines(wine_data_bad_pH[ordered_wine_data_bad_pH], pmin(uniform_bad_UL[ordered_wine_data_bad_pH],1), col="green")
lines(wine_data_bad_pH[ordered_wine_data_bad_pH], pmax(uniform_bad_LL[ordered_wine_data_bad_pH],0), col="green")
legend("bottomright", 0, 0, legend = c("confidence band", "Bad Wines"), col = c("green", "red"), pch = c(5,1))
 
##Empirical Distribution Functions F_n with Uniform Confidence Bands for the Good and Bad Wines
plot(wine_data_good_pH[ordered_wine_data_good_pH], emp_func_good_pH(wine_data_good_pH[ordered_wine_data_good_pH]), type = "l", col = "red", ylab = "F_n", xlab = "pH of Good and Bad Wines", main = "Empirical Distribution Function of pH of Good and Bad Wines")
uniform_good_UL = emp_func_good_pH(wine_data_good_pH) + quantile_value
uniform_good_LL = emp_func_good_pH(wine_data_good_pH) - quantile_value
lines(wine_data_good_pH[ordered_wine_data_good_pH], pmin(uniform_good_UL[ordered_wine_data_good_pH], 1), col = "green")
lines(wine_data_good_pH[ordered_wine_data_good_pH], pmax(uniform_good_LL[ordered_wine_data_good_pH], 0), col = "green")
lines(wine_data_bad_pH[ordered_wine_data_bad_pH], emp_func_bad_pH(wine_data_bad_pH[ordered_wine_data_bad_pH]), col = "black")
uniform_bad_UL=emp_func_bad_pH(wine_data_bad_pH) + quantile_value
uniform_bad_LL=emp_func_bad_pH(wine_data_bad_pH) - quantile_value
lines(wine_data_bad_pH[ordered_wine_data_bad_pH], pmin(uniform_bad_UL[ordered_wine_data_bad_pH], 1), col="blue")
lines(wine_data_bad_pH[ordered_wine_data_bad_pH], pmax(uniform_bad_LL[ordered_wine_data_bad_pH], 0), col="blue")
legend("bottomright", 1, 0,
       legend =  c("Good Wine", "Bad Wine", "Good Wine Band", "Bad Wine Band"),
       col =  c("red", "black", "green", "blue"),
       lwd=c(2,1), lty=c(2,1),
       box.lty=0)