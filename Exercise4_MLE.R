###################################MAXIMUM LIKELIHOOD ESTIMATION, MLE
library(rmutil)
library(ggplot2)
library(qqplotr)
library(gridExtra)

set.seed(777)
##Generating 20 Samples
laplace_sample_20 = rlaplace(20, 1, 1)
median(laplace_sample_20)  #Median of the generated sample
quantile_type = vector()
for (i in 1:9){
  quantile_type = c(quantile_type, quantile(laplace_sample_20, 0.5, type = i))
}
quantile_type             #Median computed from the Quantile function

##Generating 1000 Samples
laplace_sample_1000 = rlaplace(1000, 1, 1)
median(laplace_sample_1000)  #MLE of the generated sample

quantile_type_1000 = vector()
for (i in 1:9){
  quantile_type_1000 = c(quantile_type_1000, quantile(laplace_sample_1000, 0.5, type = i))
}
quantile_type_1000           #MLE computed from the Quantile function


##R Function to compute the MLE of a Laplace Sample
MLE = function(a, x){
    -sum(dlaplace(x, m = a, s = 1, log = T))
  }

##MLEs of samples n = 20, mu = 1 and sd = 1 by using the R function and the in-built Quantile Function
optimize(MLE, c(laplace_sample_20), interval = range(laplace_sample_20))
quantile(x = laplace_sample_20, 0.5)

##MLEs of samples n = 1000, mu = 1 and sd = 1 by using the R function and the in-built Quantile Function
optimize(MLE, c(laplace_sample_1000), interval = range(laplace_sample_1000))
quantile(x = laplace_sample_1000, 0.5)

##Monte Carlo Simulations of the MLEs

##Sample Size n = 20
par(mfrow = c(1, 2))
M = 5000
monte_value_20 = setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("iteration", "estimate"))
for (i in 1:M){
  laplace_sample_20 = rlaplace(20, m = 1, s = 1)
  mle_sample_20 = quantile(x = laplace_sample_20, 0.5, type = 5)
  monte_value_20[i, 1] = i
  monte_value_20[i, 2] = mle_sample_20
}

g1 = ggplot(monte_value_20, aes(estimate)) +
        geom_histogram(aes(y = ..density..), color = 'white') +
        stat_function(fun = dnorm, args = list(mean = mean(monte_value_20$estimate), sd = sd(monte_value_20$estimate)), color = 'maroon') +
        ggtitle("Histogram of MLE Samples Size 20") + 
        labs(x = "Estimate", y = "Density") +
        theme_classic()

g2 = ggplot(data = monte_value_20, mapping = aes(sample = estimate)) +
        stat_qq_band() +
        stat_qq_line() +
        stat_qq_point() +
        labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + 
        ggtitle("QQ Plot of MLE Samples Size 20") + 
        theme_classic()
grid.arrange(g1, g2, nrow = 1)

##Sample Size n = 1000
par(mfrow = c(1, 2))
M = 5000
monte_value_1000 = setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("iterations", "estimates"))
for (i in 1:M){
  laplace_sample_1000 = rlaplace(1000, m = 1, s = 1)
  mle_sample_1000 = quantile(x = laplace_sample_1000, 0.5, type = 5)
  monte_value_1000[i, 1] = i
  monte_value_1000[i, 2] = mle_sample_1000
}

g3 = ggplot(monte_value_1000, aes(estimates)) +
      geom_histogram(aes(y = ..density..), color = 'white') +
      stat_function(fun = dnorm, args = list(mean = mean(monte_value_1000$estimates), sd = sd(monte_value_1000$estimates)), color = 'maroon') +
      ggtitle("Histogram of MLE Samples Size 1000") + 
      labs(x = "Estimate", y = "Density") +
      theme_classic()

g4 = ggplot(data = monte_value_1000, mapping = aes(sample = estimates)) +
      stat_qq_band() +
      stat_qq_line() +
      stat_qq_point() +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
      ggtitle("QQ Plots of MLE Samples Size 1000") + 
      theme_classic()
grid.arrange(g3, g4, nrow = 1)

var(monte_value_20$estimate)
var(monte_value_1000$estimates)