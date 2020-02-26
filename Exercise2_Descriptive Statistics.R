###################################Descriptive Statistics
library(ggplot2)
library(dplyr)

#TODO: Label axes QQPlots, ECDF, Histogram

##Set working directory
setwd("C:/Users/Admin/Desktop/SLC I 2019/Datasets")

##Reading the data
wine_data_file = "winequality-white.csv"

##Load dataset
wine_data = read.csv(wine_data_file, header = TRUE, sep = ";")
str(wine_data)

##Extract the columns relevant for the analysis
wine_data_subset = wine_data[,c(2,4,9,12)]
#head(wine_data_subset)

##Categorize the Quality of the wine into good - '1' and bad - '0'
wine_data_subset$category = cut(wine_data_subset$quality, breaks = c(0,5,10), labels = c(0, 1))
head(wine_data_subset)

wine_data_split = split(wine_data_subset, wine_data_subset$category)

Map(function(x,y) assign(x,y, envir = .GlobalEnv), x = letters[1:2], y = split(wine_data_subset, wine_data_subset$category, drop = TRUE))
wine_data_bad = a
wine_data_good = b


##Histogram plots for Residual Sugar Samples(Good and Bad Wines) using 3 different Bin Widths
par(mfrow=c(3,2))
hist(wine_data_good$residual.sugar, breaks = "Sturges", main = "Sturges Bin Width", xlab = "Histogram of Residual Sugar for Good Wine", cex.axis = 0.5, font.main = 1, cex.main = 0.8)
hist(wine_data_bad$residual.sugar, breaks = "Sturges", main = "Sturges Bin Width", xlab = "Histogram of Residual Sugar for Bad Wine", cex.axis = 0.5, font.main = 1, cex.main = 0.8)

hist(wine_data_good$residual.sugar, breaks = "FD", main = "Freedmann Diaconis Bin Width", xlab = "Histogram of Residual Sugar for Good Wine", cex.axis = 0.5, font.main = 1, cex.main = 0.8)
hist(wine_data_bad$residual.sugar, breaks = "FD", main = "Freedmann Diaconis Bin Width", xlab = "Histogram of Residual Sugar for Bad Wine", cex.axis = 0.5, font.main = 1, cex.main = 0.8)

hist(wine_data_good$residual.sugar, breaks = "Scott", main = "Scott Bin Width", xlab = "Histogram of Residual Sugar for Good Wine", cex.axis = 0.5, font.main = 1, cex.main = 0.8)
hist(wine_data_bad$residual.sugar, breaks = "Scott", main = "Scott Bin Width", xlab = "Histogram of Residual Sugar for Bad Wine", cex.axis = 0.5, font.main = 1, cex.main = 0.8)

##Summary statistics of Residual Sugar Samples(Good and Bad Wines)
summary(wine_data_good$residual.sugar)
summary(wine_data_bad$residual.sugar)
sd(wine_data_good$residual.sugar)
sd(wine_data_bad$residual.sugar)
IQR(wine_data_good$residual.sugar)
IQR(wine_data_bad$residual.sugar)

##Boxplot of both Residual Sugar Samples(Good and Bad Wines)
p = ggplot(wine_data_subset, aes(x=category, y=residual.sugar, color=category)) +
  geom_boxplot() + 
  ggtitle("Boxplot of Residual Sugar Samples for Both Good and Bad Wines") + 
  theme_classic() +
  labs(y = "Residual Sugar", x = "Wine Quality") + 
  theme(legend.title = element_text(color = "blue", size = 12),
        legend.text = element_text(color = "red"))

##QQplot of both Residual Sugar Samples(Good and Bad Wines)
qqplot(wine_data_good$residual.sugar, wine_data_bad$residual.sugar, xlim = c(0, 10), 
       ylim = c(0, 10), xlab = "Good Wine", ylab = "Bad Wine", main = "Q-Q Plot for Residual Sugars")
abline(a = 0, b = 1, col = "red", lwd = 2)

##Empirical Distrubutions of both Residual Sugar Samples(Good and Bad Wines)
plot(ecdf(wine_data_good$residual.sugar), xlab = "Residual Sugar", main = "Empirical Cumluative Distribution", verticals= T, col = "blue")
lines(ecdf(wine_data_bad$residual.sugar),lty = 1, verticals = T, col = "maroon") 
legend(55,0.9, c("Good Wine","Bad Wine"), col = c("blue", "maroon"), pch = c(19,19))

##Histogram plots for Volatile Sugar Samples(Good and Bad Wines) using 4 different Bin Widths
par(mfrow=c(3,2))
hist(wine_data_good$volatile.acidity, breaks = "Sturges", main = "Sturges Bin Width", xlab = "Histogram of Volatile Acidity for Good Wine", cex.axis = 0.5, font.main = 1, cex.main = 0.8)
hist(wine_data_bad$volatile.acidity, breaks = "Sturges", main = "Sturges Bin Width", xlab = "Histogram of Volatile Acidity for Bad Wine", cex.axis = 0.5, font.main = 1, cex.main = 0.8)

hist(wine_data_good$volatile.acidity, breaks = "FD", main = "Freedmann Diaconis Bin Width", xlab = "Histogram of Volatile Acidity for Good Wine", cex.axis = 0.5, font.main = 1, cex.main = 0.8)
hist(wine_data_bad$volatile.acidity, breaks = "FD", main = "Freedmann Diaconis Bin Width", xlab = "Histogram of Volatile Acidity for Bad Wine", cex.axis = 0.5, font.main = 1, cex.main = 0.8)

hist(wine_data_good$volatile.acidity, breaks = "Scott", main = "Scott Bin Width", xlab = "Histogram of Volatile Acidity for Good Wine", cex.axis = 0.5, font.main = 1, cex.main = 0.8)
hist(wine_data_bad$volatile.acidity, breaks = "Scott", main = "Scott Bin Width", xlab = "Histogram of Volatile Acidity for Bad Wine", cex.axis = 0.5, font.main = 1, cex.main = 0.8)


##Summary statistics of Volatile Acidity Samples(Good and Bad Wines)
summary(wine_data_good$volatile.acidity)
summary(wine_data_bad$volatile.acidity)
sd(wine_data_good$volatile.acidity)
sd(wine_data_bad$volatile.acidity)
IQR(wine_data_good$volatile.acidity)
IQR(wine_data_bad$volatile.acidity)

##Boxplot of both Volatile Acidity Samples(Good and Bad Wines)
p = ggplot(wine_data_subset, aes(x = category, y = volatile.acidity, color = category)) +
  geom_boxplot() + 
  ggtitle("Boxplot of Volatile Acidity Samples for Both Good and Bad Wines") + 
  theme_classic() +
  labs(y = "Volatile Acidity", x = "Wine Quality") + 
  theme(legend.title = element_text(color = "green", size = 12),
        legend.text = element_text(color = "maroon"))
p

##QQplot of both Volatile Acidity Samples(Good and Bad Wines)
qqplot(wine_data_good$volatile.acidity, wine_data_bad$volatile.acidity, xlim = c(0, 10), 
       ylim = c(0, 10), xlab = "Good Wine", ylab = "Bad Wine", main = "Q-Q Plot for Volatile Acidity for Both Good and Bad Wines")
abline(a = 0, b = 1, col = "maroon", lwd = 2)


##Empirical Distrubutions of both Volatile Acidity Samples(Good and Bad Wines)
plot(ecdf(wine_data_good$volatile.acidity), xlab = "Volatile Acidity", main = "Empirical Cumluative Distribution of Volatile Acidity Samples", verticals = T, col = "blue")
lines(ecdf(wine_data_bad$volatile.acidity),lty = 1, verticals = T, col = "red") 
legend(55,0.9, c("Good Wine","Bad Wine"), col = c("blue", "red"), pch = c(19,19))

##Empirical Distrubutions of both Volatile Acidity Samples(Good and Bad Wines)
plot(ecdf(wine_data_good$volatile.acidity), xlab = "Volatile Acidity", main = "Empirical Cumluative Distribution of Volatile Acidity Samples", verticals = T, col = "blue")
lines(ecdf(wine_data_bad$volatile.acidity),lty = 1, verticals = T, col = "red") 
legend(55,0.9, c("Good Wine","Bad Wine"), col = c("blue", "red"), pch = c(10,10))