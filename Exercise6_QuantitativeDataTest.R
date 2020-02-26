###################################QUANTITATIVE DATA TEST
setwd("C:/Users/Admin/Desktop/SLC I 2019/Datasets")

US_election_data = read.csv("Votes.csv")
##Creating a variable, vote
US_election_data$vote = ifelse(US_election_data$Percent.Clinton > US_election_data$Percent.Trump, 1, 0)

##Create a subset based on the variable, Diversity.Index
clinton = select(filter(US_election_data, vote == 1), Diversity.Index, Percent.Clinton, Percent.Trump)
trump = select(filter(US_election_data, vote == 0), Diversity.Index, Percent.Clinton, Percent.Trump)

##Test for normality
par(mfrow = c(2, 2))
d1 = density(clinton$Diversity.Index)
plot(d1, main = "Density Plot: Diversity Index for Clinton")
qqnorm(clinton$Diversity.Index, main = "QQ-Plot: Diversity Index for Clinton"); qqline(clinton$Diversity.Index, col = 3)

d2 = density(trump$Diversity.Index)
plot(d2, main = "Density Plot: Diversity Index for Trump")
qqnorm(trump$Diversity.Index, main = "QQ-Plot: Diversity Index for Trump"); qqline(trump$Diversity.Index, col = 3) 


##T-test of the two groups
t_test = t.test(clinton$Diversity.Index,trump$Diversity.Index, conf.level = 1 - alpha)
t_test$p.value

##Wilcox Test of the two groups
wilcox.test(clinton$Diversity.Index,trump$Diversity.Index)

clinton_wilcox = wilcox.test(clinton$Diversity.Index, trump$Diversity.Index, mu = 0, conf.int = T)
clinton_wilcox$p.value



##Diversity Index Tests
par(mfrow=c(1,2))
d3 = density(US_election_data$Diversity.Index)
plot(d3, main = "Density Plot: Diversity Index of the data")

qqnorm(US_election_data$Diversity.Index, main = "QQ-Plot: Diversity Index of the data"); qqline(US_election_data$Diversity.Index, col = 3)

t.test(US_election_data$Diversity.Index, mu = 50, conf.level = 1 - alpha)
wilcox.test(US_election_data$Diversity.Index, mu = 50, conf.level = 1 - alpha)