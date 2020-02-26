###################################SIMPSON'S RULE

library(ggplot2)
library(dplyr)
library(ggpubr)


plane = matrix(c(497,62,694,117,221,12,4840,415,212,20,383,65,503,102,320,129,1841,305,201,61),ncol=4,byrow=TRUE)
colnames(plane)=c("Alaska Airline On time","Alaska Airline Delayed","America West Airline On time","America West Airline Delayed")
rownames(plane)=c("Los Angeles", "Phoenix", "San Diego", "San Fransisco","seattle")
plane=as.table(plane)
plane


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
america_west_data = subset(flight_data, airlines == "America West")

##Create rows for the total of both airlines
flight_data = rbind(flight_data, data.frame(airports ='Total', ontime = sum(alaska_data$ontime), delayed = sum(alaska_data$delayed), airlines ="Alaska Airlines"))
flight_data = rbind(flight_data, data.frame(airports ='Total', ontime = sum(america_west_data$ontime), delayed = sum(america_west_data$delayed), airlines ="America West"))

##Create a column for the percentage delayed for each airport
flight_data['percentage_delayed'] = round( ((flight_data$delayed / (flight_data$delayed + flight_data$ontime) ) * 100), digits = 2)

##ploting the bar chart
ggplot(flight_data, aes(fill = airlines, y = percentage_delayed, x = airports)) + 
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Barplot Comparing Percent of Delayed Flights between Alaska Airlines and America West for Five Cities") + 
  theme_classic() +
  geom_text(aes(label = paste0(percentage_delayed, "%")), vjust = -0.3, position = position_dodge(0.5)) + 
  labs(fill = "Airlines", y = "Percentage Delayed", x = "Airports", title="Bar Chart", subtitle="Percentage of Delayed Flight Per City for America West and Alaska Airlines", 
       caption="Source: Moore, D.S. (2003) The Basic Practice of Statistics") + 
  scale_fill_manual(values = c("#d8b365", "#5ab4ac")) +
  theme(legend.title = element_text(color = "blue", size = 12),
      legend.text = element_text(color = "red"))

##Prepare data for pie chart
los_angeles_data = filter(flight_data, airports == "Los Angeles")
phoenix_data = filter(flight_data, airports =="Phoenix")
san_diego_data = filter(flight_data, airports =="San Diego")
san_francisco_data = filter(flight_data, airports =="San Francisco")
seattle_data = filter(flight_data, airports =="Seattle")
total_data = filter(flight_data, airports == "Total")

##Pie Charts for the five Airports
par(mfrow=c(3,2))
los_angeles = ggplot(data= los_angeles_data) +
  geom_bar(aes(x= "", y = percentage_delayed, fill = airlines), stat="identity", width = 1) +
  coord_polar("y", start=0, direction = -1) + 
  geom_text(aes(x = "", y = percentage_delayed/2 + c(0, cumsum(percentage_delayed)[-length(percentage_delayed)]), 
                label = paste0(percentage_delayed, "%")), size = 5) + 
  labs(title ="Los Angeles Airport", fill = "Airlines") + 
  theme_void()

phoenix = ggplot(data = phoenix_data) +
  geom_bar(aes(x= "", y = percentage_delayed, fill = airlines), stat = "identity", width = 1) +
  coord_polar("y", start = 0, direction = -1) +
  geom_text(aes(x = "", y = percentage_delayed/2 + c(0, cumsum(percentage_delayed)[-length(percentage_delayed)]), 
                label = paste0(percentage_delayed, "%")), size = 5) +
  labs(title = "Phoenix Airport", fill = "Airlines") + 
  theme_void() 

san_diego = ggplot(data = san_diego_data) +
  geom_bar(aes(x = "", y = percentage_delayed, fill = airlines), stat="identity", width = 1) +
  coord_polar("y", start = 0, direction = -1) +
  geom_text(aes(x = "", y = percentage_delayed/2 + c(0, cumsum(percentage_delayed)[-length(percentage_delayed)]), 
                label = paste0(percentage_delayed, "%")), size = 5) +
  labs(title ="San Diego Airport", fill = "Airlines") + 
  theme_void() 

san_francisco = ggplot(data = san_francisco_data) +
  geom_bar(aes(x = "", y = percentage_delayed, fill = airlines), stat = "identity", width = 1) +
  coord_polar("y", start = 0, direction = -1) +
  geom_text(aes(x = "", y = percentage_delayed/2 + c(0, cumsum(percentage_delayed)[-length(percentage_delayed)]), 
                label = paste0(percentage_delayed, "%")), size = 5) +
  labs(title = "San Francisco Airport", fill = "Airlines") + 
  theme_void() 

seattle = ggplot(data = seattle_data) +
  geom_bar(aes(x = "", y = percentage_delayed, fill = airlines), stat = "identity", width = 1) +
  coord_polar("y", start = 0, direction = -1) + 
  geom_text(aes(x = "", y = percentage_delayed/2 + c(0, cumsum(percentage_delayed)[-length(percentage_delayed)]), 
                label = paste0(percentage_delayed, "%")), size = 5) +
  labs(title="Seattle Airport", fill = "Airlines") + 
  theme_void() 

total_pie = ggplot(data = total_data) +
  geom_bar(aes(x="", y = percentage_delayed, fill = airlines), stat = "identity", width = 1) +
  coord_polar("y", start = 0, direction = -1) +
  geom_text(aes(x = "", y = percentage_delayed/2 + c(0, cumsum(percentage_delayed)[-length(percentage_delayed)]), 
                label = paste0(percentage_delayed, "%")), size=5) +
  labs(title = "All Five Airports", fill = "Airlines") + 
  theme_void() 

g = ggarrange(los_angeles, phoenix, san_diego, san_francisco, seattle, total_pie, ncol = 2, nrow = 3)