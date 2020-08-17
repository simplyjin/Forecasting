#load libraries
library(fpp3)

#Example of creating a tsibble
y <- tsibble(Year = 2015:2019, 
             Observation = c(123,39,78,52,110),
             index = Year)
head(y)

#working with tsibble
head(PBS)

a10 <- PBS %>% 
  filter(ATC2 == "A10") %>% 
  select(Month, Concession, Type, Cost) %>% 
  summarize(TotalC = sum(Cost)) %>%  #find cost regardless of concession or type
  mutate(Cost = TotalC/1e6) #convert to millions


#time plots
head(ansett)

melsyd_economy <- ansett %>% 
  filter(Airports =="MEL-SYD", Class == "Economy")

melsyd_economy %>% 
  autoplot(Passengers) +
  labs(title = "Ansett economy class passengers",
       subtitle = "Melbourne-Sydney") +
  xlab("Year")


#Using a10 data from earlier for a timeseries plot
a10 %>% autoplot(Cost) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")


#2.4 Seasonal Plots

#now the data from each season are overlapped (aka each line represents a year and x axis represents the season/month)
a10 %>% gg_season(Cost, labels = "both") +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")