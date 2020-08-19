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


#what if we have multiple seasonal periods?
#lets use the vic_elec dataset that shows half-hourly electricity demand in  Victoria, AUS
head(vic_elec)

vic_elec %>% gg_season(Demand, period = "day") +
  theme(legend.position = "none")

vic_elec %>% gg_season(Demand, period= "week") + 
  theme(legend.position = "none")

vic_elec %>% gg_season(Demand, period="year")

#2.5 - Seasonal subseries plots

#separate each season to its own plot
a10 %>% 
  gg_subseries(Cost) +
  ylab("$ million") +
  xlab("Year") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

#looking at Austrialian quarterly vacation data
holidays <- tourism %>% 
  filter(Purpose == "Holiday") %>% 
  group_by(State) %>% 
  summarise(Trips = sum(Trips))

#this plot shows strong seasonality for most states, but the seasonal peaks do not overlap
holidays %>% autoplot(Trips) +
  ylab("thousands of trips") + 
  xlab("Year") +
  ggtitle("Australian domestic holiday nights")

#seeing the timing of the seasonal peaks
holidays %>% gg_season(Trips) +
  ylab("thousands of trips") +
  ggtitle("Australian domestic holiday nights")

#lets look at the subseries plot
holidays %>% 
  gg_subseries(Trips) + 
  ylab("thousands of trips") +
  ggtitle("Australian domestic holiday nights")


#2.6 Scatterplots

#show half hour electricity demand in Victoria, AUS
vic_elec %>% 
  filter(year(Time) == 2014) %>% 
  autoplot(Demand) +
  xlab("Year: 2014") + ylab(NULL) +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")

#half hourly temperatures in Melbourne, AUS
vic_elec %>% 
  filter(year(Time) == 2014) %>% 
  autoplot(Temperature) +
  xlab("Year: 2014") + ylab(NULL) +
  ggtitle("Half-hourly temperatures: Melbourne, Australia")


#plot the two together
vic_elec %>% 
  filter(year(Time) == 2014) %>% 
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  ylab("Demand (GW)") + xlab("Temperature (Celsius)")
#it becomes clear that high temperatures = high demand due to the need for AC
#also demand increases for very low temperatures

#scatterplot matrices
visitors <- tourism %>% 
  group_by(State) %>% 
  summarise(Trips = sum(Trips))

visitors %>% 
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  ylab("Number of visitor nights each quarter (millions)")

#plot each time series against each other
visitors %>% 
  spread(State, Trips) %>% 
  GGally::ggpairs(columns = 2:9)


#2.7 Lag plots
recent_production <- aus_production %>% 
  filter(year(Quarter) >= 1992)
recent_production %>% gg_lag(Beer, geom = "point")


#2.8 Autocorrelations
#measures linear relationship between lagged values of a time series
recent_production %>% ACF(Beer) %>% 
  autoplot()

a10 %>% ACF(Cost, lag_max = 48) %>% autoplot()
#downward trend, seasonality due to the "scalloped" shape

#2.9 White noise
set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y %>% autoplot(wn) + ggtitle("White noise")

y %>% ACF(wn) %>% autoplot()
#if one or more are outside the bounds of the dotted line, then the series is probably not white noise

#2.10 Exercise
tute1 <- readr::read_csv("../data/tute1.csv")
view(tute1)

mytimeseries <- tute1 %>% 
  mutate(Quarter = yearmonth(Quarter)) %>% 
  as_tsibble(index = Quarter)

mytimeseries %>% 
  pivot_longer(-Quarter, names_to = "Key", values_to = "Value") %>% #this needs to be done in order to show all three on the same graph
  ggplot(aes(x = Quarter, y = Value, color = Key)) +
  geom_line() +
  facet_grid(vars(Key), scales = "free_y")

#using tourism data
tourism <- readxl::read_excel("../data/tourism.xlsx")
mytimeseries2 <- tourism %>% 
  mutate(Quarter = yearquarter(Quarter)) %>% 
  as_tsibble(key = c("Region", "State", "Purpose"), index = "Quarter")

mytimeseries2 %>% 
  as_tibble() %>% 
  group_by(Region, Purpose) %>% 
  summarize(Trips = mean(Trips)) %>% 
  ungroup() %>% 
  filter(Trips == max(Trips))

statetourism <- tourism %>% 
  group_by(State) %>% 
  summarize(Trips = sum(Trips)) %>% 
  ungroup()

#4
head(aus_production)
aus_production %>% 
  autoplot(Bricks)

pelt %>% 
  autoplot(Lynx)

gafa_stock %>% 
  autoplot(Close)

vic_elec %>% 
  autoplot(Demand) +
  labs(title = "Electricity Demand",
       subtitle = "Victoria, AUS") +
  xlab("Year")

#5
head(aus_arrivals)
aus_arrivals %>% 
  autoplot(Arrivals)

aus_arrivals %>% 
  gg_season(Arrivals)

aus_arrivals %>% 
  gg_subseries(Arrivals)


#6
set.seed(1234)
myseries <- aus_retail %>% 
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

myseries %>% 
  autoplot(Turnover)

myseries %>% 
  gg_season(Turnover)

myseries %>% 
  gg_subseries(Turnover)

myseries %>% 
  gg_lag(Turnover)

myseries %>% 
  ACF(Turnover) %>% 
  autoplot()

#7
head(us_employment)
us_employment %>% 
  filter(Title == "Total Private") %>% 
  autoplot(Employed)

us_employment %>% 
  filter(Title == "Total Private") %>% 
  gg_season(Employed)

us_employment %>% 
  filter(Title == "Total Private") %>% 
  gg_subseries(Employed)

us_employment %>% 
  filter(Title == "Total Private") %>% 
  gg_lag(Employed)

us_employment %>% 
  filter(Title == "Total Private") %>% 
  ACF(Employed) %>% 
  autoplot()


#8
#1-B, 2-A, 3-D, 4-C

#9
head(aus_livestock)
aus_livestock %>% 
  filter(State == "Victoria", 
         year(Month) >= 1990 & year(Month) <= 1995) %>% 
  ACF(Count) %>% 
  autoplot()


#10
dgoog <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>%
  mutate(diff = difference(Close))