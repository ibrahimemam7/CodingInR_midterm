#' Coding in R - Midterm Project
#' Ibrahim Emam
#' July/29/2024

#import relevant libraries
library(tidyverse)
library(funModeling)
library(lubridate)
library(corrplot)
library(ggmap)

#################
## Import Data ##
#################

# start by importing the csv files into data frames
station <- read.csv("raw_data/station.csv")
trip <- read.csv("raw_data/trip.csv")
weather <- read.csv("raw_data/weather.csv")

#########
## EDA ##
#########

# get an idea of what information is contained in the station dataframe
head(station)

# determine the range of the values to identify potential outliers
# summary also shows the type for each variable
summary(station)

# identify zeros or NAs
status(station)

# visualize distributions for categorical variables
freq(station)

# repeat similar steps for the trip data frame
head(trip)
summary(trip)
status(trip)
freq(trip)

#' visualize the distribution of numerical variables in the trips data frame
#' did not do this for station dataframe because there were no important numerical
#' variables (i.e. showing distribution of longitude/latitude is not helpful)
plot_num(trip)

#' it seems the previous histogram does not show the distribution of the data for
#' trip duration, likely because the range of the data is very wide. Use log.
hist(log(trip$duration))

# repeat similar steps for the weather data frame
head(weather)
summary(weather)
status(weather)
freq(weather)
plot_num(weather)

##############
## Cleaning ##
##############

# CLEANING STATION DATA

# check for duplicates, there are none
any(duplicated(station))

# make a copy to leave the original data as is
clean_station <- station

# format installation date as posix
clean_station$installation_date <- mdy(clean_station$installation_date, tz = "UTC")

# CLEANING TRIP DATA FRAME (using copy of data, not original)

clean_trip <- trip

# check for duplicate rows in the data, there are none
any(duplicated(clean_trip))

# format trip duration as minutes instead of seconds
clean_trip$duration <- clean_trip$duration / 60

#remove outliers (any trip over 12 hours is considered unrealistic)

# keep the id of these long trips to document the outliers that were removed
long_outliers <- clean_trip$id[clean_trip$duration > 12*60]

# exclude the long outliers from the data frame
clean_trip <- clean_trip %>% 
  filter(!duration > 12*60)

#' remove very short trips that began and started at the same station
#' (these trips were likely cancelled).

# keep a note of the IDs that are considered cancelled trips 
cancelled_trips <- clean_trip$id[clean_trip$start_station_id == clean_trip$end_station_id
                                 & clean_trip$duration < 3]
# exclude cancelled trips from the data frame 
clean_trip <- clean_trip %>% 
  filter(!(start_station_id == end_station_id & duration < 3))

#' # format times as POSIX
clean_trip$start_date <- mdy_hm(clean_trip$start_date, tz = "UTC")
clean_trip$end_date <- mdy_hm(clean_trip$end_date, tz = "UTC")

# zip code has missing values, but zip code likely will not be used later

# CLEANING WEATHER DATA FRAME (using copy of data, not original)

clean_weather <- weather

# check for duplicates, there are none
any(duplicated(clean_weather))

# format date column as POSIX
clean_weather$date <- mdy(clean_weather$date)

#' code book defines "T" as trace amount of precipitation (<.01 inch). This value
#' will be imputed as 0.005 inch to keep the column entirely numeric
clean_weather <- clean_weather %>%
  mutate(precipitation_inches = 
           if_else(precipitation_inches == "T", 0.005, as.numeric(precipitation_inches)))

#' replace blank values in "events" column with "None" for clarity in later analysis.
#' also, rain is spelled with both uppercase and lowecase, so those should be combined
clean_weather <- clean_weather %>%
  mutate(events = if_else(events == "", "None", events)) %>% 
  mutate(events = if_else(events == "rain", "Rain", events))

# format weather events as factor for later analysis
clean_weather$events <- as.factor(clean_weather$events)

# no noticeable outliers or missing values

#########################################
## Summary Figures After Data Cleaning ##
#########################################

# CREATE SUMMARY FIGURES FOR STATION DATA

# station figure - 1: create a map of the stations across the bay area

# register API key for geocoding service
register_stadiamaps(key = "1e027bd6-f7fb-4b05-9f2c-ce48d3386297")

# use API to get map of bay area
sf_bay_area_map <- get_stadiamap(bbox = c(left = -122.55, bottom = 37.25, right = -121.7, top = 37.87), 
                                 zoom = 10, maptype = "alidade_smooth")

# use ggmaps package to plot the lat and long of the stations on the map
ggmap(sf_bay_area_map) +
  geom_jitter(data = clean_station, aes(x = long, y = lat), 
              width = 0.009, height = 0.009, color = "blue", size = 0.8) +
  theme_classic() +
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank())

# station figure - 2: show number of stations by city
ggplot(clean_station, aes(x = city)) +
  geom_bar(fill = "blue", col = "black") +
  labs(title = "Number of Stations by City",
       x = "City",
       y = "Number of Stations") +
  theme_classic()

# CREATE SUMMARY FIGURES FOR WEATHER DATA

# weather figure 1: Show the number of each weather event for each city

# get the number of weather events for each city
weather_events_summary <- clean_weather %>%
  group_by(events, city) %>%
  summarise(count = n(), .groups = 'drop')

# create a bar plot with one colour corresponding to each city
ggplot(weather_events_summary, aes(x = events, y = count, fill = city)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_ordinal("city") +
  labs(title = "Weather Events by City",
       x = "Weather Event",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# weather figure 2: Show the avg, min, and max throughout the year for each city

# create a line graph with blue, black, and red corresponding to min, avg, and max temp
ggplot(clean_weather, aes(x = date)) +
  facet_wrap(~city, scales = "free", ncol = 2) +
  geom_smooth(aes(y = min_temperature_f, color = "Min Temp")) +
  geom_smooth(aes(y = mean_temperature_f, color = "Mean Temp")) +
  geom_smooth(aes(y = max_temperature_f, color = "Max Temp")) +
  labs(title = "Daily Temperatures by City (°F)", color = "Temperature Type",
       x = NULL, y = NULL) +
  scale_color_manual(values = c("Min Temp" = "blue", "Mean Temp" = "black", "Max Temp" = "darkred")) +
  theme_classic() +
  theme(legend.position =  c(0.8, 0.13),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
        legend.key = element_rect(fill = "transparent"),
        plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(1, "lines"))


########################
## Rush Hour Analysis ##
########################

#' using the start or end time will bias the rush hours towards earlier or later
#' hours respectively, so a new column will be created for the midpoint
clean_trip$trip_mp <- as.POSIXct((as.numeric(clean_trip$end_date) + as.numeric(clean_trip$start_date)) / 2,
                                 tz = "UTC")

# create function to easily identify weekdays/weekends
is_weekday <- function(date) {
  day_of_week <- weekdays(date)
  return(day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
}

# create a subset of the data with only weekdays
weekday <- clean_trip %>% 
  filter(is_weekday(trip_mp))

# create a subset of the data with only weekends
weekend <- clean_trip %>% 
  filter(!is_weekday(trip_mp))

#' reformat the midpoint column to have the same month, day, and year for all
#' trip midpoints. This is important because we do not want the x-axis to have
#' bins for every day of the year (only concerned with 24hr period)
weekday$trip_mp <- update(weekday$trip_mp, year = 1970, month = 1, day = 1)
weekend$trip_mp <- update(weekend$trip_mp, year = 1970, month = 1, day = 1)

#' determine the average number of trips in any given half hour increment (will
#' use this same increment for the histogram in the next step). The average
#' will be a helpful reference value to decide what is considered 'rush hours'
avg_weekday_freq <- nrow(weekday[!is.na(weekday$trip_mp),]) / 48

# plot weekday histogram with ggplot2
ggplot(weekday, aes(x = trip_mp)) +
  geom_histogram(binwidth = 1800, fill = "blue", color = "black") +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hour") +
  labs(title = "Weekday Trip Volume",
       x = "Time",
       y = "Frequency") +
  geom_hline(yintercept = avg_weekday_freq, color = "red", linetype = "dashed") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#' histogram indicates rush hour is from 7:30am-10:30am and 16:00 to 20:00, as
#' these times have above average tip volume.

############################
## Busy Stations Analysis ##
############################

#' create new column "start_time" with the same date for all rows. Need to do this
#' because POSIX format always has an associated date, but we do not want to compare
#' dates when figuring out if a trip started/ended during rush hours (we only want
#' to filter based on time)
weekday$start_time <- update(weekday$start_date, year = 1970, month = 1, day = 1)

# create new column "end_time" with the same date for all rows (reason same as above)
weekday$end_time <- update(weekday$end_date, year = 1970, month = 1, day = 1)

# specify which hours are rush hours
am_rush_start <- as.POSIXct("1970-01-01 07:30:00")
am_rush_end <- as.POSIXct("1970-01-01 10:30:00")
pm_rush_start <- as.POSIXct("1970-01-01 16:00:00")
pm_rush_end <- as.POSIXct("1970-01-01 20:00:00")

# determine the 10 most frequent starting stations during weekday rush hours
weekday %>% 
   # find trips that start during the morning rush or the afternoon rush
  filter((start_time > am_rush_start & start_time < am_rush_end) | (start_time > pm_rush_start & start_time < pm_rush_end)) %>% 
  # group by station
  group_by(start_station_name) %>% 
  # get the frequency of trips started for each station
  summarise(frequency = n()) %>% 
  # see the 10 stations with the highest frequency of trips started during rush hours
  arrange(desc(frequency))

# determine the 10 most frequent ending stations during weekday rush hours
weekday %>% 
  # find trips that end during the morning rush or the afternoon rush
  filter((end_time > am_rush_start & end_time < am_rush_end) | (end_time > pm_rush_start & end_time < pm_rush_end)) %>% 
  # group by station
  group_by(start_station_name) %>% 
  # get the frequency of trips started for each station
  summarise(frequency = n()) %>% 
  # see the 10 stations with the highest frequency of trips ended during rush hours
  arrange(desc(frequency))

# determine the 10 most frequent starting stations during the weekend
weekend %>% 
  # group by station
  group_by(start_station_name) %>% 
  # get the frequency of trips started for each station
  summarise(frequency = n()) %>% 
  # see the 10 stations with the highest frequency of trips started
  arrange(desc(frequency))

# determine the 10 most frequent ending stations during the weekend
weekend %>% 
  # group by station
  group_by(end_station_name) %>% 
  # get the frequency of trips started for each station
  summarise(frequency = n()) %>% 
  # see the 10 stations with the highest frequency of trips started
  arrange(desc(frequency))

###############################
## Bike Utilization Analysis ##
###############################

#' depending on the date in question, a different number of total bikes will have
#' been available because additional bike stations were installed in the first
#' few months of the year. GOAL: get the average number of bikes available for
#' use in each month.

# first, find the number of bikes available for use on each day of the year

# create a vector containing the date of every day in the year 2014
dates <- seq(from = as.POSIXct("2014-01-01", tz = "UTC"), to = as.POSIXct("2014-12-31", tz = "UTC"), by = "day")

#' create an empty vector which will be used to store how many bikes were available
#' for each day
available_bikes_daily <- c()

# cycle through each day of the year
for(i in 1:365) {
  
  # identify the stations that were installed prior to the date in question
  available_stations <- clean_station[dates[i] >= clean_station$installation_date,]
  
  # get the total number of bikes that were available on that day
  available_bikes_daily[i] <- sum(available_stations$dock_count)
}

#' combine the dates with the daily bike availability into a data frame
#' so that data can be grouped easily in the next step
available_bikes <- data.frame(dates, available_bikes_daily)

#' determine the average number of bikes available for each month
#' (sum of daily availability divided by number of days in the month)
available_bikes <- available_bikes %>% 
  group_by(month(dates)) %>% 
  summarise(avg_availability = sum(available_bikes_daily)/n())

# rename the columns of the data frame for clarity
colnames(available_bikes) <- c("month", "avg_availability")

#' we now have the information needed to accurately calculate utilization.
#' Rather than calculate as (total time used/total time in month),
#' the utilization will be calculated as:
#' total time used / (total time in month * number of bikes available that month).
#' This approach provides more insight into the utilization because several bikes
#' can be used at the same time.
monthly_util <- clean_trip %>%
  mutate(trip_month = month(trip_mp)) %>%
  group_by(trip_month) %>% 
  summarise(utilization = (100*sum(duration)) / (60*24*days_in_month(trip_month[1])*available_bikes$avg_availability[trip_month[1]]))

# plot the % utilization for each month of the year
ggplot(monthly_util, aes(x = factor(trip_month), y = utilization)) +
  geom_line(group = 1, color = "blue") +
  geom_point(color = "black") +
  scale_x_discrete(labels = month.abb) +
  labs(title = "Monthly Bike Utilization",
       x = "Month",
       y = "Utilization (% per bike)") +
  theme_classic()

######################
## Weather Analysis ##
######################

#' create a new column in the clean_trip data frame corresponding to the midpoint
#' date (but not time). date will be one of the variables used in the join later,
#' so this new date column will match the date column in the weather data frame
clean_trip$mp_date <- as.POSIXct(as.Date(clean_trip$trip_mp))

#' create a data frame that has one row for each city and day, with the associated
#' weather data for that day and city, plus the number of bike trips and the
#' average duration of those bike trips for each city on each day
weather_and_trips <- clean_trip %>% 
  left_join(clean_station[, c("id", "city")], by = c("start_station_id" = "id")) %>%
  group_by(city, mp_date) %>% 
  summarise(avg_duration = mean(duration), n_trips = n()) %>%
  left_join(clean_weather, by = c("mp_date" = "date", "city" = "city"))

# create a correlation plot for each city
for(x in unique(weather_and_trips$city)){
  
  # only include one city per plot, also remove zip code since it is numeric but
  # not relevant for the analysis
  tmp <- weather_and_trips %>%
    select(-zip_code) %>% 
    filter(city == x)
  
  # remove non-numeric columns since they cannot be analyzed via cor() function
  tmp <- tmp[sapply(tmp, is.numeric)]
  
  # create the correlation matrix
  cor_matrix <- cor(tmp, use = "pairwise.complete.obs")
  
  # select only relevant columns in the matrix
  cor_matrix <- cor_matrix[c("n_trips", "avg_duration"), !colnames(cor_matrix) %in% c("n_trips", "avg_duration")]
  
  # create the correlation plot
  plot <- corrplot(cor_matrix, method = "circle",
           title = paste("Correlations Between Trips and Weather", " - ", x),
           mar = c(0,0,1,0), cl.pos = "b", cl.ratio = 4, tl.col = "black")
}
