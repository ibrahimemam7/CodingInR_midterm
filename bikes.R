#' Coding in R - Midterm Project
#' Ibrahim Emam
#' July/29/2024

#import relevant libraries
library(tidyverse)
library(funModeling)
library(lubridate)

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

# no noticeable outliers or missing values

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

dates <- seq(from = as.POSIXct("2014-01-01", tz = "UTC"), to = as.POSIXct("2014-12-31", tz = "UTC"), by = "day")

for(x in dates) {
  available_stations <- clean_station[clean_station$installation_date <= x,]
  print(length(available_stations))
}

clean_trip %>% 
  group_by(month(start_date)) %>% 
  summarize(utilization = )