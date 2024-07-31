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

# station data looks clean already. Make a copy to leave the original data as is
clean_station <- station

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
clean_trip$start_date <- mdy_hm(clean_trip$start_date)
clean_trip$end_date <- mdy_hm(clean_trip$end_date)

# zip code has missing values, but zip code likely will not be used later

# CLEANING WEATHER DATA FRAME (using copy of data, not original)

clean_weather <- weather

# check for duplicates, there are none
any(duplicated(clean_weather))

# no noticeable outliers or missing values

