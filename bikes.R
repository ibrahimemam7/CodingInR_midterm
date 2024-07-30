#' Coding in R - Midterm Project
#' Ibrahim Emam
#' July/29/2024

# start by importing the csv files into data frames
station <- read.csv("raw_data/station.csv")
trip <- read.csv("raw_data/trip.csv")
weather <- read.csv("raw_data/weather.csv")

# get an idea of what information is contained within each data frame
head(station)
summary(station)

head(trip)
summary(station)

head(weather)
summary(weather)