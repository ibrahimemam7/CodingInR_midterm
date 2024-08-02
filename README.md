CodingInR_midterm

data codebook: https://www.kaggle.com/datasets/benhamner/sf-bay-area-bike-share/discussion/23165

------------PLAN---------------

1. EDA
  - follow link: https://blog.datascienceheroes.com/exploratory-data-analysis-in-r-intro/

2. Data Cleaning
  - station data looks clean already
  - CLEANING TRIP DATA FRAME:
    - remove outliers for trip duration
    - indicate missing values with NA
    - format times as POSIX
    - remove very short trips that began and started at the same station
      (these trips were likely cancelled)

3. Rush hour analysis
  - create column containing midpoint of start and end time
  - create subset of data containing only weekdays
  - create subset of data containing only weekends
  - create histogram of trip frequency based on midpoint for weekdays
  - identify rush hours via visual inspection of histogram

4. Busy stations analysis
  - create new columns for start and end times in the weekday/weekend data       frames.
    These will either need no date (just time) or a fixed date so they can easily be
    used in the filter function to determine if a trip started/ended in rush hours,
    regardless of the day.
  - use simpler dplyr code to identify top 10 busiest stations

5. Utilization analysis
  - determine average number of total bikes available for each month
  - calculate the average utilization (per bike) for each month
  - create a figure (likely a line graph) to represent the utilization

6. Weather analysis
  - join station data to trips data (this tells us the starting city of each trip)
  - join weather data to the data frame above based on city and date
  - determine correlation of weather with number of trips