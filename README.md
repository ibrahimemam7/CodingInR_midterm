CodingInR_midterm

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
  - create new columns for start and end times in the weekday/weekend data frames.
    These will either need no date (just time) or a fixed date so they can easily be
    used in the filter function to determine if a trip started/ended in rush hours,
    regardless of the day.
  - use simpler dplyer code to identify top 10 busiest stations
    