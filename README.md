CodingInR_midterm

------------PLAN---------------

1. EDA
  - follow link: 
2. Data Cleaning
  - station data looks clean already
  - CLEANING TRIP DATA FRAME:
    - remove outliers for trip duration
    - indicate missing values with NA
    - format times as POSIX
    - remove very short trips that began and started at the same station
      (these trips were likely cancelled)
    