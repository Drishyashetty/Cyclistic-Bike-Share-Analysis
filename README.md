# Cyclistic-Bike-Share-Analysis:Google_Data_Analytics_Capstone_project

library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(janitor)

#Importing 12 Months of Trip Data
apr2020<- read_csv("202004-divvy-tripdata.csv")
may2020<- read_csv("202005-divvy-tripdata.csv")
jun2020<- read_csv("202006-divvy-tripdata.csv")
jul2020<- read_csv("202007-divvy-tripdata.csv")
aug2020<- read_csv("202008-divvy-tripdata.csv")
sep2020<- read_csv("202009-divvy-tripdata.csv")
oct2020<- read_csv("202010-divvy-tripdata.csv")
nov2020<- read_csv("202011-divvy-tripdata.csv")
dec2020<- read_csv("202012-divvy-tripdata.csv")
jan2021<- read_csv("202101-divvy-tripdata.csv")
feb2021<- read_csv("202102-divvy-tripdata.csv")
mar2021<- read_csv("202103-divvy-tripdata.csv")

# Comparing columns before the merge
compare_df_cols_same(apr2020,may2020,jun2020,jul2020, aug2020, sep2020,oct2020,nov2020, dec2020,jan2021,feb2021,mar2021)

# Changing end_station_id and start_station_id into character so the data can be merged
apr2020<- mutate(apr2020, end_station_id= as.character(end_station_id), start_station_id=as.character(start_station_id))
may2020<- mutate(may2020, end_station_id= as.character(end_station_id), start_station_id=as.character(start_station_id))
jun2020<- mutate(jun2020, end_station_id= as.character(end_station_id), start_station_id=as.character(start_station_id))
jul2020<- mutate(jul2020, end_station_id= as.character(end_station_id), start_station_id=as.character(start_station_id))
aug2020<- mutate(aug2020, end_station_id= as.character(end_station_id), start_station_id=as.character(start_station_id))
sep2020<- mutate(sep2020, end_station_id= as.character(end_station_id), start_station_id=as.character(start_station_id))
oct2020<- mutate(oct2020, end_station_id= as.character(end_station_id), start_station_id=as.character(start_station_id))
nov2020<- mutate(nov2020, end_station_id= as.character(end_station_id), start_station_id=as.character(start_station_id))
dec2020<- mutate(dec2020, end_station_id= as.character(end_station_id), start_station_id=as.character(start_station_id))
jan2021<- mutate(jan2021, end_station_id= as.character(end_station_id), start_station_id=as.character(start_station_id))
feb2021<- mutate(feb2021, end_station_id= as.character(end_station_id), start_station_id=as.character(start_station_id))
mar2021<- mutate(mar2021, end_station_id= as.character(end_station_id), start_station_id=as.character(start_station_id))

# Re-Check if the columns can be merged or not
compare_df_cols_same(apr2020,may2020,jun2020,jul2020, aug2020, sep2020,oct2020,nov2020, dec2020,jan2021,feb2021,mar2021)

# Merging all 12 months of data into 1 raw database
trip_data<- do.call("rbind", list(apr2020,may2020,jun2020,jul2020,aug2020,sep2020,oct2020,nov2020,dec2020,jan2021,feb2021,mar2021))

View(trip_data)    

# Clean the environment except for the raw database
rm(list=setdiff(ls(),'trip_data'))

# Removing unused columns
trip_data <- trip_data[,-c(1,9:12)]

# Calculating ride length as hours
trip_data$ride_length<- round(difftime(trip_data$ended_at, trip_data$started_at, units='hours'),2)

# Define trip days
trip_data$trip_days <- weekdays(trip_data$started_at)

# Checking bad ride length
sum(trip_data$ride_length < 0)

# Get rid of bad ride length data
trip_data <- trip_data[!(trip_data$ride_length < 0),]

# Create breaks
breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))

# Labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")

# Defining time of the day (morning, afternoon, evening, night)
trip_data$time_of_the_trip <- cut(x=hour(trip_data$started_at), breaks = breaks, labels = labels, include.lowest=TRUE)

# Write our cleaned and prepared data into csv
write_csv(trip_data, "C:/Users/HP/Desktop/CASE STUDY 1/ Final_Trip_Data.csv")
