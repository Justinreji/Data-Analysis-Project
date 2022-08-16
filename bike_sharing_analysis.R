
# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  


library(tidyverse) #helps wrangle data
library(ggplot2)   #helps visualize data
library(lubridate) #helps wrangle date attributes

getwd()  #displays your working directory
setwd("C:/Users/Justin/OneDrive/Documents/R Coding/Cyclistic_data/") #sets your working directory to simplify calls to data

#=====================
# STEP 1: COLLECT DATA
#=====================

tripdata_202103 <- read.csv("202103-divvy-tripdata.csv") 
tripdata_202104 <- read.csv("202104-divvy-tripdata.csv") 
tripdata_202105 <- read.csv("202105-divvy-tripdata.csv") 
tripdata_202106 <- read.csv("202106-divvy-tripdata.csv") 
tripdata_202107 <- read.csv("202107-divvy-tripdata.csv") 
tripdata_202108 <- read.csv("202108-divvy-tripdata.csv") 
tripdata_202109 <- read.csv("202109-divvy-tripdata.csv") 
tripdata_202110 <- read.csv("202110-divvy-tripdata.csv") 
tripdata_202111 <- read.csv("202111-divvy-tripdata.csv") 
tripdata_202112 <- read.csv("202112-divvy-tripdata.csv") 
tripdata_202201 <- read.csv("202201-divvy-tripdata.csv") 
tripdata_202202 <- read.csv("202202-divvy-tripdata.csv") 

#Check Column names for each data set for consistency

colnames(tripdata_202103)
colnames(tripdata_202104)
colnames(tripdata_202105)
colnames(tripdata_202106)
colnames(tripdata_202107)
colnames(tripdata_202108)
colnames(tripdata_202109)
colnames(tripdata_202110)
colnames(tripdata_202111)
colnames(tripdata_202112)
colnames(tripdata_202201)
colnames(tripdata_202202)

#Check data structures and data types for all data frames
str(tripdata_202103)
str(tripdata_202104)
str(tripdata_202105)
str(tripdata_202106)
str(tripdata_202107)
str(tripdata_202108)
str(tripdata_202109)
str(tripdata_202110)
str(tripdata_202111)
str(tripdata_202112)
str(tripdata_202201)
str(tripdata_202202)

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================

all_trips=bind_rows(tripdata_202103,tripdata_202104,tripdata_202105,tripdata_202106,tripdata_202107,tripdata_202108, tripdata_202109,tripdata_202110,tripdata_202111,tripdata_202112,tripdata_202201,tripdata_202202)

str(all_trips)

#Removing unwanted columns

all_trips <- all_trips %>% select(-c(start_lat:end_lng))
glimpse(all_trips)

#Changing the data type of started_at and ended_at from char to datetime datatype

all_trips[['started_at']] <- ymd_hms(all_trips[['started_at']])
all_trips[['ended_at']] <- ymd_hms(all_trips[['ended_at']])

# Renaming columns to make them consistent and understandable

all_trips <- rename(all_trips,
                     trip_id = ride_id,
                     bike_type = rideable_type,
                     start_time = started_at,
                     end_time = ended_at,
                     from_station_name = start_station_name,
                     from_station_id = start_station_id,
                     to_station_name = end_station_name,
                     to_station_id = end_station_id,
                     usertype = member_casual)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================

# Inspect the new table that has been created
colnames(all_trips) #List of column names
nrow(all_trips) #Total no. of rows
dim(all_trips)  #Dimensions of the data frame
head(all_trips,10) #First 10 rows
tail(all_trips,10) #Last 10 rows
str(all_trips)

summary(all_trips)  #Statistical summary of data

table(all_trips$usertype) #Checking how many casual riders and members are there

# Add columns that list the date, month, day, and year of each ride
all_trips$date <- as.Date(all_trips$start_time)
all_trips$month <- format(as.Date(all_trips$date), '%m')
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_weak <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$end_time,all_trips$start_time,units = 'mins')

str(all_trips)

# Convert "ride_length" to numeric so we can run calculations on the data
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The data frame includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the data frame (v2) since data is being removed
all_trips_v2 <- all_trips[!(all_trips$from_station_name == "HQ QR" | all_trips$ride_length<=0),]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

# Descriptive analysis on ride_length (all figures in seconds)
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = min)

# See the average ride time by each day for members vs casual users
count <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype+all_trips_v2$day_of_weak, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(start_time)) %>%  #creates weekday field using wday()
  group_by(usertype, weekday) %>%  #groups by user type and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(usertype, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(start_time)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(start_time)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
write.csv(count, file = "C:/Users/Justin/OneDrive/Documents/R Coding/Cyclistic_data/avg_length.csv")
write.csv(all_trips_v2, file = "C:/Users/Justin/OneDrive/Documents/R Coding/Cyclistic_data/All_trips_data.csv")
