#library
library(tidyverse)
library(lubridate)
library(ggplot2)

setwd("E:/googledatanalytics/12monthbikedataset")

#data from https://divvy-tripdata.s3.amazonaws.com/index.html
data_2022_01 <- read.csv("202201-divvy-tripdata.csv")
data_2022_02 <- read.csv("202202-divvy-tripdata.csv")
data_2022_03 <- read.csv("202203-divvy-tripdata.csv")
data_2022_04 <- read.csv("202204-divvy-tripdata.csv")
data_2022_05 <- read.csv("202205-divvy-tripdata.csv")
data_2022_06 <- read.csv("202206-divvy-tripdata.csv")
data_2022_07 <- read.csv("202207-divvy-tripdata.csv")
data_2022_08 <- read.csv("202208-divvy-tripdata.csv")
data_2022_09 <- read.csv("202209-divvy-tripdata.csv")
data_2022_10 <- read.csv("202210-divvy-tripdata.csv")
data_2022_11 <- read.csv("202211-divvy-tripdata.csv")
data_2022_12 <- read.csv("202212-divvy-tripdata.csv")

#combine all tables into single dataframe
data_2022_12_months <- rbind(data_2022_01
                             , data_2022_02
                             , data_2022_03
                             , data_2022_04
                             , data_2022_05
                             , data_2022_06
                             , data_2022_07
                             , data_2022_08
                             , data_2022_09
                             , data_2022_10
                             , data_2022_11
                             , data_2022_12)
#drop monthly files from the environment
remove(data_2022_01
        , data_2022_02
        , data_2022_03
        , data_2022_04
        , data_2022_05
        , data_2022_06
        , data_2022_07
        , data_2022_08
        , data_2022_09
        , data_2022_10
        , data_2022_11
        , data_2022_12)
#copy of df                          
data_2022_12_months_2 <- data_2022_12_months

#create column ride length in minutes
data_2022_12_months_2$ride_length_mins <- difftime(data_2022_12_months_2$ended_at
                                              , data_2022_12_months$started_at
                                              , units = "mins")
#create column day of week in text
data_2022_12_months_2$day_of_week <- wday(data_2022_12_months_2$started_at)

#create numeric columns for the day of week, date, month, day, year
data_2022_12_months_2$date <- as.Date(data_2022_12_months_2$started_at)
data_2022_12_months_2$day_of_week <- format(as.Date(data_2022_12_months_2$date), "%A")
data_2022_12_months_2$month <- format(as.Date(data_2022_12_months_2$date), "%m") 
data_2022_12_months_2$day <- format(as.Date(data_2022_12_months_2$date), "%d")
data_2022_12_months_2$year <- format(as.Date(data_2022_12_months_2$date), "%Y")                                 
    
#create column for seasons classification
data_2022_12_months_2 <- data_2022_12_months_2 %>% mutate(season = 
                                                           case_when(month == "03" ~ "Spring",
                                                                             month == "04" ~ "Spring",
                                                                             month == "05" ~ "Spring",
                                                                             month == "06" ~ "Summer",
                                                                             month == "07" ~ "Summer",
                                                                             month == "08" ~ "Summer",
                                                                             month == "09" ~ "Fall",
                                                                             month == "10" ~ "Fall",
                                                                             month == "11" ~ "Fall",
                                                                             month == "12" ~ "Winter",
                                                                             month == "01" ~ "Winter",
                                                                             month == "02" ~ "Winter")
)
#arrange days of the week in order
data_2022_12_months_2$day_of_week <- ordered(data_2022_12_months_2$day_of_week,
                                             levels=c("Monday",
                                                      "Tuesday",
                                                      "Wednesday",
                                                      "Thursday",
                                                      "Friday",
                                                      "Saturday",
                                                      "Sunday"))
#arrange seasons in order
data_2022_12_months_2$season <- ordered(data_2022_12_months_2$season, levels=c("Spring", "Summer", "Fall", "Winter"))
#remove duplicate rows
#data_2022_12_months_2 %>% group_by_all %>% count #count check
data_2022_12_months_2 <- distinct(data_2022_12_months_2)

#remove rows with null values
data_2022_12_months_2 <- na.omit(data_2022_12_months_2)

#remove rows where ride_length_mins is <= to zero
data_2022_12_months_2 <- data_2022_12_months_2[!(data_2022_12_months_2$ride_length_mins <=0),]

#remove unneeded columns
data_2022_12_months_2 <- data_2022_12_months_2[ , ! names(data_2022_12_months_2) %in% c("ride_id", "start_station_id", "end_station_id"
                                                    , "start_lat", "start_lng", "end_lat", "end_lng")]
#rename member_casual to member_type
data_2022_12_months_2 <- data_2022_12_months_2 %>% rename(member_type = member_casual)

#convert ride_length_mins to numeric
data_2022_12_months_2$ride_length_mins <- as.numeric(as.character(data_2022_12_months_2$ride_length_mins))

#View cleaned df
View(data_2022_12_months_2)

#Data Analysis & Visualization#

#total number of rides (approximately 57 million rides)
nrow(data_2022_12_months_2)

#total number of rides rounded 
round(nrow(data_2022_12_months_2), digits = -5)

#count member types
 # casual 2316600
 # member 3344728
data_2022_12_months_2 %>% count(member_type)

 #count of bike types
 # classic_bike  2597846
 # docked_bike    174852
 # electric_bike 2888630
data_2022_12_months_2 %>% group_by(rideable_type) %>% count(rideable_type)

#plot 1 bicycle type by number of rides
  data_2022_12_months_2 %>%
    group_by(rideable_type, member_type) %>%
    summarize(count_trips = n()) %>%  
    ggplot(aes(x=rideable_type, y=count_trips, fill=member_type, color=member_type)) +
    geom_bar(stat='identity', position='dodge') +
    theme_bw()+
    labs(title="Number of Trips by Bicycle Type", x="Bicycle Type", y="Number of Trips")
  
#total rides by bike type by member type
  # casual      classic_bike   888853
  # casual      docked_bike    174852
  # casual      electric_bike 1252895
  # member      classic_bike  1708993
  # member      electric_bike 1635735
  data_2022_12_months_2 %>%
    group_by(member_type, rideable_type) %>% 
    count(rideable_type)
#min, max, median, mean length of ride
  min(data_2022_12_months_2$ride_length_mins)    #0.01666667 mins about 1 sec
  max(data_2022_12_months_2$ride_length_mins)    #34294.07 mins
  median(data_2022_12_months_2$ride_length_mins) #10.26667 mins
  mean(data_2022_12_months_2$ride_length_mins)   #16.33202 mins
  aggregate(data_2022_12_months_2$ride_length_mins ~ data_2022_12_months_2$member_type, FUN = mean)
  aggregate(data_2022_12_months_2$ride_length_mins ~ data_2022_12_months_2$member_type, FUN = median)
  aggregate(data_2022_12_months_2$ride_length_mins ~ data_2022_12_months_2$member_type, FUN = min)
  aggregate(data_2022_12_months_2$ride_length_mins ~ data_2022_12_months_2$member_type, FUN = max)

#converts values from scientific notation <- run this for y axis proper formatting
  options(scipen = 999)
#plot 2 number of rides by day of week  
  data_2022_12_months_2 %>% 
    group_by(member_type, day_of_week) %>%
    summarize(count_trips = n()) %>%  
    ggplot(aes(x=day_of_week, y=count_trips, fill=member_type, color=member_type)) +
    geom_bar(stat='identity', position = 'dodge') +
    theme_bw()+
    labs(title ="Number of Rides by Day of Week", x = "Day of Week", y = "Number of Rides")
  
#arranges months in order
  data_2022_12_months_2$month <- ordered(data_2022_12_months_2$month, levels=c("01", "02", "03", "04", "05", "06", "07","08","09","10","11","12"))
#plot 3 number of rides per month
  data_2022_12_months_2 %>%
    group_by(member_type, month) %>%
    summarize(count_trips = n()) %>%  
    ggplot(aes(x=month, y=count_trips, fill=member_type, color=member_type)) +
    geom_bar(stat='identity', position = 'dodge') +
    scale_x_discrete(labels=c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')) +
    theme_bw() +
    labs(title ="Number of Rides per Month", x = "Month", y = "Number of Trips")

#plot 4 number of rides per month stacking bar graph 
  data_2022_12_months_2 %>%
    group_by(member_type, month) %>%
    summarise(count_trips = n()) %>%  
    ggplot(aes(x=month, y=count_trips, fill=member_type, color=member_type)) +
    geom_bar(stat='identity') +
    scale_x_discrete(labels=c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')) +
    theme_bw() +
    labs(title ="Number of Rides per Month", x = "Month", y = "Number of Trips")
  
#plot 5 number of rides per season
  data_2022_12_months_2 %>%
    group_by(member_type, season) %>% 
    summarise(count_trips = n()) %>% 
    ggplot(aes(x=season, y=count_trips, fill=member_type, color=member_type)) +
    geom_bar(stat='identity', position='dodge') +
    theme_bw() +
    labs(title ="Number of Rides per Season", x = "Seasons", y = "Number of Trips")

#total rides by season and member type
 #Spring casual       495475
 #Spring member       793272
 #Summer casual      1131336
 #Summer member      1244228
 #Fall   casual       605182
 #Fall   member       990965
 #Winter casual        84607
 #Winter member       316263
  data_2022_12_months_2 %>%
    group_by(season, member_type) %>% 
    count(season)
  
#total rides by season
 #Spring 1288747
 #Summer 2375564
 #Fall   1596147
 #Winter  400870
  data_2022_12_months_2 %>%
    group_by(season) %>% 
    count(season)

#Find popular start station for casual riders
 #casual      Streeter Dr & Grand Ave                     57844
 #casual      DuSable Lake Shore Dr & Monroe St           31747
 #casual      Millennium Park                             25400
 #casual      Michigan Ave & Oak St                       25225
 #casual      DuSable Lake Shore Dr & North Blvd          23583
  data_2022_12_months_2 %>%
    group_by(member_type, start_station_name) %>%
    summarise(number_of_ride = n()) %>%
    filter(start_station_name != "", "casual"== member_type) %>%
    arrange(-number_of_ride) %>%
    head(n=5) %>%
    select(-member_type)
  
#Find popular start station for member riders
 #member      Kingsbury St & Kinzie St              24932
 #member      Clark St & Elm St                     22028
 #member      Wells St & Concord Ln                 21294
 #member      University Ave & 57th St              19941
 #member      Clinton St & Washington Blvd          19825
  data_2022_12_months_2 %>%
    group_by(member_type,start_station_name) %>%
    dplyr::summarise(number_of_ride = n()) %>%
    filter(start_station_name != "", "member" == member_type) %>%
    arrange(-number_of_ride) %>%
    head(n=5) %>%
    select(-member_type)
  
#plot 6 average length ride per day of week  
  data_2022_12_months_2 %>%
    group_by(member_type, day_of_week) %>% 
    summarise(mean_rides = mean(ride_length_mins)) %>%
    ggplot(aes(x = day_of_week, y = mean_rides, fill = member_type)) +
    geom_col(position = "dodge") +
    labs(title = "Ride Length Averages by Weekday", 
         subtitle = "From the Cyclistic Bike Trips data set", 
         x = "Weekdays", y = "Average Length of Rides in Minutes")

