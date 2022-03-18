#Installing packages
install.packages("tidyverse")
install.packages("skimr")
install.packages("ggplot2")
install.packages("janitor")
install.packages("lubridate")
install.packages("DescTools")

#Loading Datasets
Jan<-read_csv("~//Downloads//202101-divvy-tripdata.csv")
Feb<-read_csv("~//Downloads//202102-divvy-tripdata.csv")
Mar<-read_csv("~//Downloads//202103-divvy-tripdata.csv")
Apr<-read_csv("~//Downloads//202104-divvy-tripdata.csv")
May<-read_csv("~//Downloads//202105-divvy-tripdata.csv")
Jun<-read_csv("~//Downloads//202106-divvy-tripdata.csv")
Jul<-read_csv("~//Downloads//202107-divvy-tripdata.csv")
Aug<-read_csv("~//Downloads//202108-divvy-tripdata.csv")
Sep<-read_csv("~//Downloads//202109-divvy-tripdata.csv")
Oct<-read_csv("~//Downloads//202110-divvy-tripdata.csv")
Nov<-read_csv("~//Downloads//202111-divvy-tripdata.csv")
Dec<-read_csv("~//Downloads//202112-divvy-tripdata.csv")

#Checking the spec/ datatype of the datasets
spec(Jan)
spec(Feb)
spec(Mar)
spec(Apr)
spec(May)
spec(Jun)
spec(Jul)
spec(Aug)
spec(Sep)
spec(Oct)
spec(Nov)
spec(Dec)

# Removing empty values
Jan <- Jan[complete.cases(Jan), ]
Feb <- Feb[complete.cases(Feb), ]
Mar <- Mar[complete.cases(Mar), ]
Apr <- Apr[complete.cases(Apr), ]
May <- May[complete.cases(May), ]
Jun <- Jun[complete.cases(Jun), ]
Jul <- Jul[complete.cases(Jul), ]
Aug <- Aug[complete.cases(Aug), ]
Sep <- Sep[complete.cases(Sep), ]
Oct <- Oct[complete.cases(Oct), ]
Nov <- Nov[complete.cases(Nov), ]
Dec <- Dec[complete.cases(Dec), ]

#Converting start_station_id and end_station_id dtype from char to int
Jan <- mutate(Jan, 
              start_station_id = as.integer(start_station_id),
              end_station_id = as.integer(end_station_id))

Feb <- mutate(Feb, 
              start_station_id = as.integer(start_station_id),
              end_station_id = as.integer(end_station_id))

Mar <- mutate(Mar, 
              start_station_id = as.integer(start_station_id),
              end_station_id = as.integer(end_station_id))

Apr <- mutate(Apr, 
              start_station_id = as.integer(start_station_id),
              end_station_id = as.integer(end_station_id))

May <- mutate(May, 
              start_station_id = as.integer(start_station_id),
              end_station_id = as.integer(end_station_id))

Jun <- mutate(Jun, 
              start_station_id = as.integer(start_station_id),
              end_station_id = as.integer(end_station_id))

Jul <- mutate(Jul, 
              start_station_id = as.integer(start_station_id),
              end_station_id = as.integer(end_station_id))

Aug <- mutate(Aug, 
              start_station_id = as.integer(start_station_id),
              end_station_id = as.integer(end_station_id))

Sep <- mutate(Sep, 
              start_station_id = as.integer(start_station_id),
              end_station_id = as.integer(end_station_id))

Oct <- mutate(Oct, 
              start_station_id = as.integer(start_station_id),
              end_station_id = as.integer(end_station_id))

Nov <- mutate(Nov, 
              start_station_id = as.integer(start_station_id),
              end_station_id = as.integer(end_station_id))

Dec <- mutate(Dec, 
              start_station_id = as.integer(start_station_id),
              end_station_id = as.integer(end_station_id))

#Combining all datasets
tripdata<-rbind(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
glimpse(tripdata)

#Removing rows that have a greater started_at against ended_at
tripdata <- tripdata %>% 
  filter(tripdata$started_at < tripdata$ended_at)

#Creating date, day, month, year, day_of_week, ride_length columns
tripdata$date <- as.Date(tripdata$started_at)
tripdata$day <- format(as.Date(tripdata$date), "%d")
tripdata$month <- format(as.Date(tripdata$date), "%m")
tripdata$year <- format(as.Date(tripdata$date), "%Y")
tripdata$weekday <- format(as.Date(tripdata$date), "%A")
tripdata$weekday <- ordered(tripdata$weekday, 
                        levels=c("Sunday", "Monday", "Tuesday",
                        "Wednesday", "Thursday", "Friday", "Saturday"))
tripdata$ride_length <- difftime(tripdata$ended_at, tripdata$started_at)

#Converting ride_length column dtype from drtn to num
tripdata$ride_length <- as.numeric(as.character(tripdata$ride_length))

#Showing the comparison between member and casual rides
aggregate(tripdata["ride_length"],by= tripdata["member_casual"],FUN = "mean")
aggregate(tripdata["ride_length"],by= tripdata["member_casual"],FUN = "median")
aggregate(tripdata["ride_length"],by= tripdata["member_casual"],FUN = "min")
aggregate(tripdata["ride_length"],by= tripdata["member_casual"],FUN = "max")

#Showing the comparison between the average ride length of each week day against
#each user type
aggregate(tripdata$ride_length ~ tripdata$member_casual + tripdata$day_of_week,
          FUN = mean)

#Showing the number of rides by weekday, ride type and average ride_length
tripdata2 <- tripdata %>%  
             mutate(weekday = format(as.Date(tripdata$date), "%A")) %>% 
             #groups by user-type and weekday
             group_by(member_casual, weekday) %>%   
             #cal number of rides and average duration
             summarise(number_of_rides = n(), 
                       avearge_duration = mean(ride_length)) %>%   
             arrange(member_casual, weekday) 

#Showing the number of rides by month, ride type and average ride_length
tripdata3 <- tripdata %>%  
             mutate(month = format(as.Date(tripdata$date), "%B")) %>% 
             #groups by user-type and month
             group_by(member_casual, month) %>%   
             #cal number of rides and average duration
             summarise(number_of_rides = n(), 
             avearge_duration = mean(ride_length)) %>%   
             arrange(member_casual, month) 

#Vizualizing the average ride length for each weekday
tripdataviz1 <- tripdata %>%
  mutate(weekday = format(as.Date(tripdata$date), "%A")) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), 
            avearge_duration = mean(ride_length)) %>%   
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides,fill=member_casual))+
  geom_col(position = "dodge")+
  labs(title ="Showing the average ride length for each weekday",
       subtitle = "Members Vs. Casual Users")+
  ylab("Number of Rides")+
  xlab("Week Day")

#Vizualizing the number of rides by ride type
tripdataviz2<-tripdata %>% 
  mutate(weekday = format(as.Date(tripdata$date), "%A")) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  scale_fill_manual(values = c("#CC6633","#6699CC")) +
  labs(title = "Number of Rides by Days and Rider Type",
       subtitle = "Members versus Casual Users") +
  ylab("Number of Rides") +
  xlab("Week Day")

#Showing casual riders trip duration pattern with members (mean)
ride_length_mean <- aggregate(tripdata$ride_length ~ tripdata$member_casual, FUN = mean)
colnames(ride_length_mean) <- c("ride_length", "member_casual")

#Showing casual riders trip duration pattern with members (median)
ride_length_median <- aggregate(tripdata$ride_length ~ tripdata$member_casual, FUN = median)
colnames(ride_length_median) <- c("ride_length", "member_casual")

#Showing the counts of casual member rides where mean of member trip_duration is smaller
tripdata %>% filter(member_casual == 'casual', ride_length>= ride_length_mean$member_casual[2]) %>%
  summarise(number_of_riders = n())

#Showing the counts casual member rides where mean of member trip_duration is smaller
tripdata %>% filter(member_casual == 'casual', ride_length>= ride_length_median$member_casual[2]) %>%
  summarise(number_of_riders = n()) 





