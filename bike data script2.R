# installed packages necessary for data analysis and any date formatting issues
# library used to make all functions available
install.packages("lubridate")
install.packages("tidyverse")
library(lubridate)
library(tidyverse)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("scales")
library(scales)
#imported all 12 edited excel files, noting that they run from November, 2020 to October, 2021
nov <- read_csv("/Users/owner/Documents/GoogleDA_Files/Verifiable Files/csv of final files/2020_11_Bikes_Final.csv")
dec <- read_csv("/Users/owner/Documents/GoogleDA_Files/Verifiable Files/csv of final files/2020_12_Bikes_Final.csv")
jan <- read_csv("/Users/owner/Documents/GoogleDA_Files/Verifiable Files/csv of final files/2021_01_Bikes_Final.csv")
feb <- read_csv("/Users/owner/Documents/GoogleDA_Files/Verifiable Files/csv of final files/2021_02_Bikes_Final.csv")
mar <- read_csv("/Users/owner/Documents/GoogleDA_Files/Verifiable Files/csv of final files/2021_03_Bikes_Final.csv")
apr <- read_csv("/Users/owner/Documents/GoogleDA_Files/Verifiable Files/csv of final files/2021_04_Bikes_Final.csv")
may <- read_csv("/Users/owner/Documents/GoogleDA_Files/Verifiable Files/csv of final files/2021_05_Bikes_Final.csv")
june <- read_csv("/Users/owner/Documents/GoogleDA_Files/Verifiable Files/csv of final files/2021_06_Bikes_Final.csv")
july <- read_csv("/Users/owner/Documents/GoogleDA_Files/Verifiable Files/csv of final files/2021_07_Bikes_Final.csv")
aug <- read_csv("/Users/owner/Documents/GoogleDA_Files/Verifiable Files/csv of final files/2021_08_Bikes_Final.csv")
sept <- read_csv("/Users/owner/Documents/GoogleDA_Files/Verifiable Files/csv of final files/2021_09_Bikes_Final.csv")
oct <- read_csv("/Users/owner/Documents/GoogleDA_Files/Verifiable Files/csv of final files/2021_10_Bikes_Final.csv")
# after import, examined columns of each file to check column types matched and could be merged
str(nov)
str(dec)
str(jan)
str(feb)
str(mar)
str(apr)
str(may)
str(june)
str(july)
str(aug)
str(sept)
str(oct)
#found that the 2020 months had different column types, so changed November column types, removed NA values that came from end dates occurring after start dates
nov$end_station_id <- as.character(nov$end_station_id)
nov$ride_length_seconds <- as.double(nov$ride_length_seconds)
nov <- nov %>% drop_na(ride_length_seconds)
nov$start_station_id <- as.character(nov$start_station_id)
#repeated November changes for December, data format now matches for all 12 months
dec$ride_length_seconds <- as.double(dec$ride_length_seconds)
dec <- dec %>% drop_na(ride_length_seconds)
#checked that data frames still looked proper, also reran prior commands of str()
View(dec)
View(nov)
#combined the 12 dataframes into 1, called "year"
year <- bind_rows(jan, feb, mar, apr, may, june, july, aug, sept, oct, nov, dec, .id = "month")
View(year)


#Cleaning:
year <- year %>% drop_na(ride_length_seconds)
#above drops rows with no ride length or negative ride length, below checks for duplicates
year[duplicated(year), ]
#note that while there were 865 NAs, there were zero duplicate rows
#combined all rows to make 5.4 million row dataframe for analysis


#Analysis:
save(year, file="bike_data_combined.RData")
str(year)
summary(year$ride_length_seconds)
#the following generated summary data on ride length, grouped by membership type
year %>%
  group_by(member_casual) %>%
  summarize(min = min(ride_length_seconds),
            mean = mean(ride_length_seconds),
            max = max(ride_length_seconds))
#as the above lines were separated by member type, the lines below added in grouping by weekdays
year %>%
  group_by(member_casual, weekday) %>%
  summarize(min = min(ride_length_seconds),
            mean = mean(ride_length_seconds),
            max = max(ride_length_seconds))
#the following lines removed the member type grouping and sorted by average ride length:
year %>% group_by(weekday) %>% count(weekday, sort=TRUE)
year %>%
  group_by(weekday) %>%
  summarize(min = min(ride_length_seconds),
            mean = mean(ride_length_seconds),
            max = max(ride_length_seconds))
#the following two lines give tibbles of total bike rides by day, then also by membership
year %>% count(weekday, sort = TRUE)
year %>% count(weekday, member_casual, sort=TRUE)
#how many casual rides lasted longer than 30 minutes?
year %>% subset(member_casual=="casual") %>% count(ride_length_seconds>1800)
#how many lasted longer than 3 hours?
year %>% subset(member_casual=="casual") %>% count(ride_length_seconds>10800)


#Graphing:
#here I began graphing various bar charts to examine membership differences
#the first graph iss just a combined bar chart, the second is two charts differentiated by membership type
ggplot(data = year) + geom_bar(mapping=aes(x=weekday, fill = member_casual))
ggplot(data = year) + geom_bar(mapping=aes(x=weekday, fill = member_casual)) + facet_wrap(~member_casual)
#the following two lines were written for ease of axes labels
Day_of_Week <- c("Sun.", "Mon.", "Tues.", "Wed.", "Thur.", "Fri.", "Sat.")
Abr_Day_of_Week <- c("Su", "M", "T", "W", "Th", "F", "S")
#the following lines created the graph comparing number of total rides each day, separated by membership type
ggplot(data=year, aes(x=weekday, fill=member_casual)) +
  geom_bar(position="dodge") + 
  scale_x_continuous("Weekday", labels=Day_of_Week, breaks = c(1:7)) + 
  scale_y_continuous("Total Bike Rides", labels = comma) + 
  scale_fill_discrete(name = "Member Type") + 
  labs(title = "Total Bike Rides By Day and Membership Type") + 
  theme(plot.title = element_text(hjust = 0.5))
# the following lines plotted average ride length by day and membership type
ggplot(data=year) +
  stat_summary(
    mapping = aes(x = weekday, y = ride_length_seconds, fill= as.factor(weekday)),
    fun = mean,
    geom = "bar",
  ) + facet_wrap(~member_casual) + 
  scale_x_continuous("Weekday", labels=Abr_Day_of_Week, breaks = c(1:7)) + 
  scale_y_continuous("Average Ride Length in Seconds", labels = comma) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill=guide_legend(title="Day")) +
  labs(title = "Average Ride Length By Day and Membership Type") +
  theme(plot.title = element_text(hjust = 0.5))
#the next lines repeated the prior ones, but without a legend
ggplot(data=year) +
  stat_summary(
    mapping = aes(x = weekday, y = ride_length_seconds, fill= as.factor(weekday)),
    fun = mean,
    geom = "bar",
  ) + facet_wrap(~member_casual) + 
  scale_x_continuous("Weekday", labels=Abr_Day_of_Week, breaks = c(1:7)) + 
  scale_y_continuous("Average Ride Length in Seconds", labels = comma) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill="none") +
  labs(title = "Average Ride Length By Day and Membership Type") +
  theme(plot.title = element_text(hjust = 0.5))
#the following turned the above graph into one of average minutes, not seconds
ggplot(data=year) +
  stat_summary(
    mapping = aes(x = weekday, y = (ride_length_seconds)/60, fill= as.factor(weekday)),
    fun = mean,
    geom = "bar",
  ) + facet_wrap(~member_casual) + 
  scale_x_continuous("Weekday", labels=Abr_Day_of_Week, breaks = c(1:7)) + 
  scale_y_continuous("Average Ride Length in Minutes", labels = comma) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill="none") +
  labs(title = "Average Ride Length By Day and Membership Type") +
  theme(plot.title = element_text(hjust = 0.5))
#the following plotted total rides by weekday, with separate plots for each month, but it failed to properly order the multiple plots
ggplot(data = year) + 
  geom_bar(mapping=aes(x=weekday, fill=member_casual )) + 
  facet_wrap(~month) + 
  labs(title="Ridership by Month") + 
  guides(fill=guide_legend(title="Membership Type")) + 
  scale_x_continuous("Weekday", labels=Abr_Day_of_Week, breaks = c(1:7)) +
  scale_y_continuous("Number of Total Rides", labels = comma) +
  theme(plot.title = element_text(hjust = 0.5))
#the following is the previous series of plots, but in the right order
ggplot(data = year) + 
  geom_bar(mapping=aes(x=weekday, fill=member_casual )) + 
  facet_wrap(~factor(month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"), labels = c('1' = "January", '2' = "February", '3' = "March", '4' = "April", '5' = "May", '6' = "June", '7' = "July", '8' = "August", '9' = "September", '10' = "October", '11' = "November", '12' = "December"))) + 
  labs(title="Ridership by Month") + 
  guides(fill=guide_legend(title="Membership Type")) + 
  scale_x_continuous("Weekday", labels=Abr_Day_of_Week, breaks = c(1:7)) +
  scale_y_continuous("Number of Total Rides", labels = comma) +
  theme(plot.title = element_text(hjust = 0.5))
#the following is the graphs above, but with 2 rows of plots instead of the default 3
ggplot(data = year) + 
  geom_bar(mapping=aes(x=weekday, fill=member_casual ), position = "dodge") + 
  facet_wrap(~factor(month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"), labels = c('1' = "January", '2' = "February", '3' = "March", '4' = "April", '5' = "May", '6' = "June", '7' = "July", '8' = "August", '9' = "September", '10' = "October", '11' = "November", '12' = "December")), nrow = 2) + 
  labs(title="Ridership by Month") + 
  guides(fill=guide_legend(title="Membership Type")) + 
  scale_x_continuous("Weekday", labels=Abr_Day_of_Week, breaks = c(1:7)) +
  scale_y_continuous("Number of Total Rides", labels = comma) +
  theme(plot.title = element_text(hjust = 0.5))
#the following is the graphs above, but with just one row
ggplot(data = year) + 
  geom_bar(mapping=aes(x=weekday, fill=member_casual ), position = "dodge") + 
  facet_wrap(~factor(month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"), labels = c('1' = "January", '2' = "February", '3' = "March", '4' = "April", '5' = "May", '6' = "June", '7' = "July", '8' = "August", '9' = "September", '10' = "October", '11' = "November", '12' = "December")), nrow = 1) + 
  labs(title="Ridership by Month") + 
  guides(fill=guide_legend(title="Membership Type")) + 
  scale_x_continuous("Weekday", labels=Abr_Day_of_Week, breaks = c(1:7)) +
  scale_y_continuous("Number of Total Rides", labels = comma) +
  theme(plot.title = element_text(hjust = 0.5))
#the following plots are of total bike rides, faceted by member type
ggplot(data = year) + 
  geom_histogram(mapping=aes(x=weekday, fill=as.character(weekday))) + 
  facet_wrap(~member_casual) + 
  scale_x_continuous("Weekday", labels=Abr_Day_of_Week, breaks = c(1:7)) + 
  scale_y_continuous("Total Bike Rides", labels = comma) + 
  labs(title="Total Bike Rides By Membership") +
  guides(fill="none") +
  theme(plot.title = element_text(hjust = 0.5))
  #guides(fill=guide_legend(title="Day of Week, Sunday = 1")) <- use this for having a legend, in place of the line two above
#the following is just total rides by day
ggplot(data = year) + 
  geom_histogram(mapping=aes(x=weekday), fill="darkorchid2") + 
  scale_x_continuous("Weekday", labels=Abr_Day_of_Week, breaks = c(1:7)) + 
  scale_y_continuous("Total Bike Rides", labels = comma) + 
  labs(title="Total Bike Rides By Day") +
  guides(fill="none") +
  theme(plot.title = element_text(hjust = 0.5))
#the following is just average ride length in seconds, no facets
ggplot(data=year) +
  stat_summary(
    mapping = aes(x = weekday, y = ride_length_seconds, fill= as.factor(weekday)),
    fun = mean,
    geom = "bar",
  ) + 
  scale_x_continuous("Weekday", labels=Abr_Day_of_Week, breaks = c(1:7)) + 
  scale_y_continuous("Average Ride Length in Seconds", labels = comma) + 
  scale_fill_brewer(palette = "Set2") +
  guides(fill="none") +
  labs(title = "Average Ride Length") +
  theme(plot.title = element_text(hjust = 0.5))
#the following graphed average ride length in minutes, no facets
ggplot(data=year, mapping = aes(x = weekday, y= (ride_length_seconds)/60, fill = as.factor(weekday))) +
  stat_summary(
    mapping = aes(x = weekday, y = (ride_length_seconds)/60, fill= as.factor(weekday)),
    fun = mean,
    geom = "bar",
  ) + 
  scale_x_continuous("Weekday", labels=Abr_Day_of_Week, breaks = c(1:7)) + 
  scale_y_continuous("Average Ride Length in Minutes", labels = comma) + 
  scale_fill_brewer(palette = "Set2") +
  guides(fill="none") +
  labs(title = "Average Ride Length") +
  theme(plot.title = element_text(hjust = 0.5))
#the following is total bike rides by day for just casual riders
ggplot(data = subset(year, member_casual=="casual"))+
  geom_histogram(mapping=aes(x=weekday)) +
  scale_x_continuous("Weekday", labels=Abr_Day_of_Week, breaks = c(1:7)) +
  scale_y_continuous("Total Bike Rides", labels = comma) + 
  labs(title="Total Rides By Day, Casual Riders") +
  guides(fill="none") +
  theme(plot.title = element_text(hjust = 0.5))
#the following is total bike rides by day but for annual members
ggplot(data = subset(year, member_casual=="member"))+ 
  geom_histogram(mapping=aes(x=weekday), fill="blue2") + 
  scale_x_continuous("Weekday", labels=Abr_Day_of_Week, breaks = c(1:7)) + 
  scale_y_continuous("Total Bike Rides", labels = comma) + 
  labs(title="Total Rides By Day, Annual Members") +
  guides(fill="none") +
  theme(plot.title = element_text(hjust = 0.5))