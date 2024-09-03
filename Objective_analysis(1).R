#Load the packages
# 15/941

library(tidyverse)
library(ggplot2)
library(dplyr)
library(janitor)
library(skimr)
library(hrbrthemes)
library(scales)
library(ggrepel)


#Loading of the dataset 
dailyActivity <- read.csv("C:/Users/DELL/Desktop/Software/CLG/R/R_activities/Activity-3 & 4/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
sleepData <- read.csv("C:/Users/DELL/Desktop/Software/CLG/R/R_activities/Activity-3 & 4/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weightInfo <- read.csv("C:/Users/DELL/Desktop/Software/CLG/R/R_activities/Activity-3 & 4/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv") 
heartRate_df <- read.csv("C:/Users/DELL/Desktop/Software/CLG/R/R_activities/Activity-3 & 4/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")


head(dailyActivity)
skim_without_charts(dailyActivity)


dailyActivity$ActivityDate <- as.Date(dailyActivity$ActivityDate, format = "%m/%d/%Y")
dailyActivity <- dailyActivity %>% 
  select(-LoggedActivitiesDistance)

#removing not needed data
#dailyActivity <- dailyActivity %>% 
# select(-LoggedActivitiesDistance)

dailyActivity %>% 
  filter(ActivityDate=="2016-04-13") %>% 
  summarise(unique_ids=n_distinct(Id)) %>% 
  pull(unique_ids) %>% 
  paste("No of users data for 2016-05-12: ", ., sep=" ")

dailyActivity %>% 
  filter(ActivityDate=="2016-04-12") %>% 
  summarise(unique_ids=n_distinct(Id)) %>% 
  pull(unique_ids) %>% 
  paste("No of users data for 2016-04-12: ", ., sep=" ")

dailyActivity %>% 
  filter(ActivityDate=="2016-05-12") %>% 
  select(Id, TotalSteps, TotalDistance, Calories) %>% 
  arrange(TotalSteps)

dailyActivity %>% 
  filter(ActivityDate=="2016-04-12") %>% 
  select(Id, TotalSteps, TotalDistance, Calories) %>% 
  arrange(TotalSteps)



#Filtering out data for date 2016-05-12 - since data is inconsistent. Adding Day of Week Column for better statistics.

dailyActivity <- 
  filter(dailyActivity, ActivityDate!="2016-05-12")

#adding DayOfWeek column
dailyActivity$DayOfWeek <- weekdays(dailyActivity$ActivityDate)

DayOfWeek_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

dailyActivity$DayOfWeek <- factor(dailyActivity$DayOfWeek, levels = DayOfWeek_order)

#Avg STeps walked by Users
dailyActivity %>% 
  mutate(Id = as.character(Id)) %>%
  group_by(Id) %>% 
  summarise(TotalSteps=mean(TotalSteps)) %>% 
  ggplot(aes(x=Id,y=TotalSteps,fill=Id)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  labs(x="", y="", title="Average Steps walked by Users") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#For some users the Average steps walked goes as up as more than 15K but also as less as 2K. To have a better fitness routine, an average of 7-10K steps should be walked everyday. Still there are few users who hardly hit even the 5K mark.

#Total and Average Steps Covered by Day of the Week
dailyActivity %>%
  group_by(DayOfWeek) %>%
  summarise(total_steps = sum(TotalSteps, na.rm = TRUE),
            avg_steps = mean(TotalSteps, na.rm = TRUE)) %>%
  pivot_longer(cols = c(total_steps, avg_steps),
               names_to = "variable",
               values_to = "steps") %>% 
  ggplot(aes(x = DayOfWeek, y = steps, color = variable, group = variable)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "Total and Average Steps Covered by Day of the Week",
       x = "",
       y = "Steps") +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  facet_wrap(~ variable, scales = "free_y", nrow = 2) +
  theme_minimal() +
  theme(legend.position = "none") + 
  scale_color_manual(values = c("total_steps" = "#00b38f", "avg_steps" = "#ff3377"))

#Sunday being the weekend, users do tend to be relaxing for the day and for weekdays it is more or less the same average of steps covered.

#Average Distance Walked by Day of the Week
dailyActivity %>%
  group_by(DayOfWeek) %>%
  summarise(total_dist = sum(TotalDistance, na.rm = TRUE),
            avg_dist = mean(TotalDistance, na.rm = TRUE)) %>%
  pivot_longer(cols = c(total_dist, avg_dist),
               names_to = "variable",
               values_to = "distance") %>% 
  ggplot(aes(x = DayOfWeek, y = distance, color = variable, group = variable)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "Total and Average Distance Covered by Day of the Week",
       x = "",
       y = "Distance in (km)") +
  facet_wrap(~ variable, scales = "free_y", nrow = 2) +
  theme_minimal() +
  theme(legend.position = "none") + 
  scale_color_manual(values = c("total_dist" = "#00b38f", "avg_dist" = "#ff3377"))

#The distance graph is very much similar with the steps plot and there isn't much of a difference

#Avg Steps walked by Date
dailyActivity %>% 
  group_by(ActivityDate) %>% 
  summarise(TotalSteps=mean(TotalSteps)) %>% 
  ggplot(aes(x=ActivityDate,y=TotalSteps,fill=ActivityDate)) +
  geom_area(fill = "lightblue", alpha=0.8) + 
  geom_line(colour = "black", linewidth = 0.8) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  labs(x="", y="", title="Average Steps covered for the period") +
  theme(axis.text.x = element_text(angle = 0), legend.position = "none")+
  theme_bw()

#Avg Dist walked by Date
dailyActivity %>% 
  group_by(ActivityDate) %>% 
  summarise(TotalDistance=mean(TotalDistance)) %>% 
  ggplot(aes(x=ActivityDate,y=TotalDistance,fill=ActivityDate)) +
  geom_area(fill = "#990099", alpha=0.5) + 
  geom_line(colour = "black", linewidth = 0.8) +
  labs(x="", y="Distance in (km)", title="Average Distance covered for the period") +
  theme(axis.text.x = element_text(angle = 0), legend.position = "none")+
  theme_bw()

#Distance vs Steps
dailyActivity %>% 
  ggplot(aes(y=TotalSteps, x=TotalDistance)) +
  geom_point() +
  geom_smooth(method='loess', formula='y~x') +
  labs(title = "Distance vs Steps Relation", x="Distance in (km)", y="Steps")

#In the graph of Distance and Steps it can be seen, the two variables share a straight line relationship with each other. Hence, they shared the similarities while plotting the previous graphs.

#Total Calories burnt by Users
dailyActivity %>% 
  mutate(Id = as.character(Id)) %>%
  group_by(Id) %>% 
  summarise(Calories=sum(Calories)) %>% 
  ggplot(aes(x=Id,y=Calories,fill=Id)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  labs(x="", y="", title="Total Calories burnt by Users") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Avg Calories burnt by Users
dailyActivity %>% 
  mutate(Id = as.character(Id)) %>%
  group_by(Id) %>% 
  summarise(Calories=mean(Calories)) %>% 
  ggplot(aes(x=Id,y=Calories,fill=Id)) +
  geom_col(show.legend = FALSE) +
  labs(x="", y="", title="Average Calories burnt by Users") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Total and Average Calories burnt by Day of the Week
dailyActivity %>%
  group_by(DayOfWeek) %>%
  summarise(total_calories = sum(Calories, na.rm = TRUE),
            avg_calories = mean(Calories, na.rm = TRUE)) %>%
  pivot_longer(cols = c(total_calories, avg_calories),
               names_to = "variable",
               values_to = "calories") %>% 
  ggplot(aes(x = DayOfWeek, y = calories, color = variable, group = variable)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "Total and Average Calories burnt by Day of the Week",
       x = "",
       y = "Calories") +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  facet_wrap(~ variable, scales = "free_y", nrow = 2) +
  theme_minimal() +
  theme(legend.position = "none") + 
  scale_color_manual(values = c("total_calories" = "#00b38f", "avg_calories" = "#ff3377"))

#Average Calorie Burnrate is more or less same, and drop can be seen only for Sunday.

#Avg Calories burnt by Date
dailyActivity %>% 
  group_by(ActivityDate) %>% 
  summarise(Calories=mean(Calories)) %>% 
  ggplot(aes(x=ActivityDate,y=Calories,fill=ActivityDate)) +
  geom_area(fill = "orange", alpha=0.6) + 
  geom_line(colour = "black", linewidth = 0.8) +
  geom_label(aes(label="Almost Constant Burn"),x=as.Date("2016-05-05"),y=1800, label.padding = unit(0.55, "lines"),
             label.size = 0.35,color = "black",fill="#69b3a2") +
  labs(x="", y="Calories", title="Average Calories burnt for the entire period") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5), legend.position = "none") +
  theme_bw()

#Steps vs Calories
dailyActivity %>% 
  ggplot(aes(x=TotalSteps,y=Calories)) +
  geom_point() + 
  geom_smooth(method = 'loess' , formula = 'y ~ x') +
  scale_x_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  labs(title="Steps walked vs Calories burnt Relationship", x="Steps")

#Distance vs Calories
dailyActivity %>% 
  ggplot(aes(x=TotalDistance,y=Calories)) +
  geom_point() + 
  geom_smooth(method = 'loess' , formula = 'y ~ x') +
  labs(title="Distance walked vs Calories burnt Relationship", x="Distance in (km)") +
  scale_x_continuous(breaks = seq(0,25,by=5))

# Avg time spent by minute category
data_long_avg <- 
  group_by(dailyActivity, ActivityDate) %>% 
  summarise(veryActive=mean(VeryActiveMinutes),fairlyActive=mean(FairlyActiveMinutes),
            lightlyActive=mean(LightlyActiveMinutes), stillMin=mean(SedentaryMinutes)) %>% 
  pivot_longer(cols = c(veryActive, fairlyActive, lightlyActive, stillMin), 
               names_to = "minutes_category", values_to = "minutes")

minutes_order <- c("veryActive","fairlyActive","lightlyActive","stillMin")

data_long_avg$minutes_category <- factor(data_long_avg$minutes_category, levels = minutes_order)

ggplot(data_long_avg, aes(x = ActivityDate, y = minutes/60, fill = minutes_category)) +
  geom_bar(stat="identity",alpha=0.7) +
  labs(title = "Avg time spent on Daily Activities", x = "", y = "Time spent (hrs)", fill="") +
  scale_fill_manual(values = c("veryActive" = "#a3297a", "fairlyActive" = "#00b300", 
                               "lightlyActive" = "#0047b3", "stillMin"="#ff6600"),
                    labels = c("veryActive" = "Very Active" , "fairlyActive"="Fairly Active",
                               "lightlyActive"="Lightly Active" , "stillMin" ="Still Time"))

#On an average,around 16-18 hours covers a users Sedentary time. In terms of being active, users engage in light activities the most in comparison with very and fairly activities.
