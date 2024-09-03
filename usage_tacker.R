#Sleep Data Analysis


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


head(sleepData)
glimpse(sleepData)
colnames(sleepData)

#Converting the datatypes. Adding Day of Week column.

#convert to char
sleepData$Id <- as.character(sleepData$Id)

#converted to Date format
sleepData$SleepDay <- as.POSIXct(sleepData$SleepDay, format = "%m/%d/%Y %I:%M:%S %p")

#adding DayOfWeek column
sleepData$DayOfWeek <- weekdays(sleepData$SleepDay)

DayOfWeek_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

sleepData$DayOfWeek <- factor(sleepData$DayOfWeek, levels = DayOfWeek_order)
skim_without_charts(sleepData)

#Average sleep 

sleepData %>% 
  group_by(Id) %>% 
  summarise(TotalMinutesAsleep=mean(TotalMinutesAsleep)) %>% 
  ggplot(aes(x=Id,y=TotalMinutesAsleep/60, fill=Id))+
  geom_col()+
  scale_y_continuous(breaks = seq(0,10,by=2))+
  labs(title = "Average sleep ", y="Hours",x="")+
  theme_bw()+
  theme(legend.position = "none", axis.text.x = element_text(angle=90))

#More than half of the users have a good amount of sleep, but at the same time we can also see couple of users with only 1 to 2 hours of sleep and few with 5 to 6 hours. Given that a person should sleep for atleast 7 to 8 hours, this is very low. This could probably be the case of insufficient data, we will see the exact reason in the upcoming part.

sleepData %>% 
  group_by(DayOfWeek) %>% 
  summarise(TotalMinutesAsleep=mean(TotalMinutesAsleep)) %>% 
  ggplot(aes(x=DayOfWeek,y=TotalMinutesAsleep/60, fill=DayOfWeek))+
  geom_col()+
  scale_y_continuous(breaks = seq(0,8,by=1))+
  labs(title = "Average sleep by Day", y="Hours",x="")+
  theme_bw()+
  theme(legend.position = "none")

sleepData %>% 
  group_by(SleepDay) %>% 
  summarise(TotalMinutesAsleep=mean(TotalMinutesAsleep)) %>% 
  ggplot(aes(x=SleepDay,y=TotalMinutesAsleep/60, fill=SleepDay))+
  geom_col()+
  scale_y_continuous(breaks = seq(0,8,by=1))+
  labs(title = "Average sleep ", y="Hours",x="")+
  theme_bw()+
  theme(legend.position = "none")


#On a daily basis, the average sleep goes from 6 to 7.5 hours.
properSleep_ids <- sleepData %>% 
  group_by(Id) %>% 
  summarise(TotalMinutesAsleep=mean(TotalMinutesAsleep)) %>% 
  filter((TotalMinutesAsleep/60)>=7 & (TotalMinutesAsleep/60)<=8.5) %>% 
  select(Id)

filtered_sleepData2 <- sleepData %>%
  filter(Id %in% properSleep_ids$Id)


ggplot(filtered_sleepData2, aes(x=SleepDay,y=TotalMinutesAsleep/60, fill=Id))+
  geom_col()+
  labs(title = "Sufficient sleep Distribution ", y="Hours",x="",
       subtitle = "Sleep distribution for Users having average sleep between 7 to 8.5 hours")+
  facet_wrap(~Id)+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 90), legend.position = "none")

# Sleep records
sleepData %>%
  filter(TotalSleepRecords>=2) %>% 
  ggplot(aes(x=Id))+
  geom_bar(fill="#80ff00", color="black")+
  labs(title="Sleep Records Trend", x="", y="Sleep record count",
       subtitle = "How many users slept more than once in a day for the test duration?")+
  scale_y_continuous(breaks = seq(0,10,by=2))+
  theme(axis.text.x = element_text(angle=90), legend.position = "none")







#weight analysis 

head(weightInfo)
skim_without_charts(weightInfo)

#keeping only the needed data
weightInfo <- weightInfo %>% 
  select(-WeightPounds,-LogId, -Fat)

#datatype format
weightInfo$Id <- as.character(weightInfo$Id)
weightInfo$Date <- as.POSIXct(weightInfo$Date, format = "%m/%d/%Y %I:%M:%S %p")

#not enough data for weight only 8 users
unique(weightInfo$Id)
n_unique(weightInfo$Id)

#get summary
grouped_weight <- weightInfo %>% 
  group_by(Id) %>% 
  summarise(avg_weight=mean(WeightKg), avg_bmi=mean(BMI))

#User vs Weight
grouped_weight %>% 
  ggplot(aes(x=Id,y=avg_weight,group=1)) +
  geom_point()+
  geom_line(linetype="dotted")+
  geom_label(aes(label=round(avg_weight,2)),
             color = ifelse(grouped_weight$avg_weight == max(grouped_weight$avg_weight), "#ff0066", 
                            ifelse(grouped_weight$avg_weight == min(grouped_weight$avg_weight), "#ff9900", "black"))
  )+
  labs(x="ID", y="Weight (Kg)", title = "Users and Weight Range")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90))

#Users are in the range of 52kg to 133 kg.

# Plot based on BMI range
grouped_weight %>%
  mutate(bmi_range = case_when(
    avg_bmi < 18 ~ "Underweight",
    avg_bmi >= 18 & avg_bmi <= 25 ~ "Normal",
    avg_bmi > 25 ~ "Overweight"
  )) %>% 
  mutate(bmi_range = factor(bmi_range, levels = c("Underweight","Normal", "Overweight"))) %>%
  ggplot(aes(x = Id, y = avg_bmi, fill = bmi_range)) +
  geom_col(alpha = 0.75) +
  geom_text(aes(label = round(avg_bmi, 2)), vjust = -0.5, color = "black", size = 4) +
  scale_fill_manual(values = c("blue", "green", "red"),
                    labels = c("Underweight", "Normal", "Overweight"), drop=FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = c(0.9, 0.9), legend.title = element_blank()
  ) +
  labs(title = "BMI Distribution by User", x = "", y = "")



# Linear Regression
lm_model <- lm(avg_weight ~ avg_bmi, data = grouped_weight)

# Summary of the regression model
summary(lm_model)





#heart rate data analysis 




# Read data (assuming weightInfo is already loaded)

# Display data summary without charts
skim_without_charts(weightInfo)

# Keep only the needed data
weightInfo <- weightInfo %>% 
  select(-WeightPounds, -LogId, -Fat)

# Convert data types
weightInfo$Id <- as.character(weightInfo$Id)
weightInfo$Date <- as.POSIXct(weightInfo$Date, format = "%m/%d/%Y %I:%M:%S %p")

# Check unique users and count
unique_users <- unique(weightInfo$Id)
n_users <- n_distinct(weightInfo$Id)

# Get summary statistics
summary_stats <- weightInfo %>%
  group_by(Id) %>%
  summarise(avg_weight = mean(WeightKg),
            avg_bmi = mean(BMI))

# Regression analysis to understand the relationship between weight and BMI
weight_bmi_regression <- lm(avg_weight ~ avg_bmi, data = summary_stats)

# Display regression summary
summary_stats_regression <- summary(weight_bmi_regression)

# Plot regression line
ggplot(summary_stats, aes(x = avg_bmi, y = avg_weight)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +
  labs(x = "Average BMI", y = "Average Weight (Kg)", title = "Regression: BMI vs Weight") +
  geom_text(x = 20, y = 120,
            label = paste("R-squared:", round(summary_stats_regression$r.squared, 3)),
            hjust = 0, vjust = 0, color = "blue", size = 4)

# Plot BMI distribution by user
summary_stats <- summary_stats %>%
  mutate(bmi_range = case_when(
    avg_bmi < 18 ~ "Underweight",
    avg_bmi >= 18 & avg_bmi <= 25 ~ "Normal",
    avg_bmi > 25 ~ "Overweight"
  )) %>%
  mutate(bmi_range = factor(bmi_range, levels = c("Underweight", "Normal", "Overweight")))

# Plot BMI distribution
ggplot(summary_stats, aes(x = Id, y = avg_bmi, fill = bmi_range)) +
  geom_col(alpha = 0.75) +
  geom_text(aes(label = round(avg_bmi, 2)), vjust = -0.5, color = "black", size = 4) +
  scale_fill_manual(values = c("blue", "green", "red"),
                    labels = c("Underweight", "Normal", "Overweight"), drop = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = c(0.9, 0.9), legend.title = element_blank()) +
  labs(title = "BMI Distribution by User", x = "", y = "Average BMI") +
  geom_hline(yintercept = c(18, 25), linetype = "dashed", color = "grey") +
  annotate("text", x = n_users + 0.5, y = 20, label = "Normal BMI", color = "black", size = 4, hjust = 0) +
  annotate("text", x = n_users + 0.5, y = 27, label = "Overweight", color = "black", size = 4, hjust = 0) +
  annotate("text", x = n_users + 0.5, y = 15, label = "Underweight", color = "black", size = 4, hjust = 0)


#heart rate info 

glimpse(heartRate_df)
skim_without_charts(heartRate_df)

heartRate_df$Time <- as.POSIXct(heartRate_df$Time, format = "%m/%d/%Y %I:%M:%S %p")
heartRate_df$Id <- as.character(heartRate_df$Id)
summary(heartRate_df$Value)

#There are no missing values and the data is consistent as well. The data is tracked for 14 users.

heartRate_df$Time <- as.POSIXct(heartRate_df$Time, format = "%m/%d/%Y %I:%M:%S %p")
heartRate_df$Id <- as.character(heartRate_df$Id)
summary(heartRate_df$Value)


heartRate_df %>% 
  group_by(Id) %>% 
  summarise(Max=max(Value), Min=min(Value), Avg=mean(Value)) %>% 
  pivot_longer(cols = c(Avg, Min, Max), names_to = "stats_value", values_to = "heart_rate") %>% 
  arrange(desc(Id)) %>% 
  ggplot(aes(x=Id, y=heart_rate, color=stats_value, group=stats_value))+
  geom_line()+
  geom_point()+
  geom_label(aes(label=round(heart_rate,1)))+
  geom_text(aes(label="Max"),x="8877689391", y=190, color="#cc3399")+
  geom_text(aes(label="Avg"),x="8877689391", y=93, color="#009900")+
  geom_text(aes(label="Min"),x="8877689391", y=55, color="#0066cc")+
  labs(title = "User vs Heart Rate ",
       subtitle = "Depicts the Maximum, Minimum and Average Heart Rate of each User",
       x = "",
       y = "", fill=" ") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_color_manual(values = c("Max" = "#cc3399", "Min" = "#0066cc", "Avg" = "#009900"))+
  theme(legend.position = "none")