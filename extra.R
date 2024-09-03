# Load necessary libraries
library(tidyverse)
library(skimr)

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



