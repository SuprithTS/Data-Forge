library(tidyverse)

list.files(path = "../input")

library(tidyverse)
library(readr)



dailyActivity_merged <- read_csv("C:/Users/DELL/Desktop/Software/CLG/R/R_activities/Activity-3 & 4/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
fem_activity <- dailyActivity_merged %>% filter(Id=="1503960366"|Id=="1624580081"|Id=="1844505072"|Id=="1927972279"|Id=="2026352035"|Id=="2320127002"|Id=="2347167796"|Id=="2873212765"|Id=="3372868164"|Id=="3977333714"|Id=="4020332650"|Id=="4057192912"|Id=="4319703577"|Id=="4445114986"|Id=="4558609924"|Id=="5553957443"|Id=="6117666160"|Id=="6775888955"|Id=="6962181067"|Id=="8253242879"|Id=="8792009665")
dailySteps_merged <- read_csv("C:/Users/DELL/Desktop/Software/CLG/R/R_activities/Activity-3 & 4/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
fem_steps <- dailySteps_merged %>% filter(Id=="1503960366"|Id=="1624580081"|Id=="1844505072"|Id=="1927972279"|Id=="2026352035"|Id=="2320127002"|Id=="2347167796"|Id=="2873212765"|Id=="3372868164"|Id=="3977333714"|Id=="4020332650"|Id=="4057192912"|Id=="4319703577"|Id=="4445114986"|Id=="4558609924"|Id=="5553957443"|Id=="6117666160"|Id=="6775888955"|Id=="6962181067"|Id=="8253242879"|Id=="8792009665")
dailyCalories_merged <- read_csv("C:/Users/DELL/Desktop/Software/CLG/R/R_activities/Activity-3 & 4/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
fem_Calories <- dailyCalories_merged %>% filter(Id=="1503960366"|Id=="1624580081"|Id=="1844505072"|Id=="1927972279"|Id=="2026352035"|Id=="2320127002"|Id=="2347167796"|Id=="2873212765"|Id=="3372868164"|Id=="3977333714"|Id=="4020332650"|Id=="4057192912"|Id=="4319703577"|Id=="4445114986"|Id=="4558609924"|Id=="5553957443"|Id=="6117666160"|Id=="6775888955"|Id=="6962181067"|Id=="8253242879"|Id=="8792009665")


fem_stepsVScalories <- merge(fem_Calories, fem_steps, by=c('Id', 'ActivityDay'))
steps_cals_1503960366 <- fem_stepsVScalories %>% filter(Id=="1503960366")


ggplot(steps_cals_1503960366, aes(x = ActivityDay, group=1)) + 
  geom_line(aes(y = Calories*6, color = 'Calories')) + 
  geom_line(aes(y = StepTotal, color = 'StepTotal'))

names(fem_activity) <- c("Id", "ActivityDay","TotalSteps","TotalDistance","TrackerDistance","LoggedActivitiesDistance","VeryActiveDistance","ModeratelyActiveDistance","LightActiveDistance","SedentaryActiveDistance","VeryActiveMinutes","FairlyActiveMinutes","LightlyActiveMinutes","SedentaryMinutes","Calories")

StepsCalsAct_1503960366 <- inner_join(fem_activity, steps_cals_1503960366, by=NULL, copy=FALSE)
ggplot(StepsCalsAct_1503960366, aes(x=ActivityDay, group=1))+
  geom_line(aes(y=Calories*6, color='Calories'))+
  geom_line(aes(y=StepTotal, color='StepTotal'))+
  geom_line(aes(y=VeryActiveMinutes*400, color='VeryActiveMinutes'))

StepsAct <- inner_join(fem_activity, fem_steps, by=NULL, copy=FALSE)
StepsCalsAct <- inner_join(StepsAct,fem_Calories,by=NULL,copy=FALSE)

ggplot(StepsCalsAct,aes(x=(VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes),y=Calories))+
  geom_point(aes(color=Id))

ggplot(StepsCalsAct,aes(x=(LightlyActiveMinutes),y=Calories))+
  geom_point(aes(color=Id))+
  geom_smooth(method = "lm")

ggplot(fem_activity, aes(ActivityDay, TotalSteps)) + geom_point() + geom_smooth(method = "lm")
