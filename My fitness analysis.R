library('tidyverse')
library('data.table')
library('ggplot2') 
library('viridis')
library('scales')
library('waffle')
library('ggrepel')
library('plotly')
library('RColorBrewer')
library('datasauRus')
library('dplyr') 
library('readr') 
library('skimr') 
library('tibble') 
library('tidyr') 
library('purrr')
library('stringr') 
library('forcats')
library('janitor') 
library('Tmisc') 
library('class')
library('lubridate') 
library('forecast') 
library('timetk')
library('here')
library('gganimate')
library('XML')
library('gganimate')
library('ggpubr')
library('gifski')
library('gapminder')
library('timetk')

# Importing CSV file
daily_activity_metrics<-read.csv("C:/Shashi/Data & Dashboards/Datasets/daily_acitivity_metrics.csv")


head(daily_activity_metrics)

#Clean column names
my_activity_clean_names<-clean_names(daily_activity_metrics)

head(my_activity_clean_names)


#Remove N/A
my_activity_cleaned<-my_activity_clean_names%>%drop_na(step_count)


head(my_activity_cleaned)

# Mean Step Count
total_mean_steps<-my_activity_cleaned%>%group_by(date)%>%
  summarize(mean_total_steps=mean(step_count))

head(total_mean_steps)

# Activity level
activity_level<-total_mean_steps%>%
  mutate(daily_activity_level=case_when(
    mean_total_steps<2000 ~ "Very Low Activity",
    mean_total_steps>=2001 & mean_total_steps< 5000 ~ "Low Activity",
    mean_total_steps>=5001 & mean_total_steps<7500 ~ "Moderate Activity",
    mean_total_steps>=7501 & mean_total_steps<10000 ~ "High Activity",
    mean_total_steps>10001 ~ "Very High Activity"))

head(activity_level)

# Activity Percent
activity_percent<-activity_level%>%
  group_by(daily_activity_level)%>%
  summarise(total=n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(daily_activity_level) %>%
  summarise(total_percent = total / totals) %>%
  mutate(percent = scales::percent(total_percent))%>%
  arrange(desc(total_percent))

activity_percent$daily_activity_level <- factor(activity_percent$daily_activity_level, levels = c("Very Low Activity", "Low Activity", "Moderate Activity", "High Activity", "Very High Activity"))

head(activity_percent)

# Activity plot
ggplot(activity_percent,aes(x="",y = total_percent, fill=daily_activity_level)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  scale_fill_brewer(palette='YlGnBu')+
  theme_void()+ 
  theme(plot.title = element_text(hjust = 0.5,vjust= -5, size = 22, face = "bold")) +
  geom_text(aes(label = percent, x=1.2),position = position_stack(vjust = 0.5))+
  labs(title="Activity % Chart")+
  guides(fill = guide_legend(title = "Activity Level"))

# How my activity is distributed over the days of the week:
my_activity_cleaned$weekday <- weekdays(as.Date(my_activity_cleaned$date))

head(my_activity_cleaned)

weekday_activity<-my_activity_cleaned%>%drop_na(distance_m,calories_kcal)%>%group_by(weekday) %>% 
  summarize(mean_total_steps=mean(step_count), mean_total_distance=mean(distance_m), mean_calories=mean(calories_kcal))

head(weekday_activity)

weekday_activity$weekday <- ordered(weekday_activity$weekday,levels=c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday"))


# Weekday activity plots
options(repr.plot.width = 10, repr.plot.height = 5)
ggplot(data=weekday_activity)+
  geom_col(mapping = aes(x=weekday,y=mean_total_steps), fill="#33ccff")+
  theme(axis.text.x = element_text(angle = 30))+
  labs(title = "Weekly average steps", x="weekday", y="")+
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  labs(title = "Weekly average steps", x="weekday", y="Average Total Steps")


options(repr.plot.width = 10, repr.plot.height = 5)
ggplot(data=weekday_activity)+
  geom_col(mapping = aes(x=weekday,y=mean_calories), fill="#0066cc")+
  theme(axis.text.x = element_text(angle = 30))+
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  labs(title = "Weekly average calories", x="weekday", y="Calories")

# Mean steps & calories
mean_steps<-mean(as.numeric(my_activity_cleaned$step_count),na.rm=TRUE)

mean_calories<-mean(as.numeric(my_activity_cleaned$calories_kcal),na.rm=TRUE)

head(mean_steps)

head(mean_calories)

#Plot of steps vs calories
ggplot(my_activity_cleaned) + geom_point(mapping=aes(x=step_count, y=calories_kcal))+
  geom_smooth(mapping=aes(x=step_count, y=calories_kcal))

# Max heart rate
max_heart_rate <- my_activity_cleaned %>%
  select('date','max_heart_rate_bpm') %>%
  group_by(date) %>%
  summarize(max_heart_rate=mean(max_heart_rate_bpm)) 

max_heart_rate_cleaned<-na.omit(max_heart_rate)

head(max_heart_rate_cleaned)


max_heart_rate_cleaned$date <- as.Date(max_heart_rate_cleaned$date,"%Y-%m-%d")

library('transformr')

p1 <- ggplot(max_heart_rate_cleaned,aes(x=date, y=max_heart_rate, group=1)) + 
  geom_line(aes(colour=date))+
  ggtitle("Maximum heartrate over the Years") + transition_reveal(date)

p1

p1 <- p1+scale_x_date(date_labels = "%b/%Y")

p1+transition_reveal(date)

p1

# Max Heart rate vs Min Heart rate
heart_rate<-my_activity_cleaned %>% select(date, max_heart_rate_bpm, min_heart_rate_bpm, step_count) %>% 
  group_by(step_count)

tibble(heart_rate)

heart_rate_cleaned<-na.omit(heart_rate)


tibble(heart_rate_cleaned)

ggplot(heart_rate_cleaned) + 
  geom_line(mapping=aes(x=step_count, y=max_heart_rate_bpm, group=1), color="orange") +   
  labs(x="Number of steps", y="Max & Min Heart Rate", title= "<span style = 'color: #FF7400'> Maximum Heart Rate </span>") +
  theme(plot.title=element_markdown()) +
  geom_line(mapping=aes(x=step_count, y=min_heart_rate_bpm, group=1), color="red") + labs(subtitle= "<span style = 'color: #FF0000'> Minimum Heart Rate </span>") +
  theme(plot.subtitle=element_markdown())
