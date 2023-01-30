alcolohol_consumption<-read.csv("C:/Shashi/Data & Dashboards/Datasets/World Alcoholic Consumption datasets.csv")

view(alcolohol_consumption)
summary(alcolohol_consumption)

#Date is formatted as chr. Convert to appropriate date formate using lubridate
alcolohol_consumption$date <- lubridate::as_date(mdy(alcolohol_consumption$Date))


str(alcolohol_consumption)



#Gender count
gender_count<-alcolohol_consumption %>% count(Gender)
gender_count


#Country count
country_count<-alcolohol_consumption %>% 
  count(Countries)
view(country_count)

#Top alcoholic countries
top_countries<-country_count[order(country_count$n, decreasing = TRUE),]
view(top_countries)
top_10_countries<-top_countries %>% slice(1:10)

top_10_countries$Countries  <- with(top_10_countries, reorder(Countries, n))
top_10_countries_plot<-ggplot(top_10_countries,aes(label=n,fill=Countries, y=n, x=reorder(Countries, desc(n))))+
  geom_bar(stat="identity")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))
top_10_countries_plot

#Male and Female count
male_female_count<-alcolohol_consumption %>% 
  group_by(Countries, Gender) %>% 
  summarise(total_count=n(),.groups = 'drop')

view(male_female_count)

#Top alcoholic countries with male and female count
top_10_male_female_count<-
  male_female_count[order(male_female_count$total_count, decreasing = TRUE),] %>% 
  slice(1:19)

head(top_10_male_female_count)

top_10_male_female_count$Countries<-with(top_10_male_female_count,reorder(Countries,total_count))

top_10_m_f_plot<-
  ggplot(top_10_male_female_count, aes(label=total_count, fill=Gender, y=total_count,x=Countries))+
  geom_bar(stat="identity")+
  geom_text(size=3, position=position_stack(vjust = 0.5))+
  labs(X="Country", y="Gender Count")+
  coord_flip()
top_10_m_f_plot

#Which year has highest consumption
##Before calculating the year with highest consumption we need to separate year from Date
alcolohol_consumption$year <- lubridate::year(alcolohol_consumption$date)


#let us separate month and date as well
alcolohol_consumption$month <- lubridate::month(alcolohol_consumption$date) #separates month number
alcolohol_consumption$month_name<-month.name[alcolohol_consumption$month] #gives name to the month by its number

view(alcolohol_consumption)

#top year (total)
top_year_total<-alcolohol_consumption %>% 
  group_by(year, Gender) %>% 
  summarise(total_count=n(),.groups = 'drop')

view(top_year_total)

top_year_total_plot<-ggplot(top_year_total, aes(label=total_count, fill=Gender, y=total_count, x=year))+
  geom_bar(stat="identity")+
  geom_text(size=4, position=position_stack(vjust = 0.5))+
  labs(x="Year", y="Count by year and Gender")

top_year_total_plot

#which month has highest consumption
top_month_total<-alcolohol_consumption %>% 
  group_by(month_name, Gender) %>% 
  summarise(total_count=n(),.groups = 'drop')
view(top_month_total)


top_month_total_plot<-ggplot(top_month_total, aes(label=total_count, fill=Gender, y=total_count, x=reorder(month_name, total_count)))+
  geom_bar(stat="identity")+
  geom_text(size=3, position=position_stack(vjust=0.25))+
  facet_grid(~Gender)+
  labs(x="Month in Descending Order of alcohol Consumption", y="Total Count")+
  coord_flip()

top_month_total_plot

#Which month women consumed most
women_only <- top_month_total[ which(top_month_total$Gender=='Female'), ]

view(women_only)

women_only_plot<-ggplot(women_only, aes(label=total_count, fill=month_name, y=total_count, x=reorder(month_name, total_count)))+
  geom_bar(stat="identity")+
  geom_text(size=3, position=position_stack(vjust=0.25))+
  facet_grid(~Gender)+
  labs(x="Month in Descending Order of alcohol Consumption", y="Total Count")+
  coord_flip()+
  theme(legend.position = "none")

women_only_plot

#which month men consumed most
men_only<-top_month_total[which(top_month_total$Gender=='Male'),]
view(men_only)

men_only_plot<-ggplot(men_only, aes(label=total_count, fill=month_name, y=total_count, x=reorder(month_name, total_count)))+
  geom_bar(stat="identity")+
  geom_text(size=3, position=position_stack(vjust=0.25))+
  facet_grid(~Gender)+
  labs(x="Month in Descending Order of alcohol Consumption", y="Total Count")+
  coord_flip()+
  theme(legend.position = "none")

men_only_plot
