covid<-fread("C:/Shashi/Data & Dashboards/Datasets/owid-covid-data.csv")
str(covid)

# renaming location to name for mapping
colnames(covid)[3]<-"name"

#Renaming incorrect names of countries
covid$name[covid$name=="United States"]<- "United States of America"
covid$name[covid$name=="Democratic Republic of Congo"]<- "Democratic Republic of the Congo"
covid$name[covid$name=="Tanzania"]<- "United Republic of Tanzania"
covid$name[covid$name=="Congo"]<- "Republic of Congo"
covid$name[covid$name=="Cote d'Ivoire"]<- "Ivory Coast"
covid$name[covid$name=="Sierra Leone"]<- "Sierra Leone"
covid$name[covid$name=="Guinea-Bissau"]<- "Guinea Bissau"
covid$name[covid$name=="Rwanda"]<- "Rwanda"
covid$name[covid$name=="Burundi"]<- "Burundi"


# Total Cases (selecting only countries)
total_cases<-covid %>%
  select(name, date,total_cases) %>% 
  filter(name!='World',name!='Asia',name!='High income', 
         name!='Europe', name!='European Union', name!='Upper middle income',
         name!='North America', name!='Africa', name!='South America',
         name!='Lower middle income')

total_cases_world<-total_cases %>% 
  filter(date==max(date))

highchart() %>%
  hc_add_series_map(worldgeojson, df = total_cases_world, value = "total_cases", joinBy = "name") %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_bloom()) %>% 
  hc_colorAxis(minColor = "lightblue", maxColor="mediumblue") %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text = "World Total Cases", style = list(fontSize = "25px")) %>%
  hc_subtitle(text="Total Covid Cases: 1.18 Billion", style=list(fontsize = "15px")) 

top_10_total_cases<-total_cases_world %>% 
  arrange(desc(total_cases)) %>% 
  slice(1:10)

highchart() %>% 
  hc_chart(type="column", options3d=list(enabled=TRUE, alpha=15, beta=15)) %>% 
  hc_xAxis(categories=top_10_total_cases$name) %>% 
  hc_add_series(data=top_10_total_cases$total_cases, name="Top 10 Countries") %>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_title(text="Top 10 Countries With Most cases(total)")


# Total deaths
total_deaths<-covid %>% 
  select(name, date,total_deaths) %>% 
  filter(name!='World',name!='Asia',name!='High income', 
         name!='Europe', name!='European Union', name!='Upper middle income',
         name!='North America', name!='Africa', name!='South America',
         name!='Lower middle income')



total_deaths_world<-total_deaths %>% 
  filter(date==max(date))

highchart() %>% 
  hc_add_series_map(worldgeojson, df=total_deaths_world, value = "total_deaths", joinBy = "name") %>% 
  hc_legend(enable=TRUE) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_colorAxis(minColor = "orange", maxColor = "red") %>% 
  hc_title(text = "Total Deaths Worldwide", style=list(fontsize="25px")) %>% 
  hc_subtitle(text = "16.8 million deaths") %>% 
  hc_mapNavigation(enabled=TRUE)
  
  top_10_total_deaths<-total_deaths_world %>% 
    arrange(desc(total_deaths)) %>% 
    slice(1:10)

  highchart() %>% 
    hc_chart(type="column", options3d=list(enabled=TRUE, alpha=15, beta=15)) %>% 
    hc_xAxis(categories=top_10_total_deaths$name) %>% 
    hc_add_series(data=top_10_total_deaths$total_deaths, name="Top 10 Countries with most deaths") %>% 
    hc_add_theme(hc_theme_google()) %>% 
    hc_title(text="Top 10 Countries With Most Covid Deaths")
  


#  continent wise cases and deaths
  
  continents<-covid %>% 
    filter(continent=="") %>% 
    filter(date==max(date)) %>% 
    filter(name!="European Union") %>% 
    filter(name!="High income") %>% 
    filter(name!="International") %>% 
    filter(name!="Low income") %>% 
    filter(name!="Lower middle income") %>% 
    filter(name!="Upper middle income") %>% 
    filter(name!="World")
  
  view(continents)
  
  continents<-continents %>% 
    select(name, total_cases, total_deaths, population)
  
  continents_percent<-continents %>% 
    mutate(cases_percentage=paste0(round(total_cases*100/sum(total_cases),digits=2), " ",'%')) %>% 
    mutate(death_percentage=paste0(round(total_deaths*100/sum(total_deaths), digits = 2), '%')) %>% 
    mutate(population_percentage=paste0(round(population*100/sum(population), digits=2), '%'))
  
  
ggplot(continents_percent, mapping=aes(x=name, y=total_cases, fill=cases_percentage))+
    geom_col()+
    facet_wrap(.~reorder(name,desc(total_cases)))+
    labs(title="Percentage of Covid Cases",x="Contient", y="Total Cases")+
    scale_y_continuous(labels = scales::label_number_si())+
    guides(fill=guide_legend(title="Covid Case Percentage"))+
    theme(axis.text.x=element_blank())

ggplot(continents_percent, mapping=aes(x=reorder(name,desc(total_deaths)), y=total_deaths, fill=reorder(death_percentage,desc(death_percentage))))+
  geom_col()+
  facet_wrap(.~reorder(name,desc(total_deaths)))+
  labs(title="Percentage of Covid Cases",x="Contient", y="Total Deaths")+
  scale_y_continuous(labels = scales::label_number_si())+
  guides(fill=guide_legend(title="Covid Death Percentage"))+
  theme(axis.text.x=element_blank())

# Continent wise plots

covid_dates<-covid
covid_dates$Posixdate <- as.POSIXct(covid$date, format="%Y-%m-%d")

continental<-covid_dates %>% 
  filter(iso_code=="OWID_AFR") %>% 
  filter(iso_code=="OWID_ASI") %>% 
  filter(iso_code=="OWID_EUR") %>% 
  filter(iso_code=="OWID_OCE") %>% 
  filter(iso_code=="OWID_NAM") %>% 
  filter(iso_code=="OWID_SAM")

continental<-filter(covid_dates,iso_code%in%c("OWID_AFR","OWID_ASI","OWID_EUR",
                                              "OWID_OCE","OWID_NAM","OWID_SAM"))
view(continental)

# Total cases animated plot
cases<-ggplot(data = continental, aes(x=Posixdate,y=total_cases, colour=name))+
  geom_line(size=1.2)+
  xlab("Year")+
  ylab("Total Number of Cases")+
  scale_colour_discrete(name="Continent")+
  scale_y_continuous(labels = scales::label_number_si())+
  transition_reveal(Posixdate)+
  transition_reveal(Posixdate)+
  anim_save("Cases.gif", cases)

# static plot             
ggplot(data=continental, aes(x=Posixdate, y=total_cases, color=name))+
  geom_line(size=1.5)+
  xlab("Year")+
  ylab("Total Number of Cases")+
  scale_color_discrete(name="Continent")+
  scale_y_continuous(labels = scales::label_number_si())+
  facet_wrap(~name)

# Total deaths animated plot
Deaths<-ggplot(data = continental, aes(x=Posixdate,y=total_deaths, colour=name))+
  geom_line(size=1.2)+
  xlab("Year")+
  ylab("Total Number of Deaths")+
  scale_colour_discrete(name="Continent")+
  scale_y_continuous(labels = scales::label_number_si())+
  transition_reveal(Posixdate)+
  anim_save("Deaths.gif", Deaths)

# static plot
ggplot(data=continental, aes(x=Posixdate, y=total_deaths, color=name))+
  geom_line(size=1.5)+ xlab("Year")+
  ylab("Total Number of Deaths")+
  scale_color_discrete(name="Continent")+
  scale_y_continuous(labels = scales::label_number_si())+
  facet_wrap(~name)


# Countries with Most Cases and Deaths per Million
cases_deaths_per_million<-covid %>% 
  select(name, date, total_cases_per_million,total_deaths_per_million)


cases_deaths_per_million<-cases_deaths_per_million %>% 
  filter(date==max(date))

view(cases_deaths_per_million)

cases_per_million<-cases_deaths_per_million %>% 
  arrange(desc(total_cases_per_million)) %>% 
  slice(1:10)

deaths_per_million<-cases_deaths_per_million %>% 
  arrange(desc(total_deaths_per_million)) %>% 
  slice(1:10)



highchart() %>% 
  hc_chart(type="bar") %>% 
  hc_xAxis(categories=cases_per_million$name) %>% 
  hc_add_series(data=cases_per_million$total_cases_per_million, name="Cases per Million") %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_title(text="Top 10 Countries With Most Covid Cases Per Million Population")

highchart() %>% 
  hc_chart(type="bar") %>% 
  hc_xAxis(categories=deaths_per_million$name) %>% 
  hc_add_series(data=deaths_per_million$total_deaths_per_million, name="Cases per Million") %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_title(text="Top 10 Countries With Most Deaths Per Million Population")


# Vaccination Percentage
vaccination<-covid %>% 
  select(name, date, people_fully_vaccinated, population)

vaccination<-vaccination %>% 
  filter(name!='World',name!='Asia',name!='High income', 
         name!='Europe', name!='European Union', name!='Upper middle income',
         name!='North America', name!='Africa', name!='South America',
         name!='Lower middle income')


vaccination<-vaccination %>%
  group_by(name, people_fully_vaccinated ,population)

vaccination<-na.omit(vaccination)

vaccination<-vaccination %>% 
  group_by(name) %>% 
  filter(date==max(date))

vaccination_percentage<-vaccination %>% 
  mutate(vacc_perc=paste0(round(people_fully_vaccinated*100/population, digits=2)," ",'%'))

top_vacc_percent<-vaccination_percentage %>% 
  arrange(desc(vacc_perc))

view(top_vacc_percent)

top_10_vacc_percent<-vaccination_percentage %>%
  group_by(name)%>% 
  summarise(vaccn_percent = max(vacc_perc, na.rm=TRUE)) %>% 
  arrange(desc(vaccn_percent)) %>% 
  slice(1:10)



ggplot(top_10_vacc_percent, mapping=aes(fill=name, x=reorder(name,desc(vaccn_percent)), y=vaccn_percent))+
  geom_col(position='identity')+
  labs(title="Top 10 Countries with Highest % of Covid Vaccination",
       x="Country", y="Percentage")+
  guides(fill=guide_legend(title=""))+
  coord_flip()

# Death Percentage
deaths<-covid %>% 
  select(name, date, total_deaths, population)

view(deaths)

deaths<-na.omit(deaths)

deaths<-deaths %>% 
  filter(date==max(date))

deaths_percent<-deaths %>% 
  mutate(death_perc=paste0(round(total_deaths*100/population, digits=2)," ", '%')) %>% 
  arrange(desc(death_perc)) %>% 
  slice(1:10)

view(deaths_percent)


ggplot(deaths_percent, mapping=aes(fill=name, x=reorder(name,desc(death_perc)), y=death_perc))+
  geom_col(position='identity')+
  labs(title="Top 10 Countries with Highest % of Covid Deaths",
       x="Country", y="Percentage")+
  guides(fill=guide_legend(title=""))

