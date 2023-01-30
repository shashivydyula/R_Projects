world_internet_users<-read.csv("C:/Shashi/Data & Dashboards/Datasets/world_internet_user.csv")


#Structure of Data set
head(world_internet_users)
world_internet_users<-world_internet_users[,-6]
str(world_internet_users)
summary(world_internet_users)

#Checking if there are any N/A values in data set
sapply(world_internet_users, function(x) sum(is.na(x)))


#highest number of users
highest_users<-world_internet_users %>% arrange(desc(internet_users))
head(highest_users)



#Creating Raw World Map 
mapdata <- map_data("world")
head(mapdata)
head(world_internet_users)

#left join data by region
mapdata<-left_join(mapdata, world_internet_users, by='region')



#World Population map
popmap<-ggplot(mapdata, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill = population)) +
  ggtitle("World Population") 

ggplotly(popmap)

#Number of internet users
map1<-ggplot(mapdata, aes(x=long, y=lat, group = group, fill = internet_users)) + 
  geom_polygon(colour = "white") +
  scale_fill_gradient(low = "violet",
                      high = "red",
                      guide="colorbar") +
  theme_bw()  +
  labs(fill = "Internet Users" ,title = "World Internet Users", x="", y="") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank())

map1

#Bar plot (Region count)
bar1<-ggplot(world_internet_users) + 
  geom_bar(mapping = aes(x=reorder(continent,continent,function(x)-length(x)), fill=continent)) +
  labs(x="Continent", y="Count", title="Region Count")

bar1



#% of Users - world map
theme1 <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "grey85"),
  plot.title = element_text(hjust = 0.5)
)

map2<-ggplot(mapdata, aes(x=long, y=lat, group=group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = percentage_of_population)) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  ggtitle("World Internet Users") +
  theme1 
  

ggplotly(map2)

#Top Countries by number
top_countries_by_number<-world_internet_users %>%
  arrange(desc(internet_users)) %>% 
  slice(2:11) %>% #Removed _world
  plot_ly(x= ~internet_users, y= ~region, type = 'bar', orientation = 'h',
        marker = list(color = 'rgb(158,202,225)',
                      line = list(color = 'rgb(8,48,107)', width = 1.5)),
        text = ~paste0('*',internet_users), textposition = 'auto', hoverinfo = 'text') %>%
  layout(yaxis = list(categoryorder = "total ascending"),
         title = 'Top 10 Countries by Number of People using Internet')

top_countries_by_number



#Pair Plots
pairs(world_internet_users[,3:5], col='blue')

total_internet_users<-world_internet_users %>% select(internet_users)
total_population<-world_internet_users %>% select(population)
individual_percent<-world_internet_users %>% select(percentage_of_population)

pair_data<-data.frame(total_internet_users,total_population,individual_percent)

pairs(pair_data)
ggpairs(pair_data)


#Internet Usage Gap Analysis
## Calculating the the reach (how many users are not using internet)
unreachable_by_numbers<-world_internet_users %>% 
  mutate(unreachable_population_by_numbers=population-internet_users) 
head(unreachable_by_numbers)

#Replacing null values with NA and removing the NA row i.e., world
unreachable_by_numbers[unreachable_by_numbers<0] <-NA


na.omit(unreachable_by_numbers)



##Top unreachable countries
unreachable_plot<-unreachable_by_numbers %>%
  arrange(-unreachable_population_by_numbers) %>% 
  slice(2:10) %>% #remove world stats
  plot_ly(x= ~unreachable_population_by_numbers, y= ~region, type = 'bar', orientation = 'h',
          marker = list(color = 'rgb(204,204,204)',
                        line = list(color = 'rgb(8,48,107)', width = 1.5)),
          text = ~paste0('*',unreachable_population_by_numbers), textposition = 'auto', hoverinfo = 'text') %>%
  layout(yaxis = list(categoryorder = "total ascending"))
        


unreachable_plot<-unreachable_plot %>% layout(title = "Countries with highest number of users who don't use internet",
               barmode = 'group',
               xaxis = list(title = ""),
               yaxis = list(title = ""))

unreachable_plot

#Percentage of people not using internet
unreachable_by_percent<-world_internet_users %>% 
  mutate(unreachable_population_by_percent=100-percentage_of_population) 


#Unreachable by % of population
unreachable_by_percent<-world_internet_users %>% 
  mutate(unreachable_population_by_percent=100-percentage_of_population) 
head(unreachable_by_percent)

unreachable_by_percent[unreachable_by_percent<0] <-NA
unreachable_by_percent<-unreachable_by_percent[!is.na(unreachable_by_percent$unreachable_population_by_percent),]
head(unreachable_by_percent)

unreachable_by_percent_plot<-unreachable_by_percent %>%
  arrange(-unreachable_population_by_percent) %>% 
  slice(1:10) %>% #remove world stats
  plot_ly(x= ~unreachable_population_by_percent, y= ~region, type = 'bar', orientation = 'h',
          marker = list(color = 'rgb(255, 0 , 0)',
                        line = list(color = 'rgb(0, 0, 255)', width = 1.5)),
          text = ~paste0('*',unreachable_population_by_percent), textposition = 'auto', hoverinfo = 'text') %>%
  layout(yaxis = list(categoryorder = "total ascending"))



unreachable_by_percent_plot<-unreachable_by_percent_plot %>% layout(title = "Countries with highest percentage of users who don't use internet",
                                                                    barmode = 'group',
                                                                    xaxis = list(title = ""),
                                                                    yaxis = list(title = ""))

unreachable_by_percent_plot



internet_users_by_continent<-world_internet_users%>%
  select(continent, population, internet_users) %>% 
  group_by(continent)

head(internet_users_by_continent)

user_total<-internet_users_by_continent %>% 
  mutate(internet_percent=sum(internet_users)/sum(population)) %>% 
  group_by(continent)

head(user_total)


user_total_percent<-user_total %>% 
  mutate(internet_user_percent=internet_percent*100)
user_total_percent[user_total_percent<0] <-NA

user_total_percent<-na.omit(user_total_percent)
head(user_total_percent)



internet_users_continent_percent<-ggplot(user_total_percent, aes(fill=continent, y=internet_user_percent, x=reorder(continent, internet_user_percent))) + 
  geom_bar(position="dodge", stat="identity") +
  labs(x="Continent", y="Internet users %")

internet_users_continent_percent

ggplotly(internet_users_continent_percent)
