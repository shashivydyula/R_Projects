dogs<-read.csv("C:/Shashi/Data & Dashboards/Datasets/dog_breeds.csv")
str(dogs)
head(dogs)

dogs<-dogs %>% mutate(Common.Health.Problems=tolower(Common.Health.Problems))

dogs<-separate(dogs, Common.Health.Problems, into=c('common_problem1', 'common_problem2', 'common_problem3'), sep=',')

head(dogs)


#Remove unwanted spaces
dogs$common_problem1<-trimws(dogs$common_problem1, which=c('both'))
dogs$common_problem2<-trimws(dogs$common_problem2, which=c('both'))
dogs$common_problem3<-trimws(dogs$common_problem3, which=c('both'))
head(dogs)

#most common problem
most_common_problem1<-dogs %>% 
  group_by(common_problem1) %>% 
  summarise(total_count=n(),
            .groups='drop')%>% 
  arrange(desc(total_count))%>% as.data.frame()


most_common_problem2<-dogs %>% 
  group_by(common_problem2) %>% 
  summarise(total_count=n(),
            .groups='drop') %>% 
  arrange(desc(total_count))%>% as.data.frame()



most_common_problem3<-dogs %>% 
  group_by(common_problem3) %>% 
  summarise(total_count=n(),
            .groups='drop') %>% 
  arrange(desc(total_count))%>% as.data.frame()

#Renaming problems column to problem for joining
most_common_problem1<-most_common_problem1 %>% rename('problem'='common_problem1')
most_common_problem2<-most_common_problem2 %>% rename('problem'='common_problem2')
most_common_problem3<-most_common_problem3 %>% rename('problem'='common_problem3')



common_problems<-full_join(most_common_problem1, most_common_problem2, by='problem') %>% 
  full_join(.,most_common_problem3, by='problem')

#Replacing NA with zero values
common_problems[is.na(common_problems)]<-0

common_problems$problem_count<-common_problems$total_count.x+common_problems$total_count.y+common_problems$total_count

head(common_problems)


common_problems<-common_problems %>% select(problem, problem_count) %>% 
  arrange(desc(problem_count))
head(common_problems)

top_10_problems<-common_problems %>% slice(1:10)
head(top_10_problems)

top_10_problems$problem  <- with(top_10_problems, reorder(problem, problem_count))
top_10_problems_plot<-ggplot(top_10_problems, aes(label=problem_count, fill=problem, y=problem_count, x=reorder(problem, desc(problem_count))))+
  geom_bar(stat='identity')+
  geom_text(size = 3, position = position_stack(vjust=0.5))+
  labs(x="Top 10 Disease", y=" ")+
  coord_flip()
top_10_problems_plot

#Which country has most dog breeds
country<-dogs %>% 
  group_by(Country.of.Origin) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  arrange(desc(total_count))
head(country)


country %>%
  arrange(desc(total_count)) %>% 
  slice(1:10) %>% 
  plot_ly(x= ~total_count, y= ~Country.of.Origin, type = 'bar', orientation = 'h',
          marker = list(color = 'rgb(158,202,225)',
                        line = list(color = 'rgb(8,48,107)', width = 1.5)),
          text = ~paste0('*',total_count), textposition = 'auto', hoverinfo = 'text') %>%
  layout(yaxis = list(categoryorder = "total ascending"),
         title = 'Top 10 Countries with most Breeds')

#Common fur color
fur_color<-dogs %>% select(Breed, Fur.Color)
head(fur_color)
fur_color<-fur_color %>% mutate(Fur.Color=tolower(Fur.Color))

fur_color<-separate(fur_color, Fur.Color, into=c("color1", "color2", "color3", "color4"), sep=',')

head(fur_color)

fur_color$color1<-trimws(fur_color$color1, which=c('both'))
fur_color$color2<-trimws(fur_color$color2, which=c('both'))
fur_color$color3<-trimws(fur_color$color3, which=c('both'))
fur_color$color4<-trimws(fur_color$color4, which=c('both'))

fur_color[is.na(fur_color)]<-""

most_common_color1<-fur_color %>% 
  group_by(color1) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  arrange(desc(total_count))
head(most_common_color1)

most_common_color2<-fur_color %>% 
  group_by(color2) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  arrange(desc(total_count))
head(most_common_color2)

most_common_color3<-fur_color %>% 
  group_by(color3) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  arrange(desc(total_count))
head(most_common_color3)

most_common_color4<-fur_color %>% 
  group_by(color4) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  arrange(desc(total_count))
head(most_common_color4)

most_common_color1<-most_common_color1 %>% rename("color"="color1")
most_common_color2<-most_common_color2 %>% rename("color"="color2")
most_common_color3<-most_common_color3 %>% rename("color"="color3")

common_colors<-full_join(most_common_color1, most_common_color2, by='color') %>% 
  full_join(.,most_common_color3, by='color')

head(common_colors)


common_colors[is.na(common_colors)]<-0

common_colors$color_count<-common_colors$total_count.x+common_colors$total_count.y+common_colors$total_count

common_colors<-common_colors %>% select(color, color_count) %>% 
  arrange(desc(color_count)) %>% 
  slice(2:11) #removes first blank row count and select top
head(common_colors)

common_colors_plot<-
  common_colors %>%
  arrange(desc(color_count)) %>% 
  slice(1:10) %>% 
  plot_ly(x= ~color_count, y= ~color, type = 'bar', orientation = 'h',
          marker = list(color = 'rgb(	50, 171, 96)',
                        line = list(color = 'rgb(	120,0,128)', width = 1.5)),
          text = ~paste0('*',color_count), textposition = 'auto', hoverinfo = 'text') %>%
  layout(yaxis = list(categoryorder = "total ascending"),
         title = 'Top 10 Colors')
common_colors_plot

#Age vs Height
dogs$mean_Height..in. <- sapply(strsplit(as.character(dogs$Height..in.) , "-", fixed = TRUE), function(x) sum(as.numeric(x))) 
dogs$mean_Height..in. <- dogs$mean_Height..in. / 2 
dogs$mean_Longevity..yrs. <- sapply(strsplit(as.character(dogs$Longevity..yrs.) , "-", fixed = TRUE), function(x) sum(as.numeric(x))) 
dogs$mean_Longevity..yrs. <- dogs$mean_Longevity..yrs. / 2 

plot(dogs$mean_Longevity..yrs., dogs$mean_Height..in.)+
  text(dogs$mean_Longevity..yrs., dogs$mean_Height..in.,dogs$Breed)+
  dev.new(width=45, height=15, unit="in")

height_vs_age_plot<-ggplot(dogs, aes(fill=Breed, y=mean_Height..in., x=mean_Longevity..yrs.))+
  geom_bar(position = 'dodge', stat='identity')+
  labs(x="Longevity in Years", y="Height in inches")+
  theme(legend.position="none")

height_vs_age_plot

#Mean age 
mean_age <- ggplot(dogs, aes(x=mean_Longevity..yrs.))
mean_age + geom_area(stat = "bin")
mean_age + geom_area(stat = "bin", fill = "lightblue")+
  geom_vline(aes(xintercept=mean(mean_Longevity..yrs.)),
             color="blue", linetype="dashed", size=1)+
  labs(x="Mean Age of Dogs", y="")

#Correlating mean age with top-5 countries with most breeds
p<-ggplot(dogs[dogs$Country.of.Origin %in% c("England","Germany","Australia", "France", "Scotland"),], aes(x=mean_Longevity..yrs., fill=Country.of.Origin))+
  geom_area(stat ="bin")+facet_grid(Country.of.Origin ~ .)+
  labs(x="Mean Age of Dogs", y="")
p

#joining age vs height with mean age
ggplot(dogs[dogs$Country.of.Origin %in% c("England","Germany","Australia", "France", "Scotland"),], aes(x=mean_Longevity..yrs., y=mean_Height..in.)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2) +
  geom_point(size=3, color="#69b3a2") +
  theme_ipsum() +
  ggtitle("Age-Height")+
  labs(x="Mean Longevity", y="Mean height")+
  geom_vline(aes(xintercept=mean(mean_Longevity..yrs.)),
             color="black", linetype="dashed", size=1)
