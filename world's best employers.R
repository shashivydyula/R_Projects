best_employees<-read.csv("C:/Shashi/Data & Dashboards/Datasets/Worlds Best Employers.csv")

head(best_employees)
summary(best_employees)

#The employees column is formatted as character with commas in between. I will format it to numeric

best_employees$EMPLOYEES<-as.numeric(gsub(",","", best_employees$EMPLOYEES))
summary(best_employees)

#Top 10 Companies
top_10_companies<-best_employees %>% slice(1:10)

view(top_10_companies)

#Samsung Electronics ranks 1st in the world


#company with highest number of employees
highest_numer_of_employees<-
  best_employees[order(best_employees$EMPLOYEES, decreasing = TRUE),] %>% 
  slice(1:10)
view(highest_numer_of_employees)

highest_numer_of_employees$NAME  <- with(highest_numer_of_employees, reorder(NAME, EMPLOYEES))

highest_employees<-ggplot(highest_numer_of_employees, aes(fill=NAME, y=EMPLOYEES, x=reorder(NAME, -EMPLOYEES)))+
  geom_bar(position="dodge", stat="identity") +
             labs(x="Company", y="Number of Employees")+
  coord_flip()

highest_employees

#Amazon has the Highest number of employees

#Which industry has most companies in top 100
top_10_industry<-best_employees %>% count(INDUSTRIES)

top_10_industry<-
  top_10_industry[order(top_10_industry$n, decreasing = TRUE),] %>% 
  slice(1:10)
view(top_10_industry)

plot_ly(data=top_10_industry,labels=~INDUSTRIES, values=~n, type="pie") %>% 
  layout(title = 'Top 10 Industries',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


#IT, Internet, Software & services industry has highest companies (12) followed by Aerospace & Defense, Automotive, and Banking & Financial Services with 10 respective companies each in top 100.

#Which country has highest number of companies
country_with_highest_companies<-best_employees %>% 
  count(COUNTRY.TERRITORY)

country_with_highest_companies<-
  country_with_highest_companies[order(country_with_highest_companies$n, decreasing = TRUE),] %>% 
  slice(1:10)

view(country_with_highest_companies)

fig <- plot_ly(country_with_highest_companies, x = ~COUNTRY.TERRITORY, y = ~n, text = ~COUNTRY.TERRITORY, type = 'scatter', mode = 'markers',
               marker = list(size = ~n, opacity = 0.5, color = 'rgb(255, 65, 54)'))
fig <- fig %>% layout(title = 'Country with highest number of companies',
                      xaxis = list(showgrid = FALSE),
                      yaxis = list(showgrid = FALSE))
fig

#USA has 40 top 100 companies


#Company with least number of employees in Top 100
least_employees<-best_employees[order(best_employees$EMPLOYEES, decreasing = FALSE),] %>% 
  slice(1:10)
view(least_employees)

least_employees$NAME  <- 
  with(least_employees, reorder(NAME, EMPLOYEES))

least_employees_plot<-ggplot(least_employees, aes(fill=NAME, y=EMPLOYEES, x=reorder(NAME, -EMPLOYEES)))+
  geom_bar(position="dodge", stat="identity") +
  labs(x="Company", y="Number of Employees")
  
ggplotly(least_employees_plot)

#Mertiz Financial Group has least number of employees (20)

#Which industry has most employees in top 100
top_10_industry_employees<-best_employees %>% 
  count(INDUSTRIES, EMPLOYEES)

top_10_industry_employees<- top_10_industry_employees %>% count(INDUSTRIES, wt=EMPLOYEES)

top_10_industry_employees<-top_10_industry_employees[order(top_10_industry_employees$n, decreasing = TRUE),] %>% 
  slice(1:10)

view(top_10_industry_employees)

plot_ly(data=top_10_industry_employees,labels=~INDUSTRIES, values=~n, type="pie") %>% 
  layout(title = 'Top 10 Industries',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


