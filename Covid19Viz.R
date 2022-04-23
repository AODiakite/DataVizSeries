# Covid 19 ( Senegal )
# Load Covid Data
library(dplyr)
library(readr)
SEN.covid<-  read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv",
                      show_col_types = FALSE) %>% 
  .[.$Country=='Senegal',] %>% filter(Date_reported >= '2020-03-02')
#ploting
library(ggplot2)
library(patchwork)
p1= ggplot(SEN.covid,aes(x=Date_reported)) +
  geom_col(aes(y = New_cases),color="darkblue") +
  annotate(geom = 'col', 
           x=SEN.covid[which.max(SEN.covid$New_cases),]$Date_reported,
           y=max(SEN.covid$New_cases),colour ="red",size =1) + 
  annotate(geom = 'curve',
           x=lubridate::ymd('2020-07-28'), y = max(SEN.covid$New_cases)-500,
           xend = SEN.covid[which.max(SEN.covid$New_cases)-2,]$Date_reported,
           color ='#BB6203',
           yend = max(SEN.covid$New_cases), 
           curvature = -0.4, arrow = arrow(length = unit(4, "mm"))) +
  annotate(geom = "label", x = lubridate::ymd('2020-07-28'), 
           y = max(SEN.covid$New_cases)-600, 
           label = paste0("Maximum number of cases : ",max(SEN.covid$New_cases),
                          "\n","Date : ",
                          SEN.covid[which.max(SEN.covid$New_cases)-1,]$Date_reported,
                          "\n"),
           size = 3.1,hjust = "center",fill='#FED5AA',fontface="bold")+
  labs(title = 'Senegal',subtitle = 'Daily covid19 cases',
       x='Date',y='New cases')+
  ggthemes::theme_tufte() 

p2 = ggplot(SEN.covid,aes(x=Date_reported)) +
  geom_col(aes(y = New_deaths),color="#3F3E46") +
  annotate(geom = 'col', 
           x=SEN.covid[which.max(SEN.covid$New_deaths),]$Date_reported,
           y=max(SEN.covid$New_deaths),colour ="#FF023F",size =1) +
  annotate(geom = 'curve',
           x=lubridate::ymd('2020-07-05'), y = max(SEN.covid$New_deaths)-10,
           xend = SEN.covid[which.max(SEN.covid$New_deaths)-2,]$Date_reported,
           color ='#041087',
           yend = max(SEN.covid$New_deaths), 
           curvature = -0.4, arrow = arrow(length = unit(4, "mm"))) +
  annotate(geom = "label", x = lubridate::ymd('2020-07-07'), 
           y = max(SEN.covid$New_deaths)-12, 
           label = paste0("Maximum number of deaths : ",max(SEN.covid$New_deaths),
                          "\n","Date : ",
                          SEN.covid[which.max(SEN.covid$New_deaths)-1,]$Date_reported,
                          "\n"),
           fill='#B2B3D2', size = 3,hjust = "center",fontface="bold")+
  labs(title = 'Senegal',subtitle = 'Daily deaths related to covid19',
       x='Date',y='New deaths')+
  ggthemes::theme_tufte()