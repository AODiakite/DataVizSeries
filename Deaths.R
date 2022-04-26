library(readxl)
library(ggplot2)
AllAgesFemale <- read_excel("AllAgesFemale.xlsx") %>% 
  arrange(`Death rate per 100 000 population`) %>% tail(10) %>% mutate(Cause=factor(Cause,Cause))
fig1 = ggplot(data = AllAgesFemale,aes(x=Cause,y =`Death rate per 100 000 population`,fill =Cause))+
  geom_bar(stat="identity",show.legend = F,color = '#FFE4E3',size=.7)+
  coord_flip() +
  geom_text(aes(label=scales::comma(`Death rate per 100 000 population`)), hjust=0, nudge_y=-2,
            color = 'black',size = 3,fontface="bold") +
  labs(x="", y="Death rate per 100 000 population",
       title="Top 10 causes of death in Senegal for females aged all ages (2019)",
       subtitle="Data source : www.who.int")+
  hrbrthemes::theme_ipsum(grid = "X")
F20_24 <- read_excel("20-24Female.xlsx") %>% 
  arrange(`Death rate per 100 000 population`) %>% tail(10) %>% mutate(Cause=factor(Cause,Cause))
fig2 = ggplot(data = F20_24,aes(x=Cause,y =`Death rate per 100 000 population`,fill =Cause))+
  geom_bar(stat="identity",show.legend = F,color = '#FFE4E3',size=.7)+
  annotate(geom = 'curve',
           x="Sickle cell disorders and trait", y =  15,
           xend = "Interpersonal violence",
           color ='black',
           yend = 7.6, 
           curvature = 0.2, arrow = arrow(length = unit(2, "mm")))+
  annotate(geom = 'text',
           x="Sickle cell disorders and trait", y = 15,
           label = 'Interpersonal violence ranks fourth',
           color = 'black',
           size = 5,hjust = "center",vjust='top',fontface="bold") +
  coord_flip() +
  geom_text(aes(label=`Death rate per 100 000 population`), hjust=0, nudge_y=-2,
            color = 'black',size = 3,fontface="bold") +
  labs(x="", y="Death rate per 100 000 population",
       title="Top 10 causes of death in Senegal for females aged 20 to 24 years (2019)",
       subtitle="Data source : www.who.int",
       caption="Abdoul Oudouss Diakite")+

  hrbrthemes::theme_ipsum(grid = "X")
gridExtra::grid.arrange(fig1,fig2)
