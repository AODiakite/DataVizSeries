library(dplyr)
library(ggplot2)
air_df <- read.csv("air-pollution.csv") %>% 
  filter(Entity %in% c("Africa","China","India","United States"), Year == 2019) %>% 
  relocate(Entity ,Year) %>% 
  .[,-(2:9)] %>% 
  `names<-`(c("Entity","Nitrogen oxide (NOx)",
              "Sulphur dioxide (SO2)","Carbon monoxide (CO)",
              "Organic carbon (OC)","NMVOCs",
              "Black carbon (BC)","Ammonia (NH3)")) %>% 
  reshape2::melt(id = "Entity") %>% 
  mutate_if(is.character,as.factor) %>% 
  arrange(variable,desc(value)) %>% 
  mutate(value = round(value,2)) %>% 
  mutate(label = value) %>% 
  mutate(value = as.factor(value))

ggplot(data = air_df, aes(x=variable,y= value,fill = Entity))+
  geom_col(position = "dodge",width = .85)+
  scale_fill_manual(values = c("#041C32","#064663","#ECB365","#B2AB8C"))+
  geom_text(aes(label=scales::comma(label,accuracy = 0.01)), vjust=0.5,hjust = 1,
            color = '#FFFFF6',size = 4,fontface="bold",
            position = position_dodge2(width = 0.85,padding = 0.5)) +
  coord_flip()+
  labs(title = "Air Pollution (2019)",
       subtitle = "This graph shows, for comparison, the amount per capita (kg) of air pollutants emitted by Africa, China, India and the United States.",
       caption = "Source : Our World in Data         Credi : Abdoul Oudouss Diakité")+
  hrbrthemes::theme_ipsum(grid = "")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(family = "serif",face = "bold",size = 30,colour  = "#21325E"),
        plot.subtitle = element_text(family = "serif",face = "bold",colour  = "#21325E")
        )
