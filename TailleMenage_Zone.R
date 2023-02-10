library(tidyverse)
library(scales)
df_pop <- read.csv("ObservationData_jkcnnuf.csv") 

# Data cleaning-------
df_pop <- df_pop %>% 
  filter(Date >= 2000) %>% 
  select(-Unit) %>% 
  pivot_wider(names_from = indicateurs, values_from = Value) 

df_temp <- df_pop %>% 
  filter(Date == 2018, regions != "SENEGAL")%>% 
  filter(zones %in% c("Urban","Rual")) %>% 
  group_by(zones) %>% 
  arrange(desc(`Taille des ménages`),.by_group = T)%>% 
  mutate(regions = factor(regions, unique(regions)))


ggplot(data = df_temp, mapping = aes(x = regions, y = `Taille des ménages`, fill = zones))+
  geom_col(position = "dodge",width = 0.7)+
  scale_y_continuous(limits = c(0,max(df_temp$`Taille des ménages`)+1))+
  geom_text(
    aes(label = comma(`Taille des ménages`,accuracy = 0.1), fontface = "bold" ),
    position = position_dodge(width = 0.7),
    size = 4,
    hjust = 1.3,
    color = "white"
    )+
  geom_hline(
    aes(yintercept = 0),
    size = 0.15
    )+
  labs(
    title = "Taille des ménages par zones",
    subtitle ="Données publiées dans la Banque de Données des Indicateurs Sociaux (BADIS, 2018)\n\n",
    caption = "Source : Agence nationale de la Statistique et de la Démographie - ANSD, Sénégal
    L’ENSEMBLE DE DONNÉES ORIGINAL http://www.ansd.sn"
    )+
  scale_fill_manual(values = c("#154360", "#7FB3D5"))+
  coord_flip(clip = "off")+
  theme_void()+
  theme(
    axis.text.y = element_text(family = "Times",
                               face = "bold",
                               size = 12,hjust = 1,
                               color ="#212F3C"),
    legend.position = c(.85, .95),
    legend.direction = "horizontal",
    legend.title  = element_blank(),
    legend.margin = margin(b = 1, unit = "cm"),
    plot.margin = margin(t = 1,b=1, unit = "cm"),
    title = element_text(family = "Times", 
                         face = "bold",
                         size = 20,
                         colour = "#212F3C"
                         ),
    legend.key.size  = unit(0.5, "cm"),
    legend.text = element_text(
      size = 12,
      color = "#212F3C",
      hjust = -5,
      face = "bold"),
    plot.caption = element_text(size = 8, hjust = .9),
    plot.subtitle = element_text(size = 8)
  )
