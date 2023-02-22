library(tidyverse)
library(scales)

#first diag en barres puis diagrammes circulaires pour part de chaque continent en 2000 et 2100
df <- read.csv("world-population-by-region-with-projections.csv")
df$Population = df$Population*1e-9

df_temp <- df %>%
  filter(Year %in% c("2000", "2100")) %>%
  filter(Entity %in% c("Africa", "Asia", "Europe", "South America", "North America", "Oceania")) %>%
  mutate(Entity = factor(Entity, unique(Entity)), Year = as.character(Year)) %>%
  group_by(Year) %>%
  arrange(desc(`Population`),.by_group = T)
df_temp$pct = c(100*df_temp$Population/sum(df_temp$Population))

inset_df <- df_temp %>%
  filter(Year == 2100)
  #filter(Entity %in% c("South America", "North America", "Oceania"))
inset_df$pct = c(100*inset_df$Population/sum(inset_df$Population))

inset_df <- inset_df %>%
  arrange(desc(pct)) %>%
  mutate(lab.ypos = cumsum(pct) - 0.5*pct)

mycols <- c("#F47A1F",
  "#FDBB2F",
  "#377B2B",
  "#7AC142",
  "#007CC3",
  "#00529B")

inset_plot = ggplot(df_temp, aes(x = Year, y = pct, fill = Entity)) +
  geom_col()+
  scale_x_discrete(limits = c(" ", "2000", "2100")) +
  #geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y")+
  #geom_text(aes(y = lab.ypos, label = pct), color = "white")+
  scale_fill_manual(values = mycols) +
  #scale_fill_viridis_d()+
  theme_void()


ggplot(data = df_temp, mapping = aes(x = Entity, y = `Population`, fill = Year))+
  geom_col(position = "dodge",width = 0.5) +
  scale_y_continuous(limits = c(0,max(df_temp$`Population`)+1))+
  geom_text(
    aes(label = comma(`Population`,accuracy = 0.01), fontface = "bold" ),
    position = position_dodge(width = 0.7),
    size = 4,
    hjust = 0.8,
    color = "black"
  ) +
  labs(
    title = "Repartition of the world population accross the continents and its evolution between the years 2000 and 2100",
    subtitle = "The population is in billion and the data for the year is the 2100 is the projection by the UN. For the piechart, the interior chart is for 2000 and the exterior one is for 2100\n\n",
    caption = "Source: https://ourworldindata.org/region-population-2100 \nCaption : https://github.com/AODiakite/DataVizSeries"
  )+
  coord_flip(clip = "off")+
  theme_void()+
  theme(
    axis.text.y = element_text(family = "Times",
                               face = "bold",
                               size = 10,hjust = 1,
                               color ="#212F3C"),
    legend.position = c(.1, -0.2),
    legend.direction = "horizontal",
    legend.title  = element_blank(),
    legend.margin = margin(b = 0, unit = "cm"),
    plot.margin = margin(t = 0.7,b=0.7, unit = "cm"),
    title = element_text(family = "Times",
                         face = "bold",
                         size = 15,
                         colour = "#212F3C"
    ),

    legend.key.size  = unit(0.3, "cm"),
    legend.text = element_text(
      size = 12,
      color = "#212F3C",
      hjust = -5,
      face = "bold"),
    plot.caption = element_text(size = 8, hjust = .9),
    plot.subtitle = element_text(size = 8)
    )+

  inset_element(
    inset_plot,
    left = 1,
    bottom = 0.1 ,
    right = 0.8,
    top = 1.2,
  )


