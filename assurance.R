library(tidyverse)
library(scales)
library(stringi)
library(patchwork)
library(readxl)


assurance_df <- read_excel("assurance.xlsx")

                                                
palette_couleur <- colorRampPalette(c("#000000", "#1A5276", "#2471A3",
                                               "#2980B9", "#3498DB", "#5DADE2",
                                               "#85C1E9", "#AED6F1", "#D6EAF8"))(19)


                      

assurance_df <- assurance_df %>% 
  arrange(Montant2021) %>% 
  mutate(
    `Compagniesd'assurances` = str_wrap(`Compagniesd'assurances`,10),
    `Compagniesd'assurances` = factor(`Compagniesd'assurances`,
                                           `Compagniesd'assurances`),
         Pct_Part = percent(Part),
    palette_couleurs = rev(palette_couleur) 
         ) %>% 
  relocate(Pct_Part,.after = Part)

inset_df <- assurance_df %>% 
  filter(Montant2021 < 3500) %>% 
  arrange(desc(Montant2021)) %>% 
  mutate(`Compagniesd'assurances` = factor(`Compagniesd'assurances`,
                                           `Compagniesd'assurances`))
inset_plot = ggplot(
  inset_df,
  aes(
    x = `Compagniesd'assurances`,
    y = Montant2021,
    fill = palette_couleurs
  )
)+
  geom_col(show.legend = FALSE,width = .7)+
  geom_text(
    aes(label = Montant2021, fontface = "bold" ),
    position = position_dodge(width = 0.7),
    size = 4,
    vjust = -.3,
    color = "black"
  )+
  scale_y_continuous(
    limits = c(0,4000),
    breaks = seq(0,3000,1000) )+
  geom_hline(yintercept = 0, linewidth = 0.3)+
  scale_fill_manual(values =  inset_df$palette_couleurs)+
  theme_void()+
  theme(
    axis.text.x = element_text(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = "black", linewidth =1, fill = "#FBEEE6"),
  )

ggplot(
  assurance_df,
  aes(
    x = `Compagniesd'assurances`,
    y = Montant2021,
    fill = palette_couleurs
    )
  )+
  geom_col(show.legend = FALSE)+
  scale_y_continuous(
    limits = c(0,max(assurance_df$Montant2021)+10000),
    breaks = seq(0,20000,5000) )+
  geom_hline(yintercept = 0, linewidth = 0.3)+
  geom_text(
    aes(label = Pct_Part, fontface = "bold" ),
    position = position_dodge(width = 0.7),
    size = 4,
    hjust = -.2,
    color = "black"
  )+
  scale_fill_manual(values =  rev(assurance_df$palette_couleurs))+
  annotate(geom = 'curve',
           x= 6.5, y = 4500,
           xend = "Maas",
           color ='#622700',
           yend = 5000,
           curvature = -0.4,
           linetype = "longdash",
           linewidth = .5
  )+
  annotate(geom = 'curve',
           x= "Cnaas", y = 5700,
           xend = "Sonac",
           color ='#622700',
           yend = 16000,
           curvature = -0.1
  )+
  labs(
    title = "Assureurs non vie au Sénégal:\nRépartition du chiffre d'affaires par compagnie en 2021 (En millions de FCFA)",
    subtitle = "Le pourcentage en label représente la part de la compagnie dans le marché d'assurance non vie au Sénégal\n\n",
    caption = "Source des données : www.dna.finances.gouv.sn \nCaption : https://github.com/Codantou"
  )+
  ylab("Montant")+
  coord_flip(clip = "off") +
  theme_void()+
  theme(
    axis.text = element_text(face = "bold", size = 8),
    axis.title.x = element_text(hjust = .4, size = 12,vjust = -9),
    plot.margin = margin(b=1, unit = "cm"),
    title = element_text(family = "Times", 
                         face = "bold",
                         size = 15,
                         colour = "#212F3C",
    ),
    plot.caption = element_text(size = 8, hjust = .9),
    plot.subtitle = element_text(size = 10),
    panel.grid.major.x= element_line(colour = "#D5D8DC")
  )+
  inset_element(
    inset_plot,
    left = .5,
    bottom = 0.1 ,
    right = .8,
    top = .5,
    )

