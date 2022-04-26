library(readr)
library(ggplot2)
library(viridis)
library(hrbrthemes)
StructuresSante <- read_csv("StructuresSante.csv", 
                            col_types = cols(...1 = col_skip()),show_col_types = F)
Region_Total =glue::glue("{StructuresSante$Region} : {StructuresSante$Total}")
StructuresSante$Region =Region_Total
StructuresSante=StructuresSante[-15,]
Region = rep(StructuresSante$Region,4)
Count =unlist(c(StructuresSante[,2:5]))
`Type de structure`=c(rep("Hôpital",14),rep("Centre de santé",14),
                    rep("Poste de santé",14),rep("Case de santé",14))
df = data.frame(Region,Count,`Type de structure`)


g1= ggplot(data = df,aes(x=Region,y=Count,group = `Type de structure`,
                         fill = `Type de structure`))+
  geom_bar(position="dodge", stat="identity",width=70)+
  scale_fill_brewer(palette = "Spectral")+
  geom_text(aes(label = Count,group = `Type de structure`),
            position=position_dodge2(width = 70,padding = 0.3),
            hjust=1,
            size = 3.7,color = '#330A01',fontface="bold.italic")+
  
  ggtitle("Inventaire des structures de santé au Sénégal (2019)") +
  labs(subtitle = 
       "Comme les phases précédentes, 
L’ECPSS 2019 est une enquête des structures de santé tant du secteur public que du secteur privé au Sénégal.
L’enquête a été menée dans toutes les structures de santé recensées (hôpitaux, centres de santé, et postes de santé),ainsi que les cases de santé liées aux postes de santé sélectionnés,
dans les 14 régions du pays. Les administrateurs et prestataires des services de santé de ces structures ont été interviewés ; 
les prestataires et les patients/clients venus en consultation pour des services de santé
spécifiques (consultation de l’enfant malade de moins de cinq ans et les consultations prénatales) 
ont été observés au cours des consultations et des interviews ont été menées avec des 
clients/ accompagnateurs d’enfants malades dont les consultations avaient été observées.",
caption = "Source : ANSD (ECPSS 2019)\n Crédit : Abdoul Oudouss DIAKITE")+
facet_wrap(~Region,nrow = 5)+
theme(plot.title  = element_text(color="#C70039", size=17, face="bold.italic"),
      plot.subtitle = element_text(color="black", size=10, face="italic"),
      axis.text.x  = element_blank(),
      axis.text.y  = element_blank(),
      panel.grid = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      panel.background = element_blank())+
  coord_flip()+
  xlab("") + ylab("")
g1
