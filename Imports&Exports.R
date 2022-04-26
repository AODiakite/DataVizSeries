library(ggplot2)
library(magrittr)

#Load data
Data.SEN <- read_csv("SenegalIndicators.csv", 
                     skip = 4,show_col_types = F) %>% .[,-length(.)]

data1=as.data.frame(t(Data.SEN[Data.SEN$`Indicator Code`=='TX.VAL.MRCH.CD.WT',-(1:4)]))
data1$Date=rownames(data1)
names(data1)[1]=c('Exports')
data=as.data.frame(t(Data.SEN[Data.SEN$`Indicator Code`=='TM.VAL.MRCH.CD.WT',-(1:4)]))
data$Date=rownames(data)
names(data)[1]=c('Imports')
data=merge(data1,data,by = 'Date')
data = na.omit(data)


ggplot(data = data,aes(x= as.numeric(Date))) +
  geom_col(aes(y=Imports),fill = "#FF5733",size = 50) +
  geom_col(aes(y=Exports),fill  ="#0A7104",size =50,width = 0.7) +
  labs(title = 'Senegal',subtitle = 'Imports & Exports (current US$)')+
  #Imports arrow
  annotate(geom = 'curve',
           x=1970, y = 2e+09,
           xend = 1980,
           color ='#622700',
           yend = data$Imports[data$Date=='1980'], 
           curvature = 0.2, arrow = arrow(length = unit(4, "mm")))+
  #Imports text
  annotate(geom = 'text',
           x=1970, y = 2.25e+09,
           label = 'Imports',
           color = '#FF5733',
           size = 5,hjust = "center",vjust='top',fontface="bold") +
  #Exports arrow
  annotate(geom = 'curve',
           x=1987, y = 2e+09,
           xend = 1987,
           color ='#043701',
           yend = data$Exports[data$Date=='1987'], 
           curvature = 0, arrow = arrow(length = unit(4, "mm")))+
  #Exports text
  annotate(geom = 'text',
           x=1987, y = 2.25e+09,
           label = 'Exports',
           color = '#0A7104',
           size = 5,hjust = "center",fontface="bold") +
  #2012's Exports arrow
  annotate(geom = 'curve',
           x=1993, y = 4e+09,
           xend = 2012,
           color ='#043701',
           yend = data$Exports[data$Date=='2012'], 
           curvature = 0.4, arrow = arrow(length = unit(4, "mm")))+
  #2012's Imports arrow
  annotate(geom = 'curve',
           x=1993, y = 4e+09,
           xend = 2012,
           color ='#622700',
           yend = data$Imports[data$Date=='2012'], 
           curvature = -0.4, arrow = arrow(length = unit(4, "mm")))+
  #2012's label
  annotate(geom = "label", x = 1993, 
           y = 4e+09, 
           label = paste0("Beginning of Macky Sall's presidential term",
                          "\n",
                          "Date : 2012","\n",
                          "Imports : ",data$Imports[data$Date=='2012'],
                          "\n",
                          "Exports : ",data$Exports[data$Date=='2012'],
                          "\n"),
           size = 3.1,hjust = "center",fill='#EBC894',fontface="bold")+
  #Max Exports arrow
  annotate(geom = 'curve',
           x=1970, y = 6e+09,
           xend =as.numeric(data$Date[which.max(data$Exports)]),
           color ='#89F400',
           yend = data$Exports[which.max(data$Exports)], 
           curvature = -0.35, arrow = arrow(length = unit(4, "mm")))+
  #Max Imports arrow
  annotate(geom = 'curve',
           x=1970, y = 6e+09,
           xend = as.numeric(data$Date[which.max(data$Imports)]),
           color ='#F40400',
           yend = data$Imports[which.max(data$Imports)], 
           curvature = -0.16, arrow = arrow(length = unit(4, "mm")))+
  #Max's label
  annotate(geom = "label", x = 1970,
           y = 6e+09,
           label = paste0("Maximum Exports&Imports",
                          "\n",
                          "Date :",data$Date[which.max(data$Imports)],"\n",
                          "Imports : ",data$Imports[which.max(data$Imports)],
                          "\n",
                          "Exports : ",data$Exports[which.max(data$Exports)],
                          "\n"),
           size = 3.1,hjust = "center",fill='#E5DC94',fontface="bold")+
  ggthemes::theme_wsj()
