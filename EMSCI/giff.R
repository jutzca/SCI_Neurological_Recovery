##Emsci epidemiological data GIF
#Create 2.1.2020, Catherine Jutzeler
#Code source:
#https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/

#Clear working space
rm(list=ls())

#load libraries
library(ggplot2)
#install gganimate library via devtools
#install.packages('devtools')
#devtools::install_github('thomasp85/gganimate')
library(gganimate)

#set the theme to black and white
theme_set(theme_bw())

#load data
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/qry_CATHERINE_ISNCSCI_Age-17-12-2019.csv", sep = ',', header = T)

#filter data: Include all subjects that have baseline information on sex
emsci_withsex <- subset(emsci, Sex=='f' | Sex=='m')

#create static boxplots
p <- ggplot(
  emsci_withsex, 
  aes(x = emsci_withsex$Sex , y=as.numeric(emsci_withsex$AgeAtDOI))
) +
  geom_boxplot(show.legend = FALSE, alpha = 0.7) +
  scale_size(range = c(2, 12)) +
  labs(x = "GDP per capita", y = "Life expectancy")
p

#create the GIF including html link
p + transition_time(emsci_withsex$YEARDOI) +
  labs(title = "Year: {frame_time}")
+
  shadow_mark(alpha = 0.3, size = 0.5)

#facet by EMSCI Center
p + facet_grid(.~emsci_withsex$Center) +
  transition_time(emsci_withsex$YEARDOI) +
  labs(title = "Year: {frame_time}")





p <- ggplot(
  emsci_withsex,
  aes(emsci_withsex$ExamStage_weeks, as.numeric(LEMS), group = AIS, color = factor(emsci_withsex$AIS))
) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Weeks post injury", y = "LEMS") +
  theme(legend.position = "top")
p


 p + transition_reveal((emsci_withsex$ExamStage_weeks))+geom_point()

