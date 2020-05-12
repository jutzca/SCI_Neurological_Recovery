
rm(list=ls())

emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T)
emsci_incidence <- subset(emsci, ExamStage=='acute I' &  AgeAtDOI >= 0 & (!(is.na( Sex))))

library(dplyr)

value <-as.data.frame(emsci_incidence%>%
  count(YEARDOI))

library(easyGgplot2)

ggplot2.barplot(data=value, xName="YEARDOI", yName='n',
                width=0.9, color="black")+
  geom_text(aes(label=n), vjust=-0.12, color="black", size=3)+
  scale_y_continuous(expand = c(0.1,0)) +
  scale_x_discrete(breaks = c(2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019), expand=c(0,0) )+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  labs(fill = "", x = "Year of Injury", y = "Number of Patients")+ ggtitle("Annual incidence")+
  theme(axis.title = element_text(size = 13, face = 'bold'), 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
  )


  
  
 

