#--------------------Plot population pyramids for injury characteristics------------------------------------------------------------------------------------------------------

#Clear working space
rm(list=ls())

#Load libraries
library(dplyr)
library(ggplot2)
library("ggthemes") 

#load original dataset
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T)

#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, level of injury either cervical, thoracic, or lumbar,
#and AIS score A, B, C, or D
emsci.trauma.sex <- subset(emsci, (AgeAtDOI > 8) & (Sex=='f' | Sex=='m') & 
                                    (Cause=="ischemic" | Cause=="traumatic" | Cause=="haemorragic" |Cause=="disc herniation") & 
                                    (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar') & (YEARDOI >= 2000) &(AIS=='A' | AIS=="B"| AIS=="C"| AIS=="D"))

#Subset data to only patients with valid entry at stage 'very acute' or 'acute I' and remove duplicate patient numbers
emsci.trauma.sex.va.a1<-distinct(subset(emsci.trauma.sex, ExamStage=='acute I' | ExamStage=='very acute'), Patientennummer, .keep_all = TRUE)

#Caculate the percentage of each AIS grade per year
emsci.ais.plot = emsci.trauma.sex.va.a1 %>%
  count(YEARDOI,AIS,Sex) %>%
  group_by(YEARDOI, Sex)%>% mutate(frequency = (n / sum(n))*100)

#Plot the population pyramide
emsci.ais.plot <-ggplot(data = emsci.ais.plot, aes(x = YEARDOI, y = frequency, fill = AIS)) +
  geom_bar(data = emsci.ais.plot %>% filter(Sex == "f") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = emsci.ais.plot %>% filter(Sex == "m") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(2000, 2020), breaks = seq(2001, 2019, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of Patients [%]")+ ggtitle("Baseline Injury Severity")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
        )



emsci.nli.plot = emsci.trauma.sex.va.a1 %>%
  count(YEARDOI,NLI_level,Sex) %>%
  group_by(YEARDOI,Sex)%>%
 mutate(frequency = (n / sum(n))*100)

#Reorder levels
emsci.nli.plot$NLI_level <- factor(emsci.nli.plot$NLI_level, levels = c("cervical", "thoracic", "lumbar"))


emsci.nli.plot <- ggplot(data = emsci.nli.plot, aes(x = YEARDOI, y = frequency, fill = NLI_level)) +
  geom_bar(data = emsci.nli.plot %>% filter(Sex == "f") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = emsci.nli.plot %>% filter(Sex == "m") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 20), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(2000, 2020), breaks = seq(2001, 2019, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of Patients [%]")+ ggtitle("Baseline Injury level")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )


## Plutting the graphs together together
grid.arrange(emsci.ais.plot,
             emsci.nli.plot,
             widths=c(0.4,0.4),
             ncol=2
)




