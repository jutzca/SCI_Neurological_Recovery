#--------------------Plot population pyramids for injury characteristics------------------------------------------------------------------------------------------------------

#Clear workspace
rm(list = ls())

#where libraries are stored
.libPaths()

#paths
outdir_figures='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen/Figures'
outdir_tables='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen/Tables'

#Load libraries
library(dplyr)
library(ggplot2)
library("ggthemes") 
library(ggpubr)
library(ggridges)
library(gridExtra)
library(sjPlot)
library(tidyr)

#load original dataset
sygen<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/Sygen.csv", sep = ',', header = T,  na.strings=c("","NA"))

#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
sygen.included.cohort<- subset(sygen, (age > 0) & (sex=="female" | sex=="male") & ###Age at DOI and Sex
                                 (splvl1 == 'C' | splvl1 == 'T')&   ## Neurological level
                                 (ais1=="AIS A"| ais1=="AIS B"| ais1=="AIS C"| ais1=="AIS D")) #AIS Grades

#------Assess whether the proportion of injury severities (AIS A, B, C, D) changed over time and display in table----

#Calculate ratios of AIS grades per year
ais_ratios.sygen = sygen.included.cohort %>%
  count(ais1, yeardoi) %>%
  group_by(yeardoi)%>%
  mutate(frequency = (n / sum(n))*100)


#Fit regression model with the proportion of AIS grade as the response, and time as the predictor
ais_ratios.sygen.lm<-ais_ratios.sygen %>%
  group_by(ais1) %>%         
  do(tidy(lm(frequency~yeardoi, data=.))%>%
       mutate(
         'CI low' = estimate - std.error,
         'CI high' = estimate + std.error
       )
  ) 
ais_ratios.sygen.lm

#----Plot the population pyramide 'Baseline Injury Severity' - OVERALL----
#Caculate the percentage of each AIS grade per year
sygen.ais.proportions = sygen.included.cohort %>%
  count(yeardoi,ais1,sex) %>%
  group_by(yeardoi,sex)%>% mutate(frequency = (n / sum(n))*100)

#Plot
sygen.ais.plot <-ggplot(data = sygen.ais.proportions, aes(x = yeardoi, y = frequency, fill = ais1)) +
  geom_bar(data = sygen.ais.proportions %>% filter(sex == "female") %>% arrange(rev(yeardoi)),
           stat = "identity")+
  geom_bar(data = sygen.ais.proportions %>% filter(sex == "male") %>% arrange(rev(yeardoi)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(1991, 1998), breaks = seq(1992, 1997, 1), expand = c(0,0))+ 
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

sygen.ais.plot


ggsave(
  "ais.distribitution_overall.pdf",
  plot = sygen.ais.plot,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()



#----Plot the population pyramide 'Baseline Injury Severity' - Tetraplegic----

#Caculate the percentage of each AIS grade per year
sygen.ais.proportions.tetra = subset(sygen.included.cohort, plegia=="tetra") %>%
  count(yeardoi,ais1,sex) %>%
  group_by(yeardoi,sex)%>% mutate(frequency = (n / sum(n))*100)


sygen.ais.plot.tetra <-ggplot(data = sygen.ais.proportions.tetra, aes(x = yeardoi, y = frequency, fill = ais1)) +
  geom_bar(data = sygen.ais.proportions.tetra %>% filter(sex == "female") %>% arrange(rev(yeardoi)),
           stat = "identity")+
  geom_bar(data = sygen.ais.proportions.tetra %>% filter(sex == "male") %>% arrange(rev(yeardoi)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(1991, 1998), breaks = seq(1992, 1997, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of Tetrapleic Patients [%]")+ ggtitle("Baseline Injury Severity")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )

sygen.ais.plot.tetra


ggsave(
  "ais.distribitution_tetra.pdf",
  plot = sygen.ais.plot.tetra,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()


#----Plot the population pyramide 'Baseline Injury Severity' - Paraplegic----

#Caculate the percentage of each AIS grade per year
sygen.ais.proportions.para = subset(sygen.included.cohort, plegia=="para") %>%
  count(yeardoi,ais1,sex) %>%
  group_by(yeardoi,sex)%>% mutate(frequency = (n / sum(n))*100)


sygen.ais.plot.para <-ggplot(data = sygen.ais.proportions.para, aes(x = yeardoi, y = frequency, fill = ais1)) +
  geom_bar(data = sygen.ais.proportions.para %>% filter(sex == "female") %>% arrange(rev(yeardoi)),
           stat = "identity")+
  geom_bar(data = sygen.ais.proportions.para %>% filter(sex == "male") %>% arrange(rev(yeardoi)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(1991, 1998), breaks = seq(1992, 1997, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of parapleic Patients [%]")+ ggtitle("Baseline Injury Severity")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )

sygen.ais.plot.para


ggsave(
  "ais.distribitution_para.pdf",
  plot = sygen.ais.plot.para,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()



#----Plot the population pyramide 'Baseline Injury Level' - OVERALL----

sygen.nli.proportions = sygen.included.cohort %>%
  count(yeardoi,splvl1,sex) %>%
  group_by(yeardoi,sex)%>% mutate(frequency = (n / sum(n))*100)

#Model
sygen.nli.proportions.lm <-lm(frequency~yeardoi*splvl1, data=sygen.nli.proportions)
summary(sygen.nli.proportions.lm)


#Plot
sygen.nli.plot <- ggplot(data = sygen.nli.proportions, aes(x = yeardoi, y = frequency, fill = splvl1)) +
  geom_bar(data = sygen.nli.proportions %>% filter(sex == "female") %>% arrange(rev(yeardoi)),
           stat = "identity")+
  geom_bar(data = sygen.nli.proportions %>% filter(sex == "male") %>% arrange(rev(yeardoi)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(1991, 1998), breaks = seq(1992, 1997, 1), expand = c(0,0))+ 
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
sygen.nli.plot

ggsave(
  "nli.distribitution_overall.pdf",
  plot = sygen.nli.plot,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()



#----Plot the population pyramide 'Baseline Injury Level' - AIS A----

sygen.nli.proportions.ais_a = subset(sygen.included.cohort, ais1=="AIS A") %>%
  count(yeardoi,splvl1,sex) %>%
  group_by(yeardoi,sex)%>% mutate(frequency = (n / sum(n))*100)


sygen.nli.ais.a <- ggplot(data = sygen.nli.proportions.ais_a, aes(x = yeardoi, y = frequency, fill = splvl1)) +
  geom_bar(data = sygen.nli.proportions.ais_a %>% filter(sex == "female") %>% arrange(rev(yeardoi)),
           stat = "identity")+
  geom_bar(data = sygen.nli.proportions.ais_a %>% filter(sex == "male") %>% arrange(rev(yeardoi)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(1991, 1998), breaks = seq(1992, 1997, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of AIS A Patients [%]")+ ggtitle("Baseline Injury level")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )
sygen.nli.ais.a

ggsave(
  "nli.distribitution.ais_a.pdf",
  plot = sygen.nli.ais.a,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()



#----Plot the population pyramide 'Baseline Injury Level' - AIS B----


sygen.nli.proportions.ais_b = subset(sygen.included.cohort, ais1=="AIS B") %>%
  count(yeardoi,splvl1,sex) %>%
  group_by(yeardoi,sex)%>% mutate(frequency = (n / sum(n))*100)


sygen.nli.ais.b <- ggplot(data = sygen.nli.proportions.ais_b, aes(x = yeardoi, y = frequency, fill = splvl1)) +
  geom_bar(data = sygen.nli.proportions.ais_b %>% filter(sex == "female") %>% arrange(rev(yeardoi)),
           stat = "identity")+
  geom_bar(data = sygen.nli.proportions.ais_b %>% filter(sex == "male") %>% arrange(rev(yeardoi)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(1991, 1998), breaks = seq(1992, 1997, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of AIS B Patients [%]")+ ggtitle("Baseline Injury level")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )
sygen.nli.ais.b

ggsave(
  "nli.distribitution.ais_b.pdf",
  plot = sygen.nli.ais.b,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()

#----Plot the population pyramide 'Baseline Injury Level' - AIS C----


sygen.nli.proportions.ais_c = subset(sygen.included.cohort, ais1=="AIS C") %>%
  count(yeardoi,splvl1,sex) %>%
  group_by(yeardoi,sex)%>% mutate(frequency = (n / sum(n))*100)


sygen.nli.ais.c <- ggplot(data = sygen.nli.proportions.ais_c, aes(x = yeardoi, y = frequency, fill = splvl1)) +
  geom_bar(data = sygen.nli.proportions.ais_c %>% filter(sex == "female") %>% arrange(rev(yeardoi)),
           stat = "identity")+
  geom_bar(data = sygen.nli.proportions.ais_c %>% filter(sex == "male") %>% arrange(rev(yeardoi)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(1991, 1998), breaks = seq(1992, 1997, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of AIS C Patients [%]")+ ggtitle("Baseline Injury level")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )
sygen.nli.ais.c

ggsave(
  "nli.distribitution.ais_c.pdf",
  plot = sygen.nli.ais.c,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()


#----Plot the population pyramide 'Baseline Injury Level' - AIS D----

sygen.nli.proportions.ais_d = subset(sygen.included.cohort, ais1=="AIS D") %>%
  count(yeardoi,splvl1,sex) %>%
  group_by(yeardoi,sex)%>% mutate(frequency = (n / sum(n))*100)


sygen.nli.ais.d <- ggplot(data = sygen.nli.proportions.ais_d, aes(x = yeardoi, y = frequency, fill = splvl1)) +
  geom_bar(data = sygen.nli.proportions.ais_d %>% filter(sex == "female") %>% arrange(rev(yeardoi)),
           stat = "identity")+
  geom_bar(data = sygen.nli.proportions.ais_d %>% filter(sex == "male") %>% arrange(rev(yeardoi)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(1991, 1998), breaks = seq(1992, 1997, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of AIS D Patients [%]")+ ggtitle("Baseline Injury level")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )
sygen.nli.ais.d

ggsave(
  "nli.distribitution.ais_d.pdf",
  plot = sygen.nli.ais.d,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()
