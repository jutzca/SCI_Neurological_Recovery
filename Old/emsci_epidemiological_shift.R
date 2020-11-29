##################################################################################
##################################################################################
##### Code created by C. Jutzeler August 3rd, 2020 ###############################
##### Change in female:male ratio between 2001 and 2020 in the EMSCI cohory ######
##################################################################################
##################################################################################

#clear working space
rm(list=ls())

#The following commands will install these packages if they are not already installed:
  
#if(!require(lme4)){install.packages("lme4")}
#if(!require(sjPlot)){install.packages("sjPlot")}
#if(!require(jtools)){install.packages("jtools")}
#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(ggridges)){install.packages("ggridges")}
#if(!require(ggpubr)){install.packages("ggpubr")}
#if(!require(plyr)){install.packages("plyr")}
#if(!require(dplyr)){install.packages("dplyr")}
#if(!require(gridExtra)){install.packages("gridExtra")}
#if(!require(reshape2)){install.packages("reshape2")}
#if(!require(PMCMRplus)){install.packages("PMCMRplus")}
#if(!require(naniar)){install.packages("naniar")}
# if(!require(EpiReport)){install.packages("EpiReport")}
# if(!require(epiDisplay)){install.packages("epiDisplay")}
# devtools::install_github("hadley/devtools")
# if(!require(boot)){install.packages("boot")}
# if(!require(table1)){install.packages("table1")}
# if(!require(sjPlot)){install.packages("sjPlot")}
# if(!require(broom)){devtools::install_version("broom")}
# if(!require(pander)){install.packages("pander")}
# if(!require(gtable)){install.packages("gtable")}
# if(!require(grid)){install.packages("grid")}
# if(!require(data.table)){install.packages("data.table")}
# if(!require(tidyr)){install.packages("tidyr")}
# if(!require(ggthemes)){install.packages("ggthemes")}
# if(!require(Hmisc)){install.packages("Hmisc")}


#List of libraries required for the analyses below
library(lme4)
library(sjPlot)
library(jtools)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(plyr)
library(dplyr)
library(gridExtra)
library(reshape2)
library(PMCMRplus)
library(EpiReport)
library(epiDisplay)
library(naniar)
library(boot) 
library(table1)
library(sjPlot)
library(broom)
library(pander)
library(gtable)
library(grid)
library(data.table)
library(tidyr)
library('ggthemes')
library(Hmisc)
library(scales)



#where libraries are stored
.libPaths()

#paths
outdir_figures='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Figures'
outdir_tables='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables'


#-------------------------Data wrangling------------------------------------------------------------------------------------------------------

#load original dataset
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T,  na.strings=c("","NA"))


#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
emsci.trauma.sex <- subset(emsci, (AgeAtDOI > 8) & (Sex=='f' | Sex=='m') & ###Age at DOI and Sex
                             (Cause=="ischemic" | Cause=="traumatic" | Cause=="haemorragic" |Cause=="disc herniation") & 
                             (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar')&   ## Neurological level
                             (AIS=="A"| AIS=="B"| AIS=="C"| AIS=="D")) #AIS Grades

#------Data Analysis: Change in ratio between female and male patients over 20 years and display in table----

#Load data
emsci.trauma.sex.va.a1<-distinct(subset(emsci.trauma.sex, ExamStage=='acute I' | ExamStage=='very acute'), Patientennummer, .keep_all = TRUE)

#Count percentage of female and male subjects by Year of Injury
sex_ratios = emsci.trauma.sex.va.a1 %>%
  count(Sex, YEARDOI) %>%
  group_by(YEARDOI)%>%
  mutate(frequency = (n / sum(n))*100)

#Reshape data from long to wide in order to calculate ratios
sex_ratios <- dcast(sex_ratios, YEARDOI ~ Sex, value.var="n")
sex_ratios

#Calculate ratios
sex_ratios$Ratios <-
  case_when(
    is.na(sex_ratios$m) & is.na(sex_ratios$f) ~ 1,
    is.na(sex_ratios$m) & sex_ratios$f >= 0 ~ 1,
    sex_ratios$m >= 0 & is.na(sex_ratios$f) ~ 0,
    TRUE ~ sex_ratios$m / sex_ratios$f
  )

#Run LM to investigate if there was a change in sex ratio between 2001 and 2019
sex_ratios.lm <- lm(Ratios~YEARDOI, data=sex_ratios)
summary(sex_ratios.lm)
tab_model(sex_ratios.lm)

#------Calculate the number of patients per year by sex - Overall----
emsci.sex.long <- emsci.trauma.sex %>%
                  count(Sex, YEARDOI)%>%
                  group_by(YEARDOI)%>%
                  mutate(frequency = (n / sum(n))*100)
#------Plot population pyramide for year and color by sex - OVERALL ----
##Plot data for the male patients
gg.male <- ggplot(data = subset(emsci.sex.long,Sex=='m'), 
                  mapping = aes(
                    x = as.factor(YEARDOI), 
                    y = frequency, 
                    fill = Sex,
                    label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                  )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=as.vector("#3E606F"))+
  geom_text(hjust=(1.1), size=3.5, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 14),
        legend.text.align = 0)+ 
  ggtitle("Male") + 
  coord_flip()    

##Plot data for the female patients
gg.female <-  ggplot(data = subset(emsci.sex.long,Sex=='f'), 
                     mapping = aes(
                       x = as.factor(YEARDOI), 
                       y = frequency, 
                       fill = Sex,
                       label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                     )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(1), size=3.5, colour="#5D646F") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0)+ 
  ggtitle("Female") + 
  coord_flip()

## Plutting the graphs together together
sex.overall <- grid.arrange(gg.female,
             gg.male,
             widths=c(0.4,0.4),
             ncol=2,  top = textGrob("Overall",gp=gpar(fontsize=12,font=2))
)


ggsave(
  "sex.overall.pdf",
  plot = sex.overall,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#------Calculate the number of patients per year by sex - PARAPLEGIA----
emsci.sex.long.para <- subset(emsci.trauma.sex, plegia=='para') %>%
  count(Sex, YEARDOI)%>%
  group_by(YEARDOI)%>%
  mutate(frequency = (n / sum(n))*100)
#------Plot population pyramide for year and color by Sex - PARAPLEGIA ----
##Plot data for the male patients
gg.male.para <- ggplot(data = subset(emsci.sex.long.para,Sex=='m'), 
                  mapping = aes(
                    x = as.factor(YEARDOI), 
                    y = frequency, 
                    fill = Sex,
                    label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                  )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=as.vector("#3E606F"))+
  geom_text(hjust=(1.1), size=3.5, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 14),
        legend.text.align = 0)+ 
  ggtitle("Male") + 
  coord_flip()    

##Plot data for the female patients
gg.female.para <-  ggplot(data = subset(emsci.sex.long.para,Sex=='f'), 
                     mapping = aes(
                       x = as.factor(YEARDOI), 
                       y = frequency, 
                       fill = Sex,
                       label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                     )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(1), size=3.5, colour="#5D646F") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0)+ 
  ggtitle("Female") + 
  coord_flip()

## Plutting the graphs together together
sex.paraplegia <- grid.arrange(gg.female.para,
             gg.male.para,
             widths=c(0.4,0.4),
             ncol=2,  top = textGrob("Paraplegia",gp=gpar(fontsize=12,font=2))
)


ggsave(
  "sex.paraplegia.pdf",
  plot = sex.paraplegia,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#------Calculate the number of patients per year by sex - TETRAPLEGIA----
emsci.sex.long.tetra <- subset(emsci.trauma.sex, plegia=='tetra') %>%
  count(Sex, YEARDOI)%>%
  group_by(YEARDOI)%>%
  mutate(frequency = (n / sum(n))*100)
#------Plot population pyramide for year and color by Sex - TETRAPLEGIA ----
##Plot data for the male patients
gg.male.tetra <- ggplot(data = subset(emsci.sex.long.tetra,Sex=='m'), 
                       mapping = aes(
                         x = as.factor(YEARDOI), 
                         y = frequency, 
                         fill = Sex,
                         label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                       )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=as.vector("#3E606F"))+
  geom_text(hjust=(1.1), size=3.5, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 14),
        legend.text.align = 0)+ 
  ggtitle("Male") + 
  coord_flip()    

##Plot data for the female patients
gg.female.tetra <-  ggplot(data = subset(emsci.sex.long.tetra,Sex=='f'), 
                          mapping = aes(
                            x = as.factor(YEARDOI), 
                            y = frequency, 
                            fill = Sex,
                            label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                          )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(1), size=3.5, colour="#5D646F") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0)+ 
  ggtitle("Female") + 
  coord_flip()

## Plutting the graphs together together
sex.tetrapelgia <- grid.arrange(gg.female.tetra,
             gg.male.tetra,
             widths=c(0.4,0.4),
             ncol=2,  top = textGrob("Tetraplegia",gp=gpar(fontsize=12,font=2))
)

ggsave(
  "sex.tetrapelgia.pdf",
  plot = sex.tetrapelgia,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#------Calculate the number of patients per year by sex - AIS A----
emsci.sex.long.ais_a <- subset(emsci.trauma.sex, AIS=='A') %>%
  count(Sex, YEARDOI)%>%
  group_by(YEARDOI)%>%
  mutate(frequency = (n / sum(n))*100)
#------Plot population pyramide for year and color by Sex - AIS A ----
##Plot data for the male patients
gg.male.ais_a <- ggplot(data = subset(emsci.sex.long.ais_a,Sex=='m'), 
                       mapping = aes(
                         x = as.factor(YEARDOI), 
                         y = frequency, 
                         fill = Sex,
                         label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                       )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=as.vector("#3E606F"))+
  geom_text(hjust=(1.1), size=3.5, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 14),
        legend.text.align = 0)+ 
  ggtitle("Male") + 
  coord_flip()    
gg.male.ais_a

##Plot data for the female patients
gg.female.ais_a <-  ggplot(data = subset(emsci.sex.long.ais_a,Sex=='f'), 
                          mapping = aes(
                            x = as.factor(YEARDOI), 
                            y = frequency, 
                            fill = Sex,
                            label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                          )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(1), size=3.5, colour="#5D646F") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0)+ 
  ggtitle("Female") + 
  coord_flip()

## Plutting the graphs together together
sex.ais_a <- grid.arrange(gg.female.ais_a,
                               gg.male.ais_a,
                               widths=c(0.4,0.4),
                               ncol=2,  top = textGrob("AIS A",gp=gpar(fontsize=12,font=2))
)

sex.ais_a

ggsave(
  "sex.ais_a.pdf",
  plot = sex.ais_a,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#------Calculate the number of patients per year by sex - AIS B----
emsci.sex.long.ais_b <- subset(emsci.trauma.sex, AIS=='B') %>%
  count(Sex, YEARDOI)%>%
  group_by(YEARDOI)%>%
  mutate(frequency = (n / sum(n))*100)
#------Plot population pyramide for year and color by Sex - AIS B ----
##Plot data for the male patients
gg.male.ais_b <- ggplot(data = subset(emsci.sex.long.ais_b,Sex=='m'), 
                        mapping = aes(
                          x = as.factor(YEARDOI), 
                          y = frequency, 
                          fill = Sex,
                          label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                        )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=as.vector("#3E606F"))+
  geom_text(hjust=(1.1), size=3.5, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 14),
        legend.text.align = 0)+ 
  ggtitle("Male") + 
  coord_flip()    
gg.male.ais_b

##Plot data for the female patients
gg.female.ais_b <-  ggplot(data = subset(emsci.sex.long.ais_b,Sex=='f'), 
                           mapping = aes(
                             x = as.factor(YEARDOI), 
                             y = frequency, 
                             fill = Sex,
                             label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                           )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(1), size=3.5, colour="#5D646F") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0)+ 
  ggtitle("Female") + 
  coord_flip()

## Plutting the graphs together together
sex.ais_b <- grid.arrange(gg.female.ais_b,
                          gg.male.ais_b,
                          widths=c(0.4,0.4),
                          ncol=2,  top = textGrob("AIS B",gp=gpar(fontsize=12,font=2))
)

sex.ais_b

ggsave(
  "sex.ais_b.pdf",
  plot = sex.ais_b,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#------Calculate the number of patients per year by sex - AIS C----
emsci.sex.long.ais_c <- subset(emsci.trauma.sex, AIS=='C') %>%
  count(Sex, YEARDOI)%>%
  group_by(YEARDOI)%>%
  mutate(frequency = (n / sum(n))*100)
#------Plot population pyramide for year and color by Sex - AIS C ----
##Plot data for the male patients
gg.male.ais_c <- ggplot(data = subset(emsci.sex.long.ais_c,Sex=='m'), 
                        mapping = aes(
                          x = as.factor(YEARDOI), 
                          y = frequency, 
                          fill = Sex,
                          label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                        )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=as.vector("#3E606F"))+
  geom_text(hjust=(1.1), size=3.5, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 14),
        legend.text.align = 0)+ 
  ggtitle("Male") + 
  coord_flip()    
gg.male.ais_c

##Plot data for the female patients
gg.female.ais_c <-  ggplot(data = subset(emsci.sex.long.ais_c,Sex=='f'), 
                           mapping = aes(
                             x = as.factor(YEARDOI), 
                             y = frequency, 
                             fill = Sex,
                             label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                           )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(1), size=3.5, colour="#5D646F") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0)+ 
  ggtitle("Female") + 
  coord_flip()

## Plutting the graphs together together
sex.ais_c <- grid.arrange(gg.female.ais_c,
                          gg.male.ais_c,
                          widths=c(0.4,0.4),
                          ncol=2,  top = textGrob("AIS C",gp=gpar(fontsize=12,font=2))
)

sex.ais_b

ggsave(
  "sex.ais_c.pdf",
  plot = sex.ais_c,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()
#------Calculate the number of patients per year by sex - AIS D----
emsci.sex.long.ais_d <- subset(emsci.trauma.sex, AIS=='D') %>%
  count(Sex, YEARDOI)%>%
  group_by(YEARDOI)%>%
  mutate(frequency = (n / sum(n))*100)
#------Plot population pyramide for year and color by Sex - AIS D ----
##Plot data for the male patients
gg.male.ais_d <- ggplot(data = subset(emsci.sex.long.ais_d,Sex=='m'), 
                        mapping = aes(
                          x = as.factor(YEARDOI), 
                          y = frequency, 
                          fill = Sex,
                          label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                        )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=as.vector("#3E606F"))+
  geom_text(hjust=(1.1), size=3.5, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 14),
        legend.text.align = 0)+ 
  ggtitle("Male") + 
  coord_flip()    
gg.male.ais_d

##Plot data for the female patients
gg.female.ais_d <-  ggplot(data = subset(emsci.sex.long.ais_d,Sex=='f'), 
                           mapping = aes(
                             x = as.factor(YEARDOI), 
                             y = frequency, 
                             fill = Sex,
                             label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                           )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(1), size=3.5, colour="#5D646F") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0)+ 
  ggtitle("Female") + 
  coord_flip()

## Plutting the graphs together together
sex.ais_d <- grid.arrange(gg.female.ais_d,
                          gg.male.ais_d,
                          widths=c(0.4,0.4),
                          ncol=2,  top = textGrob("AIS D",gp=gpar(fontsize=12,font=2))
)

sex.ais_d

ggsave(
  "sex.ais_d.pdf",
  plot = sex.ais_d,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

