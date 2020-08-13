##### Code created by C. Jutzeler August 3rd, 2020
##### Validation Cohort: Sygen trial

#Clear workspace
rm(list = ls())

#where libraries are stored
.libPaths()

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
# if(!require(epicalc)){devtools::install_version("epicalc",version="2.15.1.0")}
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
library(epicalc)
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


rm(list=ls())

#-------------------------Data wrangling------------------------------------------------------------------------------------------------------

#load original dataset
sygen<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/Sygen.csv", sep = ',', header = T,  na.strings=c("","NA"))

#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
sygen.included.cohort<- subset(sygen, (age > 0) & (sex=="female" | sex=="male") & ###Age at DOI and Sex
                             (splvl1 == 'C' | splvl1 == 'T')&   ## Neurological level
                             (ais1=="AIS A"| ais1=="AIS B"| ais1=="AIS C"| ais1=="AIS D")) #AIS Grades


#Create Summary Table
#Formatting of table: Customize levels, labels, and units of listed variables
#Change names of levels of variables
levels(sygen.included.cohort$sex) <- c("Female", "Male")
levels(sygen.included.cohort$ais1) <- c("A", "B", "C", "D")
levels(sygen.included.cohort$splvl1) <- c("Cervical", "Thoracic")

#Relable variables
label(sygen.included.cohort$sex) <- "Sex"
label(sygen.included.cohort$age) <- "Age"
# label(emsci.trauma.sex.va.a1$NLI_level)<- "Neurological level of injury"
label(sygen.included.cohort$yeardoi) <- "Year of injury"
label(sygen.included.cohort$ais1) <- "AIS"
label(sygen.included.cohort$splvl1) <- "Neurological level of injury"

#Assign units to Age at Injury and Year of Injury
units(sygen.included.cohort$age) <- "years"
units(sygen.included.cohort$yeardoi) <- "years"

#Print table
table1::table1(~ sex+age+ais1+splvl1 | yeardoi, data = sygen.included.cohort)

#Calculate the change in age distribution over time - OVERALL ----
age_model.overall.sygen <-lm(age~yeardoi, data=sygen.included.cohort)
summary(age_model.overall.sygen)

#interaction tearm YEARDOI and Sex
age_model.sex.sygen <-lm(age~yeardoi*sex, data=sygen.included.cohort)
summary(age_model.sex.sygen)

####Calculate the change age distribution between 1992-1997 years - FEMALE ----
#Tetraplegia
age_model.tetra.female.sygen <-lm(age~yeardoi, data=subset(sygen.included.cohort, plegia == 'tetra' & sex =='Female'))
summary(age_model.tetra.female.sygen)

#Paraplegia
age_model.para.female.sygen <-lm(age~yeardoi, data=subset(sygen.included.cohort, plegia == 'para' & sex =='Female'))
summary(age_model.para.female.sygen)

#AIS A
age_model.ais_a.female.sygen <-lm(age~yeardoi, data=subset(sygen.included.cohort, ais1 == 'A' & sex =='Female'))
summary(age_model.ais_a.female.sygen)

#AIS B
age_model.ais_b.female.sygen <-lm(age~yeardoi, data=subset(sygen.included.cohort, ais1 == 'B' & sex =='Female'))
summary(age_model.ais_b.female.sygen)

#AIS C
age_model.ais_c.female.sygen <-lm(age~yeardoi, data=subset(sygen.included.cohort, ais1 == 'C' & sex =='Female'))
summary(age_model.ais_c.female.sygen)

#AIS D
age_model.ais_d.female.sygen <-lm(age~yeardoi, data=subset(sygen.included.cohort, ais1 == 'D' & sex =='Female'))
summary(age_model.ais_d.female.sygen)


####Calculate the change age distribution between 1992-1997 years - MALE ----
#Tetraplegia
age_model.tetra.male.sygen <-lm(age~yeardoi, data=subset(sygen.included.cohort, plegia == 'tetra' & sex =='Male'))
summary(age_model.tetra.male.sygen)

#Paraplegia
age_model.para.male.sygen <-lm(age~yeardoi, data=subset(sygen.included.cohort, plegia == 'para' & sex =='Male'))
summary(age_model.para.male.sygen)

#AIS A
age_model.ais_a.male.sygen <-lm(age~yeardoi, data=subset(sygen.included.cohort, ais1 == 'A' & sex =='Male'))
summary(age_model.ais_a.male.sygen)

#AIS B
age_model.ais_b.male.sygen <-lm(age~yeardoi, data=subset(sygen.included.cohort, ais1 == 'B' & sex =='Male'))
summary(age_model.ais_b.male.sygen)

#AIS C
age_model.ais_c.male.sygen <-lm(age~yeardoi, data=subset(sygen.included.cohort, ais1 == 'C' & sex =='Male'))
summary(age_model.ais_c.male.sygen)

#AIS D
age_model.ais_d.male.sygen <-lm(age~yeardoi, data=subset(sygen.included.cohort, ais1 == 'D' & sex =='Male'))
summary(age_model.ais_d.male.sygen)



#---Visualization: Change in age distribution between 1992-1997 years - OVERALL-----

#Set theme
theme_set(theme_ridges())

levels(sygen.included.cohort$sex) <- c("Female", "Male")


#Create plot
age_overall.sygen <- ggplot(
  sygen.included.cohort, 
  aes(y = as.factor(yeardoi) , x = age)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 1992 and 1997: Overall') +facet_grid(.~sygen.included.cohort$sex)+
  xlab("Age at Injury")+ylab("Year of Injury")+
  theme_minimal()+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        #plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text.align = 0)

age_overall.sygen

ggsave(
  "age_overall.sygen.pdf",
  plot = age_overall.sygen,
  device = 'pdf',
  path = '/Users/jutzca/Desktop/Figures',    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---Visualization: Change in age distribution between 1992-1997 years - PARAPLEGIA-----

#Set theme

theme_set(theme_ridges())

levels(sygen.included.cohort$sex) <- c("Female", "Male")

sygen.included.cohort.para <-subset(sygen.included.cohort, plegia =='para')


#Create plot
age_para.sygen <- ggplot(
  sygen.included.cohort.para, 
  aes(y = as.factor(yeardoi) , x = age)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 2001 and 2019: Paraplegic Patients') +facet_grid(.~sygen.included.cohort.para$sex)+
  xlab("Age at Injury")+ylab("Year of Injury")+
  theme_minimal()+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        #plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text.align = 0)

age_para.sygen

ggsave(
  "age_para.sygen.pdf",
  plot = age_para.sygen,
  device = 'pdf',
  path = '/Users/jutzca/Desktop/Figures',    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---Visualization: Change in age distribution between 1992-1997 years - TETRAPLEGIA-----

levels(sygen.included.cohort$sex) <- c("Female", "Male")

sygen.included.cohort.tetra <-subset(sygen.included.cohort, plegia =='tetra')


#Create plot
age_tetra.sygen <- ggplot(
  sygen.included.cohort.tetra, 
  aes(y = as.factor(yeardoi) , x = age)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 2001 and 2019: Tetraplegic Patients') +facet_grid(.~sygen.included.cohort.tetra$sex)+
  xlab("Age at Injury")+ylab("Year of Injury")+
  theme_minimal()+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        #plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text.align = 0)

age_tetra.sygen

ggsave(
  "age_tetra.sygen.pdf",
  plot = age_tetra.sygen,
  device = 'pdf',
  path = '/Users/jutzca/Desktop/Figures',    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()







#------Data Analysis: Change in ratio between female and male patients over 20 years and display in table----

#Count percentage of female and male subjects by Year of Injury
sex_ratios.sygen = sygen.included.cohort %>%
  count(sex, yeardoi) %>%
  group_by(yeardoi)%>%
  mutate(frequency = (n / sum(n))*100)

#Reshape data from long to wide in order to calculate ratios
sex_ratios.sygen <- dcast(sex_ratios.sygen, yeardoi ~ sex, value.var="n")
sex_ratios.sygen

#Calculate ratios
sex_ratios.sygen$Ratios <-
  case_when(
    is.na(sex_ratios.sygen$Male) & is.na(sex_ratios.sygen$Female) ~ 1,
    is.na(sex_ratios.sygen$Male) & sex_ratios.sygen$Female >= 0 ~ 1,
    sex_ratios.sygen$Male >= 0 & is.na(sex_ratios.sygen$Female) ~ 0,
    TRUE ~ sex_ratios.sygen$Male / sex_ratios.sygen$Female
  )

#Run LM to investigate if there was a change in sex ratio between 2001 and 2019
sex_ratios.sygen.lm <- lm(Ratios~yeardoi, data=sex_ratios.sygen)
summary(sex_ratios.sygen.lm)



#------Calculate the number of patients per year by sex - Overall----
sygen.sex.long <- sygen.included.cohort %>%
  count(sex, yeardoi)%>%
  group_by(yeardoi)%>%
  mutate(frequency = (n / sum(n))*100)
#------Plot population pyramide for year and color by sex - OVERALL ----
##Plot data for the male patients
gg.male.sygen <- ggplot(data = subset(sygen.sex.long,sex=='Male'), 
                  mapping = aes(
                    x = as.factor(yeardoi), 
                    y = frequency, 
                    fill = sex,
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
gg.male.sygen

##Plot data for the female patients
gg.female.sygen <-  ggplot(data = subset(sygen.sex.long,sex=='Female'), 
                     mapping = aes(
                       x = as.factor(yeardoi), 
                       y = frequency, 
                       fill = sex,
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
gg.female.sygen

## Plutting the graphs together together
sex.overall.sygen <- grid.arrange(gg.female.sygen,
                            gg.male.sygen,
                            widths=c(0.4,0.4),
                            ncol=2,  top = textGrob("Overall",gp=gpar(fontsize=12,font=2))
)


ggsave(
  "sex.overall.sygen.pdf",
  plot = sex.overall.sygen,
  device = 'pdf',
  path = '/Users/jutzca/Desktop/Figures',
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#------Calculate the number of patients per year by sex - PARAPLEGIA----
sygen.included.cohort.para <- subset(sygen.included.cohort, plegia=='para') %>%
  count(sex, yeardoi)%>%
  group_by(yeardoi)%>%
  mutate(frequency = (n / sum(n))*100)
#------Plot population pyramide for year and color by Sex - PARAPLEGIA ----
##Plot data for the male patients
gg.male.para.sygen <- ggplot(data = subset(sygen.included.cohort.para,sex=='Male'), 
                       mapping = aes(
                         x = as.factor(yeardoi), 
                         y = frequency, 
                         fill = sex,
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
gg.male.para.sygen

##Plot data for the female patients
gg.female.para.sygen <-  ggplot(data = subset(sygen.included.cohort.para,sex=='Female'), 
                          mapping = aes(
                            x = as.factor(yeardoi), 
                            y = frequency, 
                            fill = sex,
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

gg.female.para.sygen

## Plutting the graphs together together
sex.paraplegia.sygen <- grid.arrange(gg.female.para.sygen,
                               gg.male.para.sygen,
                               widths=c(0.4,0.4),
                               ncol=2,  top = textGrob("Paraplegia",gp=gpar(fontsize=12,font=2))
)


ggsave(
  "sex.paraplegia.sygen.pdf",
  plot = sex.paraplegia.sygen,
  device = 'pdf',
  path = '/Users/jutzca/Desktop/Figures',
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#------Calculate the number of patients per year by sex - TETRAPLEGIA----
sygen.included.cohort.tetra <- subset(sygen.included.cohort, plegia=='tetra') %>%
  count(sex, yeardoi)%>%
  group_by(yeardoi)%>%
  mutate(frequency = (n / sum(n))*100)
#------Plot population pyramide for year and color by Sex - TETRAPLEGIA ----
##Plot data for the male patients
gg.male.tetra.sygen <- ggplot(data = subset(sygen.included.cohort.tetra,sex=='Male'), 
                        mapping = aes(
                          x = as.factor(yeardoi), 
                          y = frequency, 
                          fill = sex,
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

gg.male.tetra.sygen

##Plot data for the female patients
gg.female.tetra.sygen <-  ggplot(data = subset(sygen.included.cohort.tetra,sex=='Female'), 
                           mapping = aes(
                             x = as.factor(yeardoi), 
                             y = frequency, 
                             fill = sex,
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
gg.female.tetra.sygen

## Plutting the graphs together together
sex.tetrapelgia.sygen <- grid.arrange(gg.female.tetra.sygen,
                                gg.male.tetra.sygen,
                                widths=c(0.4,0.4),
                                ncol=2,  top = textGrob("Tetraplegia",gp=gpar(fontsize=12,font=2))
)

ggsave(
  "sex.tetrapelgia.sygen.pdf",
  plot = sex.tetrapelgia.sygen,
  device = 'pdf',
  path = '/Users/jutzca/Desktop/Figures',
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


