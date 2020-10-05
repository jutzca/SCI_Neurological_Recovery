##### Code created by C. Jutzeler August 3rd, 2020
##### Validation Cohort: Sygen trial

#Clear workspace
rm(list = ls())

#where libraries are stored
.libPaths()


#paths
outdir_figures='/Users/jutzca/Documents/Github/EMSCI_20_Years/Figures'
outdir_tables='/Users/jutzca/Documents/Github/EMSCI_20_Years/Tables'
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
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T,  na.strings=c("","NA"))

#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
emsci.trauma.sex <- subset(emsci, (AgeAtDOI > 8) & (Sex=='f' | Sex=='m') & ###Age at DOI and Sex
                             (Cause=="ischemic" | Cause=="traumatic" | Cause=="haemorragic" |Cause=="disc herniation") & 
                             (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar')&   ## Neurological level
                             (AIS=="A"| AIS=="B"| AIS=="C"| AIS=="D")) #AIS Grades



#-------------------------Data analysis and visualization of ------------------------------------------------------------------------------------------------------

#------Data analysis and creation of table: Included cohort------

#Subset data to only patients with valid entry at stage 'very acute' or 'acute I' and remove duplicate patient numbers
emsci.trauma.sex.va.a1<-distinct(subset(emsci.trauma.sex, ExamStage=='acute I' | ExamStage=='very acute') , Patientennummer, .keep_all = TRUE)
#relevel cause and injury level of injury
emsci.trauma.sex.va.a1$Cause <- factor(emsci.trauma.sex.va.a1$Cause, levels=c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
emsci.trauma.sex.va.a1$NLI_level <- factor(emsci.trauma.sex.va.a1$NLI_level, levels=c("cervical", "thoracic", "lumbar", "sacral"))

#Create Summary Table
#Formatting of table: Customize levels, labels, and units of listed variables
#Change names of levels of variables
levels(emsci.trauma.sex.va.a1$Sex) <- c("Female", "Male")
levels(emsci.trauma.sex.va.a1$AIS) <- c("A", "B", "C", "D", "E", "Not tested")
levels(emsci.trauma.sex.va.a1$Cause) <- c("Disc herniation", "Haemorragic", "Ischemic", "Traumatic", "Other")
levels(emsci.trauma.sex.va.a1$NLI_level) <- c("Cervical", "Thoracic", "Lumbar", "Sacral")

#Relable variables
label(emsci.trauma.sex.va.a1$Sex) <- "Sex"
label(emsci.trauma.sex.va.a1$AgeAtDOI) <- "Age"
label(emsci.trauma.sex.va.a1$NLI_level)<- "Neurological level of injury"
label(emsci.trauma.sex.va.a1$YEARDOI) <- "Year of injury"

#Assign units to Age at Injury and Year of Injury
units(emsci.trauma.sex.va.a1$AgeAtDOI) <- "years"
units(emsci.trauma.sex.va.a1$YEARDOI) <- "years"

#Print table
table1::table1(~ Sex+AgeAtDOI+Cause+AIS+NLI_level, data = emsci.trauma.sex.va.a1)


#------Data analysis, creation of table, and figure: Patients enrolled per country------
#Create Supplemantray Table 1: Number of patients enrolled per country (5 year bins)
table1::table1(~ Country | X5_year_bins, data = emsci.trauma.sex.va.a1)

###Calculate Number of patients enrolled per year
nr.of.patients.enrolled.per.year = emsci.trauma.sex.va.a1%>%
  count(YEARDOI) %>%
  group_by(YEARDOI)

nr.of.patients.enrolled.per.year

library(ggplot2)

patients_enrolled_per_country <-ggplot(nr.of.patients.enrolled.per.year, aes(as.factor(YEARDOI),as.numeric(n), fill=n))+  geom_bar(stat="identity")+
  geom_text(aes(label=n), vjust=-0.3, size=3.5)+
  theme_economist()+
  theme(legend.position = "none",
        axis.title.y= element_blank(),
        axis.text.y= element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45))+
  scale_fill_continuous(low="red", high="darkblue") +
  labs(x= "Year", y="Number of patients enrolled")
patients_enrolled_per_country



##Save plot
ggsave(
  "patients_enrolled_per_country.pdf",
  plot = patients_enrolled_per_country,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 4,
  height = 3,
  units = "in",
  dpi = 300
)

dev.off()





#------Data analysis and creation of table: Excluded cohort------
#####Summary Table: Excluded cohort
#Extract excluded cohort
emsci.missing<-anti_join(emsci,emsci.trauma.sex.va.a1, by ="Patientennummer")
emsci.missing.unique<-distinct(emsci.missing, Patientennummer, .keep_all = TRUE)

#Format 
levels(emsci.missing.unique$Sex) <- c("Female", "Male")
levels(emsci.missing.unique$AIS) <- c("A", "B", "C", "D", "E", "Not tested")
levels(emsci.missing.unique$Cause) <- c("Disc herniation", "Haemorragic", "Ischemic", "Traumatic", "Other")
levels(emsci.missing.unique$NLI_level) <- c("Cervical", "Thoracic", "Lumbar", "Sacral")

label(emsci.missing.unique$Sex)       <- "Sex"
label(emsci.missing.unique$AgeAtDOI)       <- "Age"
label(emsci.missing.unique$NLI_level)     <- "Neurological level of injury"
label(emsci.missing.unique$YEARDOI) <- "Year of injury"

units(emsci.missing.unique$AgeAtDOI)       <- "years"
units(emsci.missing.unique$YEARDOI) <- "years"

#Summary Table: Excluded cohort
table1::table1(~ Sex+AgeAtDOI+Cause+AIS+NLI_level+YEARDOI, data = emsci.missing.unique)


#------Comparison between included and exlcuded cohorts------




#------Age distribution over time: Data analysis and visualization------

#Data analysis: Change in age distribution over 20 years

#Description of variables
des(emsci.trauma.sex.va.a1)
summ(emsci.trauma.sex.va.a1$YEARDOI, by=emsci.trauma.sex.va.a1$Sex)
dotplot(emsci.trauma.sex.va.a1$AgeAtDOI, by = emsci.trauma.sex.va.a1$Sex) 

#Create age groups
agegr <- cut(emsci.trauma.sex.va.a1$AgeAtDOI, breaks=c(18,39,59,79)) 
table(agegr)

#Calculate the change in age distribution over time - OVERALL ----
age_model.overall <-lm(AgeAtDOI~YEARDOI, data=emsci.trauma.sex.va.a1)
summary(age_model.overall)

#interaction tearm YEARDOI and Sex
age_model.sex <-lm(AgeAtDOI~YEARDOI*Sex, data=emsci.trauma.sex.va.a1)
summary(age_model.sex)

####Calculate the change age distribution over time - FEMALE ----
#Tetraplegia
age_model.tetra.female <-lm(AgeAtDOI~YEARDOI, data=subset(emsci.trauma.sex.va.a1, plegia == 'tetra' & Sex =='Female'))
summary(age_model.tetra.female)

#Paraplegia
age_model.para.female <-lm(AgeAtDOI~YEARDOI, data=subset(emsci.trauma.sex.va.a1, plegia == 'para' & Sex =='Female'))
summary(age_model.para.female)

#AIS A
age_model.ais_a.female <-lm(AgeAtDOI~YEARDOI, data=subset(emsci.trauma.sex.va.a1, AIS == 'A' & Sex =='Female'))
summary(age_model.ais_a.female)

#AIS B
age_model.ais_b.female <-lm(AgeAtDOI~YEARDOI, data=subset(emsci.trauma.sex.va.a1, AIS == 'B' & Sex =='Female'))
summary(age_model.ais_b.female)

#AIS C
age_model.ais_c.female <-lm(AgeAtDOI~YEARDOI, data=subset(emsci.trauma.sex.va.a1, AIS == 'C' & Sex =='Female'))
summary(age_model.ais_c.female)

#AIS D
age_model.ais_d.female <-lm(AgeAtDOI~YEARDOI, data=subset(emsci.trauma.sex.va.a1, AIS == 'D' & Sex =='Female'))
summary(age_model.ais_d.female)


####Calculate the change age distribution over time - MALE ----
#Tetraplegia
age_model.tetra.male <-lm(AgeAtDOI~YEARDOI, data=subset(emsci.trauma.sex.va.a1, plegia == 'tetra' & Sex =='Male'))
summary(age_model.tetra.male)

#Paraplegia
age_model.para.male <-lm(AgeAtDOI~YEARDOI, data=subset(emsci.trauma.sex.va.a1, plegia == 'para' & Sex =='Male'))
summary(age_model.para.male)

#AIS A
age_model.ais_a.male <-lm(AgeAtDOI~YEARDOI, data=subset(emsci.trauma.sex.va.a1, AIS == 'A' & Sex =='Male'))
summary(age_model.ais_a.male)

#AIS B
age_model.ais_b.male <-lm(AgeAtDOI~YEARDOI, data=subset(emsci.trauma.sex.va.a1, AIS == 'B' & Sex =='Male'))
summary(age_model.ais_b.male)

#AIS C
age_model.ais_c.male <-lm(AgeAtDOI~YEARDOI, data=subset(emsci.trauma.sex.va.a1, AIS == 'C' & Sex =='Male'))
summary(age_model.ais_c.male)

#AIS D
age_model.ais_d.male <-lm(AgeAtDOI~YEARDOI, data=subset(emsci.trauma.sex.va.a1, AIS == 'D' & Sex =='Male'))
summary(age_model.ais_d.male)



#---Visualization: Change in age distribution over 20 years - OVERALL-----

#Set theme
theme_set(theme_ridges())

levels(emsci.trauma.sex.va.a1$Sex) <- c("Female", "Male")


#Create plot
age_overall <- ggplot(
  emsci.trauma.sex.va.a1, 
  aes(y = as.factor(YEARDOI) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 3, size = 0.3
  ) + scale_fill_gradientn(
    #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
    colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
    #colours = c("#8C3F4D","#3E606F"),
    name = "Age [years]"
  )+
  labs(title = 'Age at Injury between 2001 and 2019: Overall') +facet_grid(.~emsci.trauma.sex.va.a1$Sex)+
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

age_overall

ggsave(
  "age_overall.pdf",
  plot = age_overall,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#---Visualization: Change in age distribution over 20 years - PARAPLEGIA-----

#Set theme
theme_set(theme_ridges())

emsci.trauma.sex.va.a1.para <-subset(emsci.trauma.sex.va.a1, plegia =='para')

levels(emsci.trauma.sex.va.a1.para$Sex) <- c("Female", "Male")


#Create plot
age_para <- ggplot(
  emsci.trauma.sex.va.a1.para, 
  aes(y = as.factor(YEARDOI) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 2001 and 2019: Paraplegic Patients') +facet_grid(.~emsci.trauma.sex.va.a1.para$Sex)+
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

age_para

ggsave(
  "age_para.pdf",
  plot = age_para,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---Visualization: Change in age distribution over 20 years - TETRAPLEGIA-----

#Set theme
theme_set(theme_ridges())

emsci.trauma.sex.va.a1.tetra <-subset(emsci.trauma.sex.va.a1, plegia =='tetra')

levels(emsci.trauma.sex.va.a1.tetra$Sex) <- c("Female", "Male")


#Create plot
age_tetra <- ggplot(
  emsci.trauma.sex.va.a1.tetra, 
  aes(y = as.factor(YEARDOI) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 2001 and 2019: Tetraplegic Patients') +facet_grid(.~emsci.trauma.sex.va.a1.tetra$Sex)+
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

age_tetra

ggsave(
  "age_tetra.pdf",
  plot = age_tetra,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()



#---Visualization: Change in age distribution over 20 years - AIS A-----

#Set theme
theme_set(theme_ridges())

emsci.trauma.sex.va.a1.ais_a <-subset(emsci.trauma.sex.va.a1, AIS =='A')

levels(emsci.trauma.sex.va.a1.ais_a$Sex) <- c("Female", "Male")


#Create plot
age_ais_a <- ggplot(
  emsci.trauma.sex.va.a1.ais_a, 
  aes(y = as.factor(YEARDOI) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 2001 and 2019: AIS A Patients') +facet_grid(.~emsci.trauma.sex.va.a1.ais_a$Sex)+
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

age_ais_a

ggsave(
  "age_ais_a.pdf",
  plot = age_ais_a,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()



#---Visualization: Change in age distribution over 20 years - AIS B-----

#Set theme
theme_set(theme_ridges())

emsci.trauma.sex.va.a1.ais_b <-subset(emsci.trauma.sex.va.a1, AIS =='B')

levels(emsci.trauma.sex.va.a1.ais_b$Sex) <- c("Female", "Male")


#Create plot
age_ais_b <- ggplot(
  emsci.trauma.sex.va.a1.ais_b, 
  aes(y = as.factor(YEARDOI) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 2001 and 2019: AIS B Patients') +facet_grid(.~emsci.trauma.sex.va.a1.ais_b$Sex)+
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

age_ais_b

ggsave(
  "age_ais_b.pdf",
  plot = age_ais_b,
  device = 'pdf',
  path = '/Users/jutzca/Documents/Github/EMSCI_20_Years/Figures',    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#---Visualization: Change in age distribution over 20 years - AIS C-----

#Set theme
theme_set(theme_ridges())

emsci.trauma.sex.va.a1.ais_c <-subset(emsci.trauma.sex.va.a1, AIS =='C')

levels(emsci.trauma.sex.va.a1.ais_c$Sex) <- c("Female", "Male")


#Create plot
age_ais_c <- ggplot(
  emsci.trauma.sex.va.a1.ais_c, 
  aes(y = as.factor(YEARDOI) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 2001 and 2019: AIS C Patients') +facet_grid(.~emsci.trauma.sex.va.a1.ais_c$Sex)+
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

age_ais_c

ggsave(
  "age_ais_c.pdf",
  plot = age_ais_c,
  device = 'pdf',
  path = '/Users/jutzca/Documents/Github/EMSCI_20_Years/Figures',    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()



#---Visualization: Change in age distribution over 20 years - AIS C-----

#Set theme
theme_set(theme_ridges())

emsci.trauma.sex.va.a1.ais_d <-subset(emsci.trauma.sex.va.a1, AIS =='D')

levels(emsci.trauma.sex.va.a1.ais_d$Sex) <- c("Female", "Male")


#Create plot
age_ais_d <- ggplot(
  emsci.trauma.sex.va.a1.ais_d, 
  aes(y = as.factor(YEARDOI) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 2001 and 2019: AIS D Patients') +facet_grid(.~emsci.trauma.sex.va.a1.ais_d$Sex)+
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

age_ais_d

ggsave(
  "age_ais_d.pdf",
  plot = age_ais_d,
  device = 'pdf',
  path = '/Users/jutzca/Documents/Github/EMSCI_20_Years/Figures',    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

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
  path = '/Users/jutzca/Documents/Github/EMSCI_20_Years/Figures',
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
  path = '/Users/jutzca/Desktop/Figures',
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
  path = '/Users/jutzca/Desktop/Figures',
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
  path = '/Users/jutzca/Desktop/Figures',    ###Set path to save figures
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
  path = '/Users/jutzca/Desktop/Figures',    ###Set path to save figures
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
  path = '/Users/jutzca/Desktop/Figures',    ###Set path to save figures
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
  path = '/Users/jutzca/Desktop/Figures',    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()
#------Assess whether the proportion of injury severities (AIS A, B, C, D, and E) changed over time and display in table----

#Subset data: all patients with entry of AIS grade at stage acute I
emsci.ais <- subset(emsci.trauma.sex, AIS=="A"| AIS=="B"| AIS=="C"| AIS=="D")

#Create age groups
emsci.ais$agegr <- cut(emsci.ais$AgeAtDOI, breaks=c(0,19,29,49,69,100)) 
table(emsci.ais$agegr)


#Calculate ratios of AIS grades per age group per year for female subjects
ais_ratios.by.agegroup.female = subset(emsci.ais, Sex=='f') %>%
  count(AIS, YEARDOI, agegr) %>%
  group_by(agegr, YEARDOI)%>%
  mutate(frequency = (n / sum(n))*100)


#Fit regression model with the proportion of AIS grade as the response, and time as the predictor
ais_ratios.by.agegroup.female.lm<-ais_ratios.by.agegroup.female %>%
  group_by(AIS,agegr) %>%         
  do(tidy(lm(frequency~YEARDOI, data=.))%>%
       mutate(
         'CI low' = estimate - std.error,
         'CI high' = estimate + std.error
       )
  ) 

ais_ratios.by.agegroup.female.lm <- ais_ratios.by.agegroup.female.lm[c(2,4,6,8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40),]

dodger = position_dodge(width = 0.3)
levels(ais_ratios.by.agegroup.female.lm$agegr) <- c("below 20 years","20-30 years", "31-50 years ", "51-69 years", "70+ years")
levels(ais_ratios.by.agegroup.female.lm$AIS) <- c("AIS-A", "AIS-B ", "AIS-C", "AIS-D", " ", "")

estimate.plot.female<- ggplot(ais_ratios.by.agegroup.female.lm, aes(y = estimate, x = AIS)) +
  geom_pointrange(aes(ymin = `CI low`, ymax = `CI high`),
                  position = dodger,
                  size = 0.5) + 
 facet_wrap(.~ais_ratios.by.agegroup.female.lm$agegr, ncol = 1)+geom_hline(aes(yintercept=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), color='red', linetype=2)+
  scale_y_continuous( limits = c(-3, 3.5), breaks = seq(-3, 3.5, 1), expand = c(0,0))+
  labs(y = "Estimates", x = "", title="Female") +
  coord_flip() +
  theme_economist(horizontal = FALSE)+scale_color_economist()+
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.48, size=10),
        axis.title.x = element_text(size=10, face = 'bold'),
        axis.title.y = element_text(size=10, face = 'bold', hjust=0.4),
        strip.text = element_text(size=10, face = 'italic'))

#####Calculate ratios of AIS grades per age group per year for male subjects
ais_ratios.by.agegroup.male= subset(emsci.ais, Sex=='m') %>%
  count(AIS, YEARDOI, agegr) %>%
  group_by(agegr, YEARDOI)%>%
  mutate(frequency = (n / sum(n))*100)


#Fit regression model with the proportion of AIS grade as the response, and time as the predictor
ais_ratios.by.agegroup.male.lm<-ais_ratios.by.agegroup.male %>%
  group_by(AIS,agegr) %>%         
  do(tidy(lm(frequency~YEARDOI, data=.))%>%
       mutate(
         'CI low' = estimate - std.error,
         'CI high' = estimate + std.error
       )
  ) 

ais_ratios.by.agegroup.male.lm <- ais_ratios.by.agegroup.male.lm[c(2,4,6,8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40),]

levels(ais_ratios.by.agegroup.male.lm$agegr) <- c("below 20 years","20-30 years", "31-50 years ", "51-69 years", "70+ years")
levels(ais_ratios.by.agegroup.male.lm$AIS) <- c("AIS-A", "AIS-B ", "AIS-C", "AIS-D", " ", "")

estimate.plot.male<- ggplot(ais_ratios.by.agegroup.male.lm, aes(y = estimate, x = AIS)) +
  geom_pointrange(aes(ymin = `CI low`, ymax = `CI high`),
                  position = dodger,
                  size = 0.5) +
  facet_wrap(vars(ais_ratios.by.agegroup.male.lm$agegr), ncol = 1) +geom_hline(aes(yintercept=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), color='red', linetype=2)+
  scale_y_continuous( limits = c(-3, 3.5), breaks = seq(-3, 3.5, 1), expand = c(0,0))+
  labs(y = "Estimates", x = " ", title="Male") +
  coord_flip() +
  theme_economist(horizontal = FALSE)+scale_color_economist()+
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.48, size=10),
        axis.title.x = element_text(size=10, face = 'bold'),
        axis.title.y = element_blank(),
        strip.text = element_text(size=10, face='italic'))

#Combine plots
grid.arrange(estimate.plot.female,
             estimate.plot.male,
             widths=c(0.4,0.4),
             ncol=2)


#------Temporal progression of lems over 20 years ---------------------

#Only include subjects with information of sex and AIS grades
emsci.ais <- subset(emsci.trauma.sex, AIS=="A"| AIS=="B"| AIS=="C"| AIS=="D")

emsci.ais.long <- gather(emsci.ais, outcome, value, RUEMS:SCIM3_17, factor_key=TRUE)
emsci.ais.long

#emsci.ais <- subset(emsci.ais.long, AIS=="D")


for (j in levels(emsci.ais$AIS)){ 
  for (i in unique(emsci.ais$YEARDOI_new)){ 
  print(paste("Year: ", i, " AIS score: ",j,sep = ""))
  df1 = subset(emsci.ais, YEARDOI_new == i & AIS==j)
  model<- lm(as.numeric(UEMS)~ExamStage_weeks+NLI_level+Sex, data = df1, na.action=na.exclude )
  print(summary(model)$coef)
  zw = as.data.frame(summary(model)$coef)
  zw$YEAR=i
  zw$AIS=j
  if(j=="A" & i==2002){
    zw1=zw
  }else{
    zw1=rbind(zw1,zw)
  }
  }
}


x <- zw1 %>% slice(grep("ExamStage_weeks", row.names(.)))
y <- zw1 %>% slice(grep("Intercept", row.names(.)))


ggplot(x, aes(YEAR, Estimate))+geom_point()+facet_wrap(~AIS, ncol=2)+theme_bw()+
  scale_x_continuous(breaks = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020))+
  ggtitle("LEMS")+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank())

m1 <- lm(Estimate~YEAR*AIS, data=x)

###Upper extremity motor score
model1<- lm(as.numeric(TMS)~YEARDOI*ExamStage_weeks+NLI_level+Sex+AgeAtDOI, data=emsci.ais)
summary(m1)


library(nlme)
time.linear <- lm(as.numeric(TMS)~YEARDOI*ExamStage_weeks,
                                data=emsci.ais)

tms.lm<-subset(emsci.ais, YEARDOI=="2015") %>%
  group_by(AIS) %>%         
  do(tidy(lmer(as.numeric(TMS)~ExamStage_weeks+(1|X...Patientennummer), data=.))%>%
       mutate(
         'CI low' = estimate - std.error,
         'CI high' = estimate + std.error
       )
  ) 

summary(time.linear)




#Plot
ggplot(subset(emsci.ais, NLI_level=='Cervical'),aes(x=newtimeline, y=as.numeric(TMS), group=AIS)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="black", size=0.5) +
  # shadow common to all facets
  facet_grid(emsci.ais$AIS~.)+
  # geom_vline(aes(xintercept=c(52)), color='red', linetype=2)+
  # geom_vline(aes(xintercept=c(104)), color='red', linetype=2)+
  # geom_vline(aes(xintercept=c(156)), color='red', linetype=2)+
  # geom_vline(aes(xintercept=c(208)), color='red', linetype=2)+
  # geom_vline(aes(xintercept=c(260)), color='red', linetype=2)+
  # geom_vline(aes(xintercept=c(312)), color='red', linetype=2)+
  # geom_vline(aes(xintercept=c(364)), color='red', linetype=2)+
  # geom_vline(aes(xintercept=c(416)), color='red', linetype=2)+
  # geom_vline(aes(xintercept=c(468)), color='red', linetype=2)+
  # geom_vline(aes(xintercept=c(520)), color='red', linetype=2)+
  xlab("") + ylab("Total Motor Score")+
theme_minimal()+theme(axis.text.x = element_blank()) +
  grid.rect(x = 156, y = 0,
            width = 52, height = 1,
            just = c("left", "bottom"), default.units = "native",
            gp = gpar(fill = "blue", col = "transparent", alpha = .2))
  
  
  
  geom_rect(data=emsci.ais, aes(x = NULL,y = NULL,xmin=156, xmax=208,
                           ymin=-Inf, ymax=+Inf), fill='blue', alpha=0.002)
          




#Plot
ggplot(emsci.ais,aes(x=newtimeline, y=as.numeric(TMS))) +
  # geom_rect(data=emsci.ais, aes(x = NULL,y = NULL,xmin=156, xmax=208,
  #                               ymin=-Inf, ymax=+Inf), fill='blue', alpha=0)+
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE) +
  facet_grid(emsci.ais$AIS~.)+
  xlab("") + ylab("Light touch")+
  theme_economist(horizontal = FALSE)+  scale_x_continuous( limits = c(0, 910), breaks = seq(0, 910, 52), expand = c(0,0))






#Number of patients per year and exam stage
emsci.ais2 <-as.data.frame(emsci.ais  %>%
  count(YEARDOI, ExamStage, sort = TRUE))

library(table1)

emsci.ais$TMS <-as.numeric(emsci.ais$TMS)
emsci.ais$UEMS <-as.numeric(emsci.ais$UEMS)
emsci.ais$LEMS <-as.numeric(emsci.ais$LEMS)
emsci.ais$AgeAtDOI <-as.numeric(emsci.ais$AgeAtDOI)

table1::label(emsci.ais$TMS) <- "Total Motor Score"
table1::label(emsci.ais$UEMS) <- "Upper Extremity Motor Score"
table1::label(emsci.ais$LEMS) <- "Lower Extremity Motor Score"
table1::label(emsci.ais$AgeAtDOI) <- "Age at Injury"

table1::table1(~ Sex+Cause+AIS+UEMS+LEMS+TMS+AgeAtDOI| YEARDOI*ExamStage, data = emsci.ais)




library(data.table)

setDT(emsci.ais)[ , .(count=unique(emsci.ais$Patientennummer)) , by = .(YEARDOI)]


ggplot(df[df$year>2004,],aes(x=year, y=length)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "smooth", se = TRUE) +
  xlab("") + ylab("Report Length") +
  scale_x_continuous(breaks = seq(2005, max(df$year), by = 2))


