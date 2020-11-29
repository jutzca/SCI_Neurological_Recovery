## ---------------------------
##
## Script name: 7_Annual_incidence_figure_sygen
##
## Purpose of script: To determine the annual incidence of SCI between 1992 and 1997
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2020-11-19
##
## Copyright (c) Catherine Jutzeler, 2020
## Email: catherine.jutzeler@bsse.ethz.ch
##
## ---------------------------
##
## Data source: Sygen Clinical Trial
##
## Notes: Code for the publication XXX
##   
#### ---------------------------
## Clear working space

rm(list=ls())

## set working directory for Mac

setwd("/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen") 

## ---------------------------
## load up the packages we will need:  (uncomment as required)
library(easyGgplot2)
library(ggthemes)
library(dplyr)

## ----------------------------
## Install packages needed:  (uncomment as required)

#if(!require(easyGgplot2)){install.packages("easyGgplot2")}
#if(!require(ggthemes)){install.packages("ggthemes")}
#if(!require(dplyr)){install.packages("dplyr")}

#### ---------------------------
#Set output directorypaths
outdir_figures='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen/Figures'
outdir_tables='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen/Tables'


#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

#load original dataset
sygen<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/df_sygen_formatted.csv", sep = ',', header = T,  na.strings=c("","NA"))

#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
sygen.included.cohort.all.times<- subset(sygen, (!is.na(Age)) & (Sex=="Female" | Sex=="Male") & ###Age at DOI and Sex
                                           (NLI == 'cervical' | NLI == 'thoracic')&   ## Neurological level
                                           (AIS=="AIS A"| AIS=="AIS B"| AIS=="AIS C"| AIS=="AIS D")) #AIS Grades

sygen_incidence <- distinct(subset(sygen.included.cohort.all.times, Time==0 | Time==1), ID, .keep_all = TRUE)




#Calculate the annual incidence
value.sygen <-as.data.frame(sygen_incidence%>%
                        count(YEARDOI))


#Plot the annual incidence
annual.incidence.sygen <- ggplot2.barplot(data=value.sygen, xName="YEARDOI", yName='n',
                                          width=0.9, color="black")+
  geom_text(aes(label=n), vjust=-0.12, color="black", size=3)+
  scale_y_continuous(expand = c(0.1,0)) +
  scale_x_discrete(breaks = c(2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019), expand=c(0,0) )+ 
  geom_hline(yintercept = 0) +
  #theme_economist(horizontal = FALSE) +
  labs(fill = "", x = "Year of Injury", y = "Number of Patients")+ ggtitle("Annual incidence (Sygen)")+
  theme(axis.title = element_text(size = 13, face = 'bold'), 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
  )
annual.incidence.sygen

#Save Plot
ggsave(
  "annual.incidence.sygen.pdf",
  plot = annual.incidence.sygen,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 4,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()




#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####




