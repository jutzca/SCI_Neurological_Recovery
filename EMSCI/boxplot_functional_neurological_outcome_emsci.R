## ---------------------------
##
## Script name: 9_boxplot_functional_neurological_outcome_emsci
##
## Purpose of script: To visualize the baseline, 52 weeks, and change score of functional and neurological scores
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2020-11-23
##
## Copyright (c) Catherine Jutzeler, 2020
## Email: catherine.jutzeler@bsse.ethz.ch
##
## ---------------------------
##
## Data source: European Multi-center Study about Spinal Cord Injury
##
## Notes: Code for the publication XXX
##   
#### ---------------------------

## set working directory for Mac and PC

setwd("/Users/jutzelec/Documents/GitHub/SCI_Neurological_Recovery/EMSCI") 

## ---------------------------
## load up the packages we will need:  (uncomment as required)
library(lme4)
library(sjPlot) #To creats tables
library(jtools)#To creats tables
library(ggplot2) #To creats graphs
library(ggridges) #To creats graphs
library(ggpubr) #To creats graphs
library(plyr)
library(dplyr)
library(tidyr)
library('ggthemes') #Themes for the plots
library(Hmisc)
library(scales)  #To recale the data
library(splitstackshape) #To format the model output to a table
library(lmerTest) #To run the mixed effect models
library(data.table)
library(magrittr)
library(gridExtra)
library(grid)

## ----------------------------
## Install packages needed:  (uncomment as required)

#if(!require(lme4)){install.packages("lme4")}
#if(!require(sjPlot)){install.packages("sjPlot")}
#if(!require(jtools)){install.packages("jtools")}
#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(ggridges)){install.packages("ggridges")}
#if(!require(ggpubr)){install.packages("ggpubr")}
#if(!require(plyr)){install.packages("plyr")}
#if(!require(dplyr)){install.packages("dplyr")}
# if(!require(tidyr)){install.packages("tidyr")}
# if(!require(ggthemes)){install.packages("ggthemes")}
# if(!require(Hmisc)){install.packages("Hmisc")}
# if(!require(scales)){install.packages("scales")}
# if(!require(splitstackshape)){install.packages("splitstackshape")}
# if(!require(lmerTest)){install.packages("lmerTest")}


#### ---------------------------
#Set output directorypaths
outdir_figures='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Figures'
outdir_tables='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables'


#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

#load original dataset
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T)

#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, level of injury either cervical, thoracic, or lumbar,
#and AIS score A, B, C, or D
emsci.trauma.sex <- subset(emsci, (AgeAtDOI > 8) & (Sex=='f' | Sex=='m') & 
                             (Cause=="ischemic" | Cause=="traumatic" | Cause=="haemorragic" |Cause=="disc herniation") & 
                             (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar') & (YEARDOI >= 2000) &(AIS=='A' | AIS=="B"| AIS=="C"| AIS=="D"))



library(dplyr)

emsci.trauma.sex$SCIM23_TotalScore <- as.numeric(emsci.trauma.sex$SCIM23_TotalScore)
new.data =emsci.trauma.sex %>%
  group_by(plegia, AIS,X5_year_bins,ExamStage_weeks ) %>%
  dplyr::summarize( 
    n = n(),
    mean_SCIM23_TotalScore = mean(SCIM23_TotalScore, na.rm=TRUE),
    sd_SCIM23_TotalScore = sd(SCIM23_TotalScore, na.rm=TRUE))



#To reverse the order of levels of AIS
new.data$AIS <- factor(new.data$AIS, levels=rev(levels(new.data$AIS)))

#Round values to 1 digits
new.data$mean_SCIM23_TotalScore <- round(new.data$mean_SCIM23_TotalScore, 1)

#------Baseline LEMS vs 52 weeks LEMS
library(forcats)
ggplot(new.data,aes(x = as.factor(ExamStage_weeks),y = AIS,fill = mean_SCIM23_TotalScore)) + 
  geom_tile()+scale_fill_viridis(option = "viridis",  direction = -1, limits = c(0, 100))+
  facet_grid(as.factor(X5_year_bins)~plegia)+theme_minimal()+
  geom_text(aes(label=as.numeric(mean_SCIM23_TotalScore)), size=3, color='white')

  