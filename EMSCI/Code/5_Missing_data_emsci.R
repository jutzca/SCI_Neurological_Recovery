## ---------------------------
##
## Script name: Longitudinal_analysis_sygen
##
## Purpose of script: To determine if the neurological and functional recovery changed over the course of the EMSCI study.
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2020-11-28
##
## Copyright (c) Catherine Jutzeler, 2020
## Email: catherine.jutzeler@bsse.ethz.ch
##
## ---------------------------
##
## Data source: European Multicenter Study about Spinal Cord Injury (2001-20019)
##
## Notes: Code for the publication XXXX et al., 2021
##   
#### ---------------------------

## set working directory

setwd("/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI")

## ---------------------------
## load up the packages we will need:  
library(devtools)
library(table1)
library(dplyr)
library(naniar)
library(ggplot2)
library("ggthemes") 
library(tidyverse)
library(ggpubr)
library(Hmisc)
library(naniar)
library(finalfit)
library(visdat)
library(mice)


## ----------------------------
## Install packages needed:  (uncomment as required)

# if(!require(table1)){install.packages("table1")}
# if(!require(dplyr)){install.packages('dplyr')}
# if(!require(naniar)){install.packages(('naniar'))}
# if(!require(ggplot2)){install.packages("ggplot2")}
# if(!require(ggthemes)){install.packages("ggthemes")}
# if(!require(tidyverse)){install.packages('tidyverse')}
# if(!require(mvnmle)){install.packages('finalfit')}
# if(!require(ggpubr)){install.packages("ggpubr")}
# if(!require(Gally)){install.packages("Gally")}
# if(!require(visdat)){install.packages("visdat")}
# if(!require(mice)){install.packages("mice")}


#### ---------------------------
#Set output directorypaths
outdir_figures='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Figures'
outdir_tables='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables'

#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

#-------------------------Data wrangling-----
#load original dataset
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T,  na.strings=c("","NA"))

#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
emsci <- subset(emsci, (AgeAtDOI > 8) & (Sex=='f' | Sex=='m') & ###Age at DOI and Sex
                             (Cause=="ischemic" | Cause=="traumatic" | Cause=="haemorragic" |Cause=="disc herniation") & 
                             (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar')&   ## Neurological level
                             (AIS=="A"| AIS=="B"| AIS=="C"| AIS=="D")) #AIS Grades




#Visualize missingness in the masterfile
##https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html

#Subset Data
emsci_subset <- emsci[,c(2,3,4,5,8,9,11,12,16,18,20,23,26,29,32,35,36,178,179,186,188,190,192,196,216)] 

#Subset data to most important variables
#E.g., total scores of SCIM serve as proxy for the subscores. The rationale for that arises from the fact, that the total scores can not be
#calculated if the subscores have not been assessed. Same applies for LT, PP, LEMS, UEMS, TMS

emsci_subset_newnames<-emsci_subset %>% 
  rename(
    'Neurological level of Injury' = NLI,
    #"Time up and go" = TUG,
    "10m walking test" =X10m,
   '6min walking test' = X6min,
   'Plegia' =plegia,
    "Total pin prick" =TPP,
    "Total light touch" = TLT,
    "Lower extremity motor score" = LEMS,
    "Upper extremity motor score" = UEMS,
    "Total motor score" = TMS,
    "Age at Injury" = AgeAtDOI,
    "Year of Injury" = YEARDOI,
    "PID" = Patientennummer,
    'Combined Total Score SCIM2 and 3'= SCIM23_TotalScore,
   'SCIM2 Total Score' = SCIM2_TotalScore,
   'SCIM3 Total Score' = SCIM3_TotalScore,
   'Walking Index for Spinal Cord Injury' = WISCI,
   'Voluntary anal contraction' = VAC,
     'Deep anal pressure' =DAP)

#Initial visualization of missing data
vis_miss(emsci_subset_newnames, sort_miss = T)

#Visualization of patter in missing data
pattern_missing_data.emsci <- gg_miss_upset(emsci_subset_newnames,  nsets = 50,
              nintersects = 20)
pattern_missing_data.emsci


#Visualize the missing data by exam stage
missing_data_examstage<-gg_miss_var(emsci_subset_newnames, facet=ExamStage, show_pct = TRUE)+
  scale_y_continuous(labels = abs, limits = c(0, 100), breaks = seq(0, 100, 10))+
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "", y = "% missing values")+ #ggtitle("% missing values, by exam stage and variable")+
  theme(axis.title = element_text(size = 10, face = 'bold'), 
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size=10),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        strip.text = element_text(size =10)
  )
missing_data_examstage

ggsave(
  "missing_data_examstage.emsci.pdf",
  plot = missing_data_examstage,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 10,
  height = 9,
  units = "in",
  dpi = 300
)


dev.off()

#Visualize the missing data by ais grades
missing_data_ais<-gg_miss_var(emsci_subset_newnames, facet=AIS, show_pct = TRUE)+
  scale_y_continuous(labels = abs, limits = c(0, 100), breaks = seq(0, 100, 10))+
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "", y = "% missing values")+ #ggtitle("% missing values, by exam stage and variable")+
  theme(axis.title = element_text(size = 10, face = 'bold'), 
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size=10),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        strip.text = element_text(size =10)
  )
missing_data_ais

ggsave(
  "missing_data_ais_emsci.pdf",
  plot = missing_data_ais,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 9,
  height = 10,
  units = "in",
  dpi = 300
)

dev.off()

#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

