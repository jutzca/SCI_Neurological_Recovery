## ---------------------------
##
## Script name: 18_EMSCI_vs_Sygen
##
## Purpose of script: To compare the EMSCI and Sygen Cohorts
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2020-03-14
##
## Copyright (c) Catherine Jutzeler, 2022
## Email: catherine.jutzeler@hest.ethz.ch
##
## ---------------------------
##
## Data source: European Multi-center Study about Spinal Cord Injury
##
## Notes: Code for the publication XXX
##   
#### ---------------------------
# R Studio Clean-Up:
cat("\014") # clear console
rm(list=ls()) # clear workspace
gc() # garbage collector

#### ---------------------------
## set working directory

setwd("/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI") 

## ---------------------------
## load up the packages we will need:  (uncomment as required)
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


#### ---------------------------
#Set output directorypaths
outdir_figures='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Figures'
outdir_tables='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables'

#-------------------------Data wrangling------------------------------------------------------------------------------------------------------

#load original dataset
emsci<- read.csv("/Volumes/jutzelec/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T,  na.strings=c("","NA"))


#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
emsci.trauma.sex <- subset(emsci, (AgeAtDOI > 8) & (Sex=='f' | Sex=='m') & ###Age at DOI and Sex
                             (Cause=="ischemic" | Cause=="traumatic" | Cause=="haemorragic" |Cause=="disc herniation") & 
                             (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar')&   ## Neurological level
                             (AIS=="A"| AIS=="B"| AIS=="C"| AIS=="D")) #AIS Grades

#-------------------------Data analysis and visualization------------------------------------------------------------------------------------------------------

#------Data analysis and creation of table:  EMSCI cohort------

#Subset data to only patients with valid entry at stage 'very acute' or 'acute I' and remove duplicate patient numbers
emsci.trauma.sex.va.a1<-distinct(subset(emsci.trauma.sex, ExamStage=='acute I' | ExamStage=='very acute') , Patientennummer, .keep_all = TRUE)

#relevel cause and injury level of injury
emsci.trauma.sex.va.a1$Cause <- factor(emsci.trauma.sex.va.a1$Cause, levels=c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
emsci.trauma.sex.va.a1$NLI_level <- factor(emsci.trauma.sex.va.a1$NLI_level, levels=c("cervical", "thoracic", "lumbar", "sacral"))


#------Data analysis and creation of table:  Sygen cohort------

# Load original dataset
sygen<- read.csv("/Volumes/jutzelec/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/df_sygen_formatted.csv", sep = ',', header = T,  na.strings=c("","NA"))

# Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
sygen.included.cohort.all.times<- subset(sygen, (!is.na(Age)) & (Sex=="Female" | Sex=="Male") & ###Age at DOI and Sex
                                           (NLI == 'cervical' | NLI == 'thoracic')&   ## Neurological level
                                           (AIS=="AIS A"| AIS=="AIS B"| AIS=="AIS C"| AIS=="AIS D")) #AIS Grades

sygen.included.cohort <- distinct(subset(sygen.included.cohort.all.times, Time==0 | Time==1), ID, .keep_all = TRUE)


table1::table1(~ Sex+Age+AIS+NLI, data = sygen.included.cohort)


#sex
prop.test(x=c(143,560), n=c(1059, 3542),
          conf.level=0.95)


#injury characteristics - AIS Score
prop.test(x=c(446,77,149,31), n=c(1818, 522, 858, 1373),
          conf.level=0.95)

#injury characteristics - NLI
prop.test(x=c(540,163,0), n=c(2438, 1643, 520),
          conf.level=0.95)

#age
sygen.included.cohort$status <- 'sygen cohort'
emsci.trauma.sex.va.a1$status <- 'emsci cohort'
merged_data_emsci_sygen<-rbind.fill(sygen.included.cohort,emsci.trauma.sex.va.a1)

#Welch Two Sample t-test
t.test(AgeAtDOI ~ status, data = merged_data)