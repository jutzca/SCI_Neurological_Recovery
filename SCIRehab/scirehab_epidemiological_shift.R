##### Code created by C. Jutzeler August 3rd, 2020
##### Validation Cohort: scirehab trial

#Clear workspace
rm(list = ls())

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

#where libraries are stored
.libPaths()

#paths
outdir_figures='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/SCIRehab/Figures'
outdir_tables='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/SCIRehab/Tables'


#-------------------------Data wrangling------------------------------------------------------------------------------------------------------

#load original dataset
scirehab<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/df_rehab_formatted.csv", sep = ',', header = T,  na.strings=c("","NA"))

#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
scirehab.included.cohort<- subset(scirehab, (Sex=="Female" | Sex=="Male") & ###Age at DOI and Sex
                                 (NLI == 'cervical' | NLI == 'thoracic' | NLI == 'lumbar')&   ## Neurological level
                                 (AIS=="AIS A"| AIS=="AIS B"| AIS=="AIS C"| AIS=="AIS D")) #AIS Grades



#-------------------------Data analysis and visualization of ------------------------------------------------------------------------------------------------------

#------Data analysis and creation of table: Included cohort------

#Subset data to only patients with valid entry at stage 'very acute' or 'acute I' and remove duplicate patient numbers
scirehab.included.cohort.admission<-distinct(subset(scirehab,Time=='admission') , ID, .keep_all = TRUE)

scirehab.included.cohort.admission<- subset(scirehab.included.cohort.admission, (Sex=="Female" | Sex=="Male") & ###Age at DOI and Sex
                                    (NLI == 'cervical' | NLI == 'thoracic' | NLI == 'lumbar')&   ## Neurological level
                                    (AIS=="AIS A"| AIS=="AIS B"| AIS=="AIS C"| AIS=="AIS D")) #AIS Grades

#relevel cause and injury level of injury
scirehab.included.cohort.admission$Cause <- factor(scirehab.included.cohort.admission$Cause, levels=c("automobile", "fall", "gun shot wound", 'motorcycle', "pedestrian", "person-to-person contact", 'water related', "other sports", "others"))
scirehab.included.cohort.admission$NLI <- factor(scirehab.included.cohort.admission$NLI, levels=c("cervical", "thoracic", "lumbar"))


#Create Summary Table
#Formatting of table: Customize levels, labels, and units of listed variables

#Change names of levels of variables
levels(scirehab.included.cohort.admission$NLI) <- c("Cervical", "Thoracic", "Lumbar")
levels(scirehab.included.cohort.admission$Cause) <- c("Automobile", "Fall", "Gunshot wound", 'Motorcycle', "Pedestrian", "Person-to-person contact", 'Water related',  "Other sports", "Others")
levels(scirehab.included.cohort.admission$Sex) <- c("Female", "Male")
levels(scirehab.included.cohort.admission$AIS) <- c("A", "B", "C", "D")
levels(scirehab.included.cohort.admission$NLI) <- c("Cervical", "Thoracic", "Lumbar")

#Change label of variables
label(scirehab.included.cohort.admission$yeardoi) <- "Year of injury"
label(scirehab.included.cohort.admission$AIS) <- "AIS Score"
label(scirehab.included.cohort.admission$NLI) <- "Neurological level of injury"

#Assign units to Age at Injury and Year of Injury
units(scirehab.included.cohort.admission$Age) <- "years"
units(scirehab.included.cohort.admission$yeardoi) <- "years"

#Print table
table1::table1(~ Sex+Age+AIS+Cause+NLI, data = scirehab.included.cohort.admission)
