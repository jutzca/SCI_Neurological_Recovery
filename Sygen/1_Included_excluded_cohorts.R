## ---------------------------
##
## Script name: Included_excluded_cohorts_sygen
##
## Purpose of script: To create and describe the included and excluded Sygen cohorts
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2020-11-20
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
## ---------------------------
## 
## load up the packages we will need: 
## 
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
## 
## ----------------------------
## 
## Install packages needed:  (uncomment as required)
## 
# if(!require(lme4)){install.packages("lme4")}
# if(!require(sjPlot)){install.packages("sjPlot")}
# if(!require(jtools)){install.packages("jtools")}
# if(!require(ggplot2)){install.packages("ggplot2")}
# if(!require(ggridges)){install.packages("ggridges")}
# if(!require(ggpubr)){install.packages("ggpubr")}
# if(!require(plyr)){install.packages("plyr")}
# if(!require(dplyr)){install.packages("dplyr")}
# if(!require(gridExtra)){install.packages("gridExtra")}
# if(!require(reshape2)){install.packages("reshape2")}
# if(!require(PMCMRplus)){install.packages("PMCMRplus")}
# if(!require(naniar)){install.packages("naniar")}
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
##
#### ---------------------------
##
## R Studio Clean-Up:
cat("\014") # clear console
rm(list=ls()) # clear workspace
gc() # garbage collector
##
#### ---------------------------
##
## Set working directory 
setwd("/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen") 
##
#### ---------------------------
##
## Set output directorypaths
outdir_figures='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen/Figures'
outdir_tables='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen/Tables'
##
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####
# Load original dataset
sygen<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/Sygen.csv", sep = ',', header = T,  na.strings=c("","NA"))

# Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
sygen.included.cohort<- subset(sygen, (age > 0) & (sex=="female" | sex=="male") & ###Age at DOI and Sex
                                 (splvl1 == 'C' | splvl1 == 'T')&   ## Neurological level
                                 (ais1=="AIS A"| ais1=="AIS B"| ais1=="AIS C"| ais1=="AIS D")) #AIS Grades


# Create Summary Table
# Formatting of table: Customize levels, labels, and units of listed variables
# Change names of levels of variables
levels(sygen.included.cohort$sex) <- c("Female", "Male")
levels(sygen.included.cohort$ais1) <- c("A", "B", "C", "D")
levels(sygen.included.cohort$splvl1) <- c("Cervical", "Thoracic")

# Relable variables
label(sygen.included.cohort$sex) <- "Sex"
label(sygen.included.cohort$age) <- "Age"
# Label(emsci.trauma.sex.va.a1$NLI_level)<- "Neurological level of injury"
label(sygen.included.cohort$yeardoi) <- "Year of injury"
label(sygen.included.cohort$ais1) <- "AIS"
label(sygen.included.cohort$splvl1) <- "Neurological level of injury"

# Assign units to Age at Injury and Year of Injury
units(sygen.included.cohort$age) <- "years"
units(sygen.included.cohort$yeardoi) <- "years"

# Print table
table1::table1(~ sex+age+ais1+splvl1 | yeardoi, data = sygen.included.cohort)

#---------- Data analysis and creation of table: Excluded cohort --------#
# Summary Table: Excluded cohort
# Extract excluded cohort
sygen.missing<-anti_join(sygen,sygen.included.cohort, by ="ptid")
sygen.missing.unique<-distinct(sygen.missing, ptid, .keep_all = TRUE)

# Formatting of table: Customize levels, labels, and units of listed variables
# Change names of levels of variables
levels(sygen.missing.unique$sex) <- c("Female", "Male")
levels(sygen.missing.unique$ais1) <- c("A", "B", "C", "D")
levels(sygen.missing.unique$splvl1) <- c("Cervical", "Thoracic")

# Relable variables
label(sygen.missing.unique$sex) <- "Sex"
label(sygen.missing.unique$age) <- "Age"

# label(emsci.trauma.sex.va.a1$NLI_level)<- "Neurological level of injury"
label(sygen.missing.unique$yeardoi) <- "Year of injury"
label(sygen.missing.unique$ais1) <- "AIS"
label(sygen.missing.unique$splvl1) <- "Neurological level of injury"

# Assign units to Age at Injury and Year of Injury
units(sygen.missing.unique$age) <- "years"
units(sygen.missing.unique$yeardoi) <- "years"

# Print table
table1::table1(~ sex+age+ais1+splvl1, data = sygen.missing.unique)

#---------- Comparison between included and exlcuded Sygen cohorts --------#
# Sex
prop.test(x=c(11,83), n=c(143, 560),
          conf.level=0.95)

# Age
sygen.missing.unique$status <- 'excluded cohort'
sygen.included.cohort$status <- 'included cohort'
merged_data.sygen<-rbind(sygen.missing.unique,sygen.included.cohort)

# Welch Two Sample t-test
t.test(age ~ status, data = merged_data.sygen)

# Here's a quick visualization of the difference:
ggplot(merged_data.sygen, aes(x=age, fill = status)) + 
  geom_histogram(alpha = .5, bins = 20, position = "identity") + 
  theme_classic()


#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

