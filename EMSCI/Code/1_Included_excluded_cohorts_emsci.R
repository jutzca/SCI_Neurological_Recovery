## ---------------------------
##
## Script name: Included_excluded_cohorts_emsci
##
## Purpose of script: To create and describe the included and excluded EMSCI cohorts
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
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T,  na.strings=c("","NA"))


#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
emsci.trauma.sex <- subset(emsci, (AgeAtDOI > 8) & (Sex=='f' | Sex=='m') & ###Age at DOI and Sex
                             (Cause=="ischemic" | Cause=="traumatic" | Cause=="haemorragic" |Cause=="disc herniation") & 
                             (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar')&   ## Neurological level
                             (AIS=="A"| AIS=="B"| AIS=="C"| AIS=="D")) #AIS Grades

#-------------------------Data analysis and visualization------------------------------------------------------------------------------------------------------

#------Data analysis and creation of table: Included EMSCI cohort------

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
label(emsci.trauma.sex.va.a1$AIS) <- "AIS Score"

#Assign units to Age at Injury and Year of Injury
units(emsci.trauma.sex.va.a1$AgeAtDOI) <- "years"
units(emsci.trauma.sex.va.a1$YEARDOI) <- "years"

#Print table
table1::table1(~ Sex+AgeAtDOI+Cause+AIS+NLI_level, data = emsci.trauma.sex.va.a1)

#------Data analysis, creation of table, and figure: Patients enrolled in EMSCI per year and country------
#Create Supplemantray Table 1: Number of patients enrolled per country (5 year bins)
table1::table1(~ Country | X5_year_bins, data = emsci.trauma.sex.va.a1)

###Calculate Number of patients enrolled per year
nr.of.patients.enrolled.per.year = emsci.trauma.sex.va.a1%>%
  dplyr::count(YEARDOI) %>%
  dplyr::group_by(YEARDOI)
nr.of.patients.enrolled.per.year

# Calculate average recruitment per year 
ave.recruitment <-mean(nr.of.patients.enrolled.per.year$n)
sd.recruitment <-sd(nr.of.patients.enrolled.per.year$n)


sort(nr.of.patients.enrolled.per.year$n)

##Plot number of patients enrolled per year
patients_enrolled_per_year <-ggplot(nr.of.patients.enrolled.per.year, aes(as.factor(YEARDOI),as.numeric(n), fill=n))+  geom_bar(stat="identity")+
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
patients_enrolled_per_year

##Save plot
ggsave(
  "patients_enrolled_per_year.emsci.pdf",
  plot = patients_enrolled_per_year,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 4,
  height = 3,
  units = "in",
  dpi = 300
)

dev.off()


###Calculate Number of patients enrolled per country
nr.of.patients.enrolled.per.country = emsci.trauma.sex.va.a1%>%
  count(Country) %>%
  group_by(Country)
nr.of.patients.enrolled.per.country

sort(nr.of.patients.enrolled.per.country$n)

##Plot number of patients enrolled per country
patients_enrolled_per_country <-ggplot(nr.of.patients.enrolled.per.country, aes(as.factor(Country),as.numeric(n), fill=n))+  geom_bar(stat="identity")+
  geom_text(aes(label=n), vjust=-0.3, size=3.5)+
  theme_economist()+
  theme(legend.position = "none",
        axis.title.y= element_blank(),
        axis.text.y= element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45))+
  labs(x= "Country", y="Number of patients enrolled")
patients_enrolled_per_country

##Save plot
ggsave(
  "patients_enrolled_per_country.emsci.pdf",
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


#------Comparison between included and exlcuded EMSCI cohorts------

#sex
prop.test(x=c(141,477), n=c(1059, 3542),
          conf.level=0.95)


#injury characteristics
prop.test(x=c(137,35,54,140), n=c(1818, 522, 858, 1373),
          conf.level=0.95)

#age
emsci.missing.unique$status <- 'excluded cohort'
emsci.trauma.sex.va.a1$status <- 'included cohort'
merged_data<-rbind(emsci.missing.unique,emsci.trauma.sex.va.a1)

#Welch Two Sample t-test
t.test(AgeAtDOI ~ status, data = merged_data)

#Here's a quick visualization of the difference:
ggplot(merged_data, aes(x=AgeAtDOI, fill = status)) + 
  geom_histogram(alpha = .5, bins = 20, position = "identity") + 
  theme_classic()

#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

