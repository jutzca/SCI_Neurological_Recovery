#-------------------------Assess missigness of the data------------------------------------------------------------------------------------------------------
#Code written by C. Jutzeler, March 20th, 2020

#Clear workspace
rm(list = ls())

#Install r packages as required for analyses and plots
if(!require(table1)){install.packages("table1")}
if(!require(dplyr)){install.packages('dplyr')}
if(!require(naniar)){install.packages(('naniar'))}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(ggthemes)){install.packages("ggthemes")}
if(!require(BaylorEdPsych)){install.packages("BaylorEdPsych")}
if(!require(tidyverse)){install.packages('tidyverse')}
if(!require(mvnmle)){install.packages('mvnmle')}
if(!require(ggpubr)){install.packages("ggpubr")}

#Load libraries
library(table1)
library(dplyr)
library(naniar)
library(ggplot2)
library("ggthemes") 
library(BaylorEdPsych)
library(tidyverse)
library(mvnmle)
library(ggpubr)

#load original dataset
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T,  na.strings=c("","NA"))

#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar
emsci.trauma.sex <- subset(emsci, (AgeAtDOI > 8) & (Sex=='f' | Sex=='m') & 
                             (Cause=="ischemic" | Cause=="traumatic" | Cause=="haemorragic" |Cause=="disc herniation") & 
                             (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar') & (YEARDOI >= 2000))

#Subset data to only patients with valid entry at stage 'very acute' or 'acute I' and remove duplicate patient numbers
emsci.trauma.sex.va.a1<-distinct(subset(emsci.trauma.sex, ExamStage=='acute I' | ExamStage=='very acute'), Patientennummer, .keep_all = TRUE)

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
table1::table1(~ Sex+Cause+AIS+NLI_level+AgeAtDOI+YEARDOI, data = emsci.missing.unique)

#Visualize missingness in the masterfile
##https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html

emsci$ExamStage<-factor(emsci$ExamStage, c("very acute", "acute I", "acute II", "acute III", "chronic"))

emsci2<-subset(emsci, ExamStage=='acute I' | ExamStage=='very acute')
newdata <- emsci[,c(1,4,7,8, 10,11,15, 18, 21, 26, 27, 30, 33, 185, 187, 189,191 )] #column 1:20

#Test if data is missing at Completely At Random 
MCARvsMAR<-LittleMCAR(newdata)
view(MCARvsMAR)



vis_miss(newdata)


newdata2<-newdata %>% 
  rename(
    "6-Minute test" = X6min,
    'Neurological level of Injury' = NLI_level,
    "Time up and go" = TUG,
    "10m walking test" =X10m,
    "Total pin prick" =TPP,
    "Total light touch" = TLT,
    "Lower extremity motor score" = LMS,
    "Upper extremity motor score" = UEMS,
    "Total motor score" = TMS,
    "AIS" = AIS,
    "Age at Injury" = AgeAtDOI,
    "Year of Injury" = YEARDOI,
    "PID" = Patientennummer
  )

gg_miss_var(newdata, facet=ExamStage, show_pct = TRUE)+
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



  
gg_miss_upset(newdata,  nsets = 50,
              nintersects = NA)

+
  scale_fill_economist() +
  labs(fill = "", x = "", y = "Proportion of Patients [%]")+ ggtitle("% missing values, by exam stage and variable")+
  theme(axis.title = element_text(size = 10, face = 'bold'), 
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )




gg_miss_span(newdata, 
             UEMS, 
             span_every = 3000, 
             facet = AIS)

names(emsci)
