## ---------------------------
##
## Script name: 4_Injury_characteristics_sygen
##
## Purpose of script: To determine if and to what extent the injury characteristics of SCI patients changed between 1992 and 1997.
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

## set working directory for Mac and PC

setwd("/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen") 

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
#Clear working space

rm(list = ls())

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

sygen.included.cohort <- distinct(subset(sygen.included.cohort.all.times, Time==0 | Time==1), ID, .keep_all = TRUE)

#### -------------------------------------------------------------------------- Data Analysis ------------------------------------------------------------------------------------------------####

#### Rescale Data
rescale.many <- function(dat, column.nos) { 
  nms <- names(dat) 
  for(col in column.nos) { 
    name <- paste(nms[col],".rescaled", sep = "") 
    dat[name] <- rescale(dat[,col], center = TRUE, scale = TRUE) 
  } 
  cat(paste("Rescaled ", length(column.nos),      " variable(s)n")) 
  dat 
} 

sygen.included.cohort <-rescale.many(sygen.included.cohort, c(9)) 


#Count percentage of Female and male subjects by Year of Injury - OVERALL-----
sygen.ais.proportions.overall = sygen.included.cohort %>%
  count(YEARDOI.rescaled,AIS,Sex) %>%
  group_by(YEARDOI.rescaled)%>% 
  mutate(frequency = (n / sum(n))*100)


sygen.ais.proportions.overall.df <- as.data.frame(sygen.ais.proportions.overall)

#All AIS A patients (male and Female pooled)
sygen.ais.proportions.overall.ais.a <- subset(sygen.ais.proportions.overall.df, AIS=="AIS A")

sygen.ais.proportions.overall.ais.a.lm <- lm(frequency~YEARDOI.rescaled, data = sygen.ais.proportions.overall.ais.a, na.action = na.omit)
summary(sygen.ais.proportions.overall.ais.a.lm)

#All AIS B patients (male and Female pooled)
sygen.ais.proportions.overall.ais.b <- subset(sygen.ais.proportions.overall.df, AIS=="AIS B")

sygen.ais.proportions.overall.ais.b.lm <- lm(frequency~YEARDOI.rescaled, data = sygen.ais.proportions.overall.ais.b, na.action = na.omit)
summary(sygen.ais.proportions.overall.ais.b.lm)

#All AIS C patients (male and Female pooled)
sygen.ais.proportions.overall.ais.c <- subset(sygen.ais.proportions.overall.df, AIS=="AIS C")

sygen.ais.proportions.overall.ais.c.lm <- lm(frequency~YEARDOI.rescaled, data = sygen.ais.proportions.overall.ais.c, na.action = na.omit)
summary(sygen.ais.proportions.overall.ais.c.lm)

#All AIS D patients (male and Female pooled)
sygen.ais.proportions.overall.ais.d <- subset(sygen.ais.proportions.overall.df, AIS=="AIS D")

sygen.ais.proportions.overall.ais.d.lm <- lm(frequency~YEARDOI.rescaled, data = sygen.ais.proportions.overall.ais.d, na.action = na.omit)
summary(sygen.ais.proportions.overall.ais.d.lm)


#Count percentage of Female and male subjects by Year of Injury - SUBGROUPS stratified by Sex and Plegia-----
sygen.ais.proportions.overall.sex.plegia = sygen.included.cohort %>%
  count(YEARDOI.rescaled,AIS,Plegia) %>%
  group_by(YEARDOI.rescaled)%>% 
  mutate(frequency = (n / sum(n))*100)


sygen.ais.proportions.overall.sex.plegia.df <- as.data.frame(sygen.ais.proportions.overall.sex.plegia)


#------Calculate the change sex distribution over time for subgroups ----
ais.score<- unique(sygen.ais.proportions.overall.sex.plegia.df$AIS)

rescaled.nli <- unique(sygen.ais.proportions.overall.sex.plegia.df$Plegia) 

# create data frame to store results
results.sygen.ais.scores <- data.frame()
  for (j in rescaled.nli){
    for (i in ais.score){
      print(paste("MODEL",j, i,  sep = " "))
      df1 = subset(sygen.ais.proportions.overall.sex.plegia.df, (AIS == i & Plegia== j))
      mixed.lmer <- lm(frequency~YEARDOI.rescaled, data = df1, na.action = na.omit)
      print(summary(mixed.lmer))
      
      # ## capture summary stats
      intercept.estimate <- coef(summary(mixed.lmer))[1]
      YEARDOI.estimate <- coef(summary(mixed.lmer))[2]
      intercept.std <- coef(summary(mixed.lmer))[3]
      YEARDOI.std <- coef(summary(mixed.lmer))[4]
      intercept.tval <- coef(summary(mixed.lmer))[5]
      YEARDOI.tval <- coef(summary(mixed.lmer))[6]
      intercept.pval <- coef(summary(mixed.lmer))[7]
      YEARDOI.pval <- coef(summary(mixed.lmer))[8]
      
      # get coefficents of mixed.lmer
      cfit <- coef(summary(mixed.lmer))
      
      # # create temporary data frame
      df <- data.frame(Plegia= j,AIS = i, intercept.estimate = cfit[1], YEARDOI.estimate = cfit[2], intercept.std =cfit[3],
                       YEARDOI.std= cfit[4], intercept.tval= cfit[5],YEARDOI.tval= cfit[6],
                       intercept.pval =cfit[7],
                       YEARDOI.pval =cfit[8],
                       stringsAsFactors = F)
      
      #bind rows of temporary data frame to the results data frame
      results.sygen.ais.scores <- rbind(results.sygen.ais.scores, df)
      
    }
  }



#------Create Table to export

results.sygen.ais.scores.new <-merged.stack(results.sygen.ais.scores,                ## Add the id if it doesn't exist
                                            var.stubs = c("estimate", "std", "tval", "pval"),   ## Specify the stubs
                                            sep = "var.stubs",                   ## The sep is just the stubs 
                                            atStart = FALSE)   

results.sygen.ais.scores.new.df <- as.data.frame(results.sygen.ais.scores.new)

#Rename variables
names(results.sygen.ais.scores.new.df)[names(results.sygen.ais.scores.new.df) == '.time_1'] <- 'Variable'
names(results.sygen.ais.scores.new.df)[names(results.sygen.ais.scores.new.df) == 'estimate'] <- 'Estimate'
names(results.sygen.ais.scores.new.df)[names(results.sygen.ais.scores.new.df) == 'std'] <- 'Standard Error'
names(results.sygen.ais.scores.new.df)[names(results.sygen.ais.scores.new.df) == 'tval'] <- 't-value'
names(results.sygen.ais.scores.new.df)[names(results.sygen.ais.scores.new.df) == 'pval'] <- 'p-value'

#Create a new variable based on condition
results.sygen.ais.scores.new.df$order[(results.sygen.ais.scores.new.df$Variable == 'intercept.')] <- 1
results.sygen.ais.scores.new.df$order[(results.sygen.ais.scores.new.df$Variable == 'YEARDOI.')] <- 2

#Create a new variable based on condition
results.sygen.ais.scores.new.df$Variable[(results.sygen.ais.scores.new.df$Variable == 'intercept.')] <- "Intercept"
results.sygen.ais.scores.new.df$Variable[(results.sygen.ais.scores.new.df$Variable == 'YEARDOI.')] <- "YEARDOI"

#Create a new variable based on condition
results.sygen.ais.scores.new.df$model_temp[(results.sygen.ais.scores.new.df$Plegia == "para" & results.sygen.ais.scores.new.df$AIS == "AIS A")] <- 'Paraplegia:AIS A'
results.sygen.ais.scores.new.df$model_temp[(results.sygen.ais.scores.new.df$Plegia == "para" & results.sygen.ais.scores.new.df$AIS == "AIS B")] <- 'Paraplegia:AIS B'
results.sygen.ais.scores.new.df$model_temp[(results.sygen.ais.scores.new.df$Plegia == "para" & results.sygen.ais.scores.new.df$AIS == "AIS C" )] <- 'Paraplegia:AIS C'
results.sygen.ais.scores.new.df$model_temp[(results.sygen.ais.scores.new.df$Plegia == "para" & results.sygen.ais.scores.new.df$AIS == "AIS D" )] <- 'Paraplegia:AIS D'

results.sygen.ais.scores.new.df$model_temp[(results.sygen.ais.scores.new.df$Plegia == "tetra" & results.sygen.ais.scores.new.df$AIS == "AIS A")] <- 'Tetraplegia:AIS A'
results.sygen.ais.scores.new.df$model_temp[(results.sygen.ais.scores.new.df$Plegia == "tetra" & results.sygen.ais.scores.new.df$AIS == "AIS B")] <- 'Tetraplegia:AIS B'
results.sygen.ais.scores.new.df$model_temp[(results.sygen.ais.scores.new.df$Plegia == "tetra" & results.sygen.ais.scores.new.df$AIS == "AIS C" )] <- 'Tetraplegia:AIS C'
results.sygen.ais.scores.new.df$model_temp[(results.sygen.ais.scores.new.df$Plegia == "tetra" & results.sygen.ais.scores.new.df$AIS == "AIS D" )] <- 'Tetraplegia:AIS D'



#Add adjusted p-value column
results.sygen.ais.scores.new.df$Adjusted.pval<- as.numeric(results.sygen.ais.scores.new.df$`p-value`)*8

#Rename column
names(results.sygen.ais.scores.new.df)[names(results.sygen.ais.scores.new.df) == 'Adjusted.pval'] <- 'Adjusted p-value'

#Make t-value, p-value, and Adjusted p-value numeric
results.sygen.ais.scores.new.df$`t-value`<-as.numeric(results.sygen.ais.scores.new.df$`t-value`)
results.sygen.ais.scores.new.df$`p-value`<-as.numeric(results.sygen.ais.scores.new.df$`p-value`)
results.sygen.ais.scores.new.df$`Adjusted p-value`<-as.numeric(results.sygen.ais.scores.new.df$`Adjusted p-value`)

#Function to round to 3 digits
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

results.sygen.ais.scores.new.df.2 <- round_df(results.sygen.ais.scores.new.df, 3)


#Sort data
results.sygen.ais.scores.new.df.3digits <- arrange(results.sygen.ais.scores.new.df.2,model_temp,order)

#Create a new variable based on condition
results.sygen.ais.scores.new.df.3digits$Model[(results.sygen.ais.scores.new.df.3digits$Plegia == "tetra" & results.sygen.ais.scores.new.df.3digits$AIS == "AIS A" & results.sygen.ais.scores.new.df.3digits$order==1)] <- 'Tetraplegia: AIS A'
results.sygen.ais.scores.new.df.3digits$Model[(results.sygen.ais.scores.new.df.3digits$Plegia == "tetra" & results.sygen.ais.scores.new.df.3digits$AIS == "AIS B"& results.sygen.ais.scores.new.df.3digits$order==1)] <- 'Tetraplegia: AIS B'
results.sygen.ais.scores.new.df.3digits$Model[(results.sygen.ais.scores.new.df.3digits$Plegia == "tetra" & results.sygen.ais.scores.new.df.3digits$AIS == "AIS C" & results.sygen.ais.scores.new.df.3digits$order==1)] <- 'Tetraplegia: AIS C'
results.sygen.ais.scores.new.df.3digits$Model[(results.sygen.ais.scores.new.df.3digits$Plegia == "tetra" & results.sygen.ais.scores.new.df.3digits$AIS == "AIS D" & results.sygen.ais.scores.new.df.3digits$order==1)] <- 'Tetraplegia: AIS D'

results.sygen.ais.scores.new.df.3digits$Model[(results.sygen.ais.scores.new.df.3digits$Plegia == "para" & results.sygen.ais.scores.new.df.3digits$AIS == "AIS A" & results.sygen.ais.scores.new.df.3digits$order==1)] <- 'Paraplegia: AIS A'
results.sygen.ais.scores.new.df.3digits$Model[(results.sygen.ais.scores.new.df.3digits$Plegia == "para" & results.sygen.ais.scores.new.df.3digits$AIS == "AIS B" & results.sygen.ais.scores.new.df.3digits$order==1)] <- 'Paraplegia: AIS B'
results.sygen.ais.scores.new.df.3digits$Model[(results.sygen.ais.scores.new.df.3digits$Plegia == "para" & results.sygen.ais.scores.new.df.3digits$AIS == "AIS C" & results.sygen.ais.scores.new.df.3digits$order==1)] <- 'Paraplegia: AIS C'
results.sygen.ais.scores.new.df.3digits$Model[(results.sygen.ais.scores.new.df.3digits$Plegia == "para" & results.sygen.ais.scores.new.df.3digits$AIS == "AIS D" & results.sygen.ais.scores.new.df.3digits$order==1)] <- 'Paraplegia: AIS D'

# #Replace NA with empty cell
results.sygen.ais.scores.new.df.3digits[is.na(results.sygen.ais.scores.new.df.3digits)] <- ""
results.sygen.ais.scores.new.df.3digits[results.sygen.ais.scores.new.df.3digits == "<NA>"] <- ""

#Write csv file with only selected columns
write.csv(results.sygen.ais.scores.new.df.3digits[,c(11,3:7,10)],"/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen/Tables/ais_scores_distribution_sygen.csv", row.names = F)


#### -------------------------------------------------------------------------- Visualize Results ------------------------------------------------------------------------------------------------####

#----Plot the population pyramide 'Baseline Injury Severity' - OVERALL----
#Caculate the percentage of each AIS grade per year
sygen.ais.proportions.visualize = sygen.included.cohort %>%
  count(YEARDOI,AIS,Sex) %>%
  group_by(YEARDOI,Sex)%>% mutate(frequency = (n / sum(n))*100)

#Plot
sygen.ais.plot <-ggplot(data = sygen.ais.proportions.visualize, aes(x = YEARDOI, y = frequency, fill = AIS)) +
  geom_bar(data = sygen.ais.proportions.visualize %>% filter(Sex == "Female") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = sygen.ais.proportions.visualize %>% filter(Sex == "Male") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(1991, 1998), breaks = seq(1992, 1997, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of Patients [%]")+ ggtitle("Baseline Injury Severity")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )

sygen.ais.plot


ggsave(
  "ais.distribitution_overall.pdf",
  plot = sygen.ais.plot,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()



#----Plot the population pyramide 'Baseline Injury Severity' - Tetraplegic----

#Caculate the percentage of each AIS grade per year
sygen.ais.proportions.tetra = subset(sygen.included.cohort, Plegia=="tetra") %>%
  count(YEARDOI,AIS,Sex) %>%
  group_by(YEARDOI,Sex)%>% mutate(frequency = (n / sum(n))*100)


sygen.ais.plot.tetra <-ggplot(data = sygen.ais.proportions.tetra, aes(x = YEARDOI, y = frequency, fill = AIS)) +
  geom_bar(data = sygen.ais.proportions.tetra %>% filter(Sex == "Female") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = sygen.ais.proportions.tetra %>% filter(Sex == "Male") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(1991, 1998), breaks = seq(1992, 1997, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of Tetrapleic Patients [%]")+ ggtitle("Baseline Injury Severity")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )

sygen.ais.plot.tetra


ggsave(
  "ais.distribitution_tetra.pdf",
  plot = sygen.ais.plot.tetra,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()


#----Plot the population pyramide 'Baseline Injury Severity' - Paraplegic----

#Caculate the percentage of each AIS grade per year
sygen.ais.proportions.para = subset(sygen.included.cohort, Plegia=="para") %>%
  count(YEARDOI,AIS,Sex) %>%
  group_by(YEARDOI,Sex)%>% mutate(frequency = (n / sum(n))*100)


sygen.ais.plot.para <-ggplot(data = sygen.ais.proportions.para, aes(x = YEARDOI, y = frequency, fill = AIS)) +
  geom_bar(data = sygen.ais.proportions.para %>% filter(Sex == "Female") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = sygen.ais.proportions.para %>% filter(Sex == "Male") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(1991, 1998), breaks = seq(1992, 1997, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of parapleic Patients [%]")+ ggtitle("Baseline Injury Severity")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )

sygen.ais.plot.para


ggsave(
  "ais.distribitution_para.pdf",
  plot = sygen.ais.plot.para,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()



#----Plot the population pyramide 'Baseline Injury Level' - OVERALL----

sygen.nli.proportions = sygen.included.cohort %>%
  count(YEARDOI,NLI,Sex) %>%
  group_by(YEARDOI,Sex)%>% mutate(frequency = (n / sum(n))*100)

#Model
sygen.nli.proportions.lm <-lm(frequency~YEARDOI, data=sygen.nli.proportions)
summary(sygen.nli.proportions.lm)


#Plot
sygen.nli.plot <- ggplot(data = sygen.nli.proportions, aes(x = YEARDOI, y = frequency, fill = NLI)) +
  geom_bar(data = sygen.nli.proportions %>% filter(Sex == "Female") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = sygen.nli.proportions %>% filter(Sex == "Male") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(1991, 1998), breaks = seq(1992, 1997, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of Patients [%]")+ ggtitle("Baseline Injury level")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )
sygen.nli.plot

ggsave(
  "nli.distribitution_overall.pdf",
  plot = sygen.nli.plot,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()



#----Plot the population pyramide 'Baseline Injury Level' - AIS A----

sygen.nli.proportions.ais_a = subset(sygen.included.cohort, AIS=="AIS A") %>%
  count(YEARDOI,NLI,Sex) %>%
  group_by(YEARDOI,Sex)%>% mutate(frequency = (n / sum(n))*100)


sygen.nli.ais.a <- ggplot(data = sygen.nli.proportions.ais_a, aes(x = YEARDOI, y = frequency, fill = NLI)) +
  geom_bar(data = sygen.nli.proportions.ais_a %>% filter(Sex == "Female") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = sygen.nli.proportions.ais_a %>% filter(Sex == "Male") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(1991, 1998), breaks = seq(1992, 1997, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of AIS A Patients [%]")+ ggtitle("Baseline Injury level")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )
sygen.nli.ais.a

ggsave(
  "nli.distribitution.ais_a.pdf",
  plot = sygen.nli.ais.a,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()



#----Plot the population pyramide 'Baseline Injury Level' - AIS B----


sygen.nli.proportions.ais_b = subset(sygen.included.cohort, AIS=="AIS B") %>%
  count(YEARDOI,NLI,Sex) %>%
  group_by(YEARDOI,Sex)%>% mutate(frequency = (n / sum(n))*100)


sygen.nli.ais.b <- ggplot(data = sygen.nli.proportions.ais_b, aes(x = YEARDOI, y = frequency, fill = NLI)) +
  geom_bar(data = sygen.nli.proportions.ais_b %>% filter(Sex == "Female") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = sygen.nli.proportions.ais_b %>% filter(Sex == "Male") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(1991, 1998), breaks = seq(1992, 1997, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of AIS B Patients [%]")+ ggtitle("Baseline Injury level")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )
sygen.nli.ais.b

ggsave(
  "nli.distribitution.ais_b.pdf",
  plot = sygen.nli.ais.b,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()

#----Plot the population pyramide 'Baseline Injury Level' - AIS C----


sygen.nli.proportions.ais_c = subset(sygen.included.cohort, AIS=="AIS C") %>%
  count(YEARDOI,NLI,Sex) %>%
  group_by(YEARDOI,Sex)%>% mutate(frequency = (n / sum(n))*100)


sygen.nli.ais.c <- ggplot(data = sygen.nli.proportions.ais_c, aes(x = YEARDOI, y = frequency, fill = NLI)) +
  geom_bar(data = sygen.nli.proportions.ais_c %>% filter(Sex == "Female") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = sygen.nli.proportions.ais_c %>% filter(Sex == "Male") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(1991, 1998), breaks = seq(1992, 1997, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of AIS C Patients [%]")+ ggtitle("Baseline Injury level")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )
sygen.nli.ais.c

ggsave(
  "nli.distribitution.ais_c.pdf",
  plot = sygen.nli.ais.c,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()


#----Plot the population pyramide 'Baseline Injury Level' - AIS D----

sygen.nli.proportions.ais_d = subset(sygen.included.cohort, AIS=="AIS D") %>%
  count(YEARDOI,NLI,Sex) %>%
  group_by(YEARDOI,Sex)%>% mutate(frequency = (n / sum(n))*100)


sygen.nli.ais.d <- ggplot(data = sygen.nli.proportions.ais_d, aes(x = YEARDOI, y = frequency, fill = NLI)) +
  geom_bar(data = sygen.nli.proportions.ais_d %>% filter(Sex == "Female") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = sygen.nli.proportions.ais_d %>% filter(Sex == "Male") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(1991, 1998), breaks = seq(1992, 1997, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of AIS D Patients [%]")+ ggtitle("Baseline Injury level")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )
sygen.nli.ais.d

ggsave(
  "nli.distribitution.ais_d.pdf",
  plot = sygen.nli.ais.d,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()

#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####
