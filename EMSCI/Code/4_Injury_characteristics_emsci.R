## ---------------------------
##
## Script name: 4_baseline_injury_characteristics_emsci
##
## Purpose of script: To determine if and to what extent the injury characteristics of SCI patients changed between 2001 and 2019.
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
## ---------------------------
##
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
library(table1)
##
## ----------------------------
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
# if(!require(tidyr)){install.packages("tidyr")}
# if(!require(ggthemes)){install.packages("ggthemes")}
# if(!require(Hmisc)){install.packages("Hmisc")}
# if(!require(scales)){install.packages("scales")}
# if(!require(splitstackshape)){install.packages("splitstackshape")}
# if(!require(lmerTest)){install.packages("lmerTest")}
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
## Set working directory and output directorypaths
setwd("/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI") 
##
##
outdir_figures='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Figures'
outdir_tables='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables'
##
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

# Load original dataset
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T)

# Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, level of injury either cervical, thoracic, or lumbar,
#and AIS score A, B, C, or D
emsci.trauma.sex <- subset(emsci, (AgeAtDOI > 8) & (Sex=='f' | Sex=='m') & 
                                    (Cause=="ischemic" | Cause=="traumatic" | Cause=="haemorragic" |Cause=="disc herniation") & 
                                    (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar') & (YEARDOI >= 2000) &(AIS=='A' | AIS=="B"| AIS=="C"| AIS=="D"))

# Subset data to only patients with valid entry at stage 'very acute' or 'acute I' and remove duplicate patient numbers
emsci.trauma.sex.va.a1<-distinct(subset(emsci.trauma.sex, ExamStage=='acute I' | ExamStage=='very acute'), Patientennummer, .keep_all = TRUE)

#### -------------------------------------------------------------------------- Data Analysis ------------------------------------------------------------------------------------------------####

#---------- Data Analysis: Change in ratio between female and male patients over 20 years and display in table --------#

# Subset data to only patients with valid entry at stage 'very acute' or 'acute I' and remove duplicate patient numbers
emsci.trauma.sex.va.a1<-distinct(subset(emsci.trauma.sex, ExamStage=='acute I' | ExamStage=='very acute'), Patientennummer, .keep_all = TRUE)

#---------- Rescale Data
rescale.many <- function(dat, column.nos) { 
  nms <- names(dat) 
  for(col in column.nos) { 
    name <- paste(nms[col],".rescaled", sep = "") 
    dat[name] <- rescale(dat[,col], center = TRUE, scale = TRUE) 
  } 
  cat(paste("Rescaled ", length(column.nos),      " variable(s)n")) 
  dat 
} 

emsci.trauma.sex.va.a1 <-rescale.many(emsci.trauma.sex.va.a1, c(8)) 

#---------- Count percentage of female and male subjects by Year of Injury - OVERALL --------#
emsci.ais.proportions.overall = emsci.trauma.sex.va.a1 %>%
  dplyr::count(YEARDOI.rescaled,AIS,Sex) %>%
  dplyr::group_by(YEARDOI.rescaled)%>% 
  dplyr::mutate(frequency = (n / sum(n))*100)


emsci.ais.proportions.overall.df <- as.data.frame(emsci.ais.proportions.overall)

# All AIS A patients (male and female pooled)
emsci.ais.proportions.overall.ais.a <- subset(emsci.ais.proportions.overall.df, AIS=="A")

emsci.ais.proportions.overall.ais.a.lm <- lm(frequency~YEARDOI.rescaled, data = emsci.ais.proportions.overall.ais.a, na.action = na.omit)
summary(emsci.ais.proportions.overall.ais.a.lm)

# All AIS B patients (male and female pooled)
emsci.ais.proportions.overall.ais.b <- subset(emsci.ais.proportions.overall.df, AIS=="B")

emsci.ais.proportions.overall.ais.b.lm <- lm(frequency~YEARDOI.rescaled, data = emsci.ais.proportions.overall.ais.b, na.action = na.omit)
summary(emsci.ais.proportions.overall.ais.b.lm)

# All AIS C patients (male and female pooled)
emsci.ais.proportions.overall.ais.c <- subset(emsci.ais.proportions.overall.df, AIS=="C")

emsci.ais.proportions.overall.ais.c.lm <- lm(frequency~YEARDOI.rescaled, data = emsci.ais.proportions.overall.ais.c, na.action = na.omit)
summary(emsci.ais.proportions.overall.ais.c.lm)

# All AIS D patients (male and female pooled)
emsci.ais.proportions.overall.ais.d <- subset(emsci.ais.proportions.overall.df, AIS=="D")

emsci.ais.proportions.overall.ais.d.lm <- lm(frequency~YEARDOI.rescaled, data = emsci.ais.proportions.overall.ais.d, na.action = na.omit)
summary(emsci.ais.proportions.overall.ais.d.lm)

#---------- Count percentage of female and male subjects by Year of Injury - SUBGROUPS stratified by Sex and Plegia --------#
emsci.ais.proportions.overall.df = emsci.trauma.sex.va.a1 %>%
  dplyr::count(YEARDOI.rescaled,AIS,Sex,plegia) %>%
  dplyr::group_by(YEARDOI.rescaled)%>% 
  dplyr::mutate(frequency = (n / sum(n))*100)%>% 
  as.data.frame()

#---------- Calculate the change sex distribution over time for subgroups --------#
ais.score<- unique(emsci.ais.proportions.overall.df$AIS)
rescaled.sex <- unique(emsci.ais.proportions.overall.df$Sex)
rescaled.nli <- unique(emsci.ais.proportions.overall.df$plegia) 

# Create data frame to store results
results.emsci.ais.scores <- data.frame()
for (h in rescaled.sex) {
  for (j in rescaled.nli){
    for (i in ais.score){
      print(paste("MODEL",h,j, i,  sep = " "))
      df1 = subset(emsci.ais.proportions.overall.df, (AIS == i & plegia== j & Sex == h))
      mixed.lmer <- lm(frequency~YEARDOI.rescaled, data = df1, na.action = na.omit)
      print(summary(mixed.lmer))
      n=nobs(mixed.lmer)
      
      # Capture summary stats
      intercept.estimate <- coef(summary(mixed.lmer))[1]
      YEARDOI.estimate <- coef(summary(mixed.lmer))[2]
      intercept.std <- coef(summary(mixed.lmer))[3]
      YEARDOI.std <- coef(summary(mixed.lmer))[4]
      intercept.tval <- coef(summary(mixed.lmer))[5]
      YEARDOI.tval <- coef(summary(mixed.lmer))[6]
      intercept.pval <- coef(summary(mixed.lmer))[7]
      YEARDOI.pval <- coef(summary(mixed.lmer))[8]
      
      # Get coefficents of mixed.lmer
      cfit <- coef(summary(mixed.lmer))
      
      # Create temporary data frame
      df <- data.frame(Sex= h, plegia= j,AIS = i, intercept.estimate = cfit[1], YEARDOI.estimate = cfit[2], intercept.std =cfit[3],
                       YEARDOI.std= cfit[4], intercept.tval= cfit[5],YEARDOI.tval= cfit[6],
                       intercept.pval =cfit[7],
                       YEARDOI.pval =cfit[8],
                       stringsAsFactors = F)
      
      
      
      df2<- cbind(df, n)
      
      # Bind rows of temporary data frame to the results data frame
      results.emsci.ais.scores <- rbind(results.emsci.ais.scores, df2)
      
    }
  }
}


#---------- Create Table to export

results.emsci.ais.scores.new <-merged.stack(results.emsci.ais.scores,                ## Add the id if it doesn't exist
                                     var.stubs = c("estimate", "std", "tval", "pval"),   ## Specify the stubs
                                     sep = "var.stubs",                   ## The sep is just the stubs 
                                     atStart = FALSE)   

results.emsci.ais.scores.new.df <- as.data.frame(results.emsci.ais.scores.new)

# Rename variables
names(results.emsci.ais.scores.new.df)[names(results.emsci.ais.scores.new.df) == '.time_1'] <- 'Variable'
names(results.emsci.ais.scores.new.df)[names(results.emsci.ais.scores.new.df) == 'estimate'] <- 'Estimate'
names(results.emsci.ais.scores.new.df)[names(results.emsci.ais.scores.new.df) == 'std'] <- 'Standard Error'
names(results.emsci.ais.scores.new.df)[names(results.emsci.ais.scores.new.df) == 'tval'] <- 't-value'
names(results.emsci.ais.scores.new.df)[names(results.emsci.ais.scores.new.df) == 'pval'] <- 'p-value'

# Create a new variable based on condition
results.emsci.ais.scores.new.df$order[(results.emsci.ais.scores.new.df$Variable == 'intercept.')] <- 1
results.emsci.ais.scores.new.df$order[(results.emsci.ais.scores.new.df$Variable == 'YEARDOI.')] <- 2

# Create a new variable based on condition
results.emsci.ais.scores.new.df$Variable[(results.emsci.ais.scores.new.df$Variable == 'intercept.')] <- "Intercept"
results.emsci.ais.scores.new.df$Variable[(results.emsci.ais.scores.new.df$Variable == 'YEARDOI.')] <- "YEARDOI"

#Create a new variable based on condition
results.emsci.ais.scores.new.df$model_temp[(results.emsci.ais.scores.new.df$Sex == 'f' & results.emsci.ais.scores.new.df$plegia == "para" & results.emsci.ais.scores.new.df$AIS == "A")] <- 'Female:Paraplegia:AIS A'
results.emsci.ais.scores.new.df$model_temp[(results.emsci.ais.scores.new.df$Sex == 'f' & results.emsci.ais.scores.new.df$plegia == "para" & results.emsci.ais.scores.new.df$AIS == "B")] <- 'Female:Paraplegia:AIS B'
results.emsci.ais.scores.new.df$model_temp[(results.emsci.ais.scores.new.df$Sex == 'f' & results.emsci.ais.scores.new.df$plegia == "para" & results.emsci.ais.scores.new.df$AIS == "C" )] <- 'Female:Paraplegia:AIS C'
results.emsci.ais.scores.new.df$model_temp[(results.emsci.ais.scores.new.df$Sex == 'f' & results.emsci.ais.scores.new.df$plegia == "para" & results.emsci.ais.scores.new.df$AIS == "D" )] <- 'Female:Paraplegia:AIS D'

results.emsci.ais.scores.new.df$model_temp[(results.emsci.ais.scores.new.df$Sex == 'f' & results.emsci.ais.scores.new.df$plegia == "tetra" & results.emsci.ais.scores.new.df$AIS == "A")] <- 'Female:Tetraplegia:AIS A'
results.emsci.ais.scores.new.df$model_temp[(results.emsci.ais.scores.new.df$Sex == 'f' & results.emsci.ais.scores.new.df$plegia == "tetra" & results.emsci.ais.scores.new.df$AIS == "B")] <- 'Female:Tetraplegia:AIS B'
results.emsci.ais.scores.new.df$model_temp[(results.emsci.ais.scores.new.df$Sex == 'f' & results.emsci.ais.scores.new.df$plegia == "tetra" & results.emsci.ais.scores.new.df$AIS == "C" )] <- 'Female:Tetraplegia:AIS C'
results.emsci.ais.scores.new.df$model_temp[(results.emsci.ais.scores.new.df$Sex == 'f' & results.emsci.ais.scores.new.df$plegia == "tetra" & results.emsci.ais.scores.new.df$AIS == "D" )] <- 'Female:Tetraplegia:AIS D'

results.emsci.ais.scores.new.df$model_temp[(results.emsci.ais.scores.new.df$Sex == 'm' & results.emsci.ais.scores.new.df$plegia == "para" & results.emsci.ais.scores.new.df$AIS == "A")] <- 'Male:Paraplegia:AIS A'
results.emsci.ais.scores.new.df$model_temp[(results.emsci.ais.scores.new.df$Sex == 'm' & results.emsci.ais.scores.new.df$plegia == "para" & results.emsci.ais.scores.new.df$AIS == "B")] <- 'Male:Paraplegia:AIS B'
results.emsci.ais.scores.new.df$model_temp[(results.emsci.ais.scores.new.df$Sex == 'm' & results.emsci.ais.scores.new.df$plegia == "para" & results.emsci.ais.scores.new.df$AIS == "C" )] <- 'Male:Paraplegia:AIS C'
results.emsci.ais.scores.new.df$model_temp[(results.emsci.ais.scores.new.df$Sex == 'm' & results.emsci.ais.scores.new.df$plegia == "para" & results.emsci.ais.scores.new.df$AIS == "D" )] <- 'Male:Paraplegia:AIS D'

results.emsci.ais.scores.new.df$model_temp[(results.emsci.ais.scores.new.df$Sex == 'm' & results.emsci.ais.scores.new.df$plegia == "tetra" & results.emsci.ais.scores.new.df$AIS == "A")] <- 'Male:Tetraplegia:AIS A'
results.emsci.ais.scores.new.df$model_temp[(results.emsci.ais.scores.new.df$Sex == 'm' & results.emsci.ais.scores.new.df$plegia == "tetra" & results.emsci.ais.scores.new.df$AIS == "B")] <- 'Male:Tetraplegia:AIS B'
results.emsci.ais.scores.new.df$model_temp[(results.emsci.ais.scores.new.df$Sex == 'm' & results.emsci.ais.scores.new.df$plegia == "tetra" & results.emsci.ais.scores.new.df$AIS == "C" )] <- 'Male:Tetraplegia:AIS C'
results.emsci.ais.scores.new.df$model_temp[(results.emsci.ais.scores.new.df$Sex == 'm' & results.emsci.ais.scores.new.df$plegia == "tetra" & results.emsci.ais.scores.new.df$AIS == "D" )] <- 'Male:Tetraplegia:AIS D'


# Add adjusted p-value column
results.emsci.ais.scores.new.df$Adjusted.pval<- as.numeric(results.emsci.ais.scores.new.df$`p-value`)*16

# Rename column
names(results.emsci.ais.scores.new.df)[names(results.emsci.ais.scores.new.df) == 'Adjusted.pval'] <- 'Adjusted p-value'

# Make t-value, p-value, and Adjusted p-value numeric
results.emsci.ais.scores.new.df$`t-value`<-as.numeric(results.emsci.ais.scores.new.df$`t-value`)
results.emsci.ais.scores.new.df$`p-value`<-as.numeric(results.emsci.ais.scores.new.df$`p-value`)
results.emsci.ais.scores.new.df$`Adjusted p-value`<-as.numeric(results.emsci.ais.scores.new.df$`Adjusted p-value`)

# Function to round to 3 digits
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

results.emsci.ais.scores.new.df.2 <- round_df(results.emsci.ais.scores.new.df, 3)

# Sort data
results.emsci.ais.scores.new.df.3digits <- arrange(results.emsci.ais.scores.new.df.2,model_temp,order)

# Create a new variable based on condition
results.emsci.ais.scores.new.df.3digits$Model[(results.emsci.ais.scores.new.df.3digits$Sex == 'f' & results.emsci.ais.scores.new.df.3digits$plegia == "tetra" & results.emsci.ais.scores.new.df.3digits$AIS == "A" & results.emsci.ais.scores.new.df.3digits$order==1)] <- 'Female: Tetraplegia: AIS A'
results.emsci.ais.scores.new.df.3digits$Model[(results.emsci.ais.scores.new.df.3digits$Sex == 'f' & results.emsci.ais.scores.new.df.3digits$plegia == "tetra" & results.emsci.ais.scores.new.df.3digits$AIS == "B"& results.emsci.ais.scores.new.df.3digits$order==1)] <- 'Female: Tetraplegia: AIS B'
results.emsci.ais.scores.new.df.3digits$Model[(results.emsci.ais.scores.new.df.3digits$Sex == 'f' & results.emsci.ais.scores.new.df.3digits$plegia == "tetra" & results.emsci.ais.scores.new.df.3digits$AIS == "C" & results.emsci.ais.scores.new.df.3digits$order==1)] <- 'Female: Tetraplegia: AIS C'
results.emsci.ais.scores.new.df.3digits$Model[(results.emsci.ais.scores.new.df.3digits$Sex == 'f' & results.emsci.ais.scores.new.df.3digits$plegia == "tetra" & results.emsci.ais.scores.new.df.3digits$AIS == "D" & results.emsci.ais.scores.new.df.3digits$order==1)] <- 'Female: Tetraplegia: AIS D'

results.emsci.ais.scores.new.df.3digits$Model[(results.emsci.ais.scores.new.df.3digits$Sex == 'f' & results.emsci.ais.scores.new.df.3digits$plegia == "para" & results.emsci.ais.scores.new.df.3digits$AIS == "A" & results.emsci.ais.scores.new.df.3digits$order==1)] <- 'Female: Paraplegia: AIS A'
results.emsci.ais.scores.new.df.3digits$Model[(results.emsci.ais.scores.new.df.3digits$Sex == 'f' & results.emsci.ais.scores.new.df.3digits$plegia == "para" & results.emsci.ais.scores.new.df.3digits$AIS == "B" & results.emsci.ais.scores.new.df.3digits$order==1)] <- 'Female: Paraplegia: AIS B'
results.emsci.ais.scores.new.df.3digits$Model[(results.emsci.ais.scores.new.df.3digits$Sex == 'f' & results.emsci.ais.scores.new.df.3digits$plegia == "para" & results.emsci.ais.scores.new.df.3digits$AIS == "C" & results.emsci.ais.scores.new.df.3digits$order==1)] <- 'Female: Paraplegia: AIS C'
results.emsci.ais.scores.new.df.3digits$Model[(results.emsci.ais.scores.new.df.3digits$Sex == 'f' & results.emsci.ais.scores.new.df.3digits$plegia == "para" & results.emsci.ais.scores.new.df.3digits$AIS == "D" & results.emsci.ais.scores.new.df.3digits$order==1)] <- 'Female: Paraplegia: AIS D'

results.emsci.ais.scores.new.df.3digits$Model[(results.emsci.ais.scores.new.df.3digits$Sex == 'm' & results.emsci.ais.scores.new.df.3digits$plegia == "tetra" & results.emsci.ais.scores.new.df.3digits$AIS == "A" & results.emsci.ais.scores.new.df.3digits$order==1)] <- 'Male: Tetraplegia: AIS A'
results.emsci.ais.scores.new.df.3digits$Model[(results.emsci.ais.scores.new.df.3digits$Sex == 'm' & results.emsci.ais.scores.new.df.3digits$plegia == "tetra" & results.emsci.ais.scores.new.df.3digits$AIS == "B" & results.emsci.ais.scores.new.df.3digits$order==1)] <- 'Male: Tetraplegia: AIS B'
results.emsci.ais.scores.new.df.3digits$Model[(results.emsci.ais.scores.new.df.3digits$Sex == 'm' & results.emsci.ais.scores.new.df.3digits$plegia == "tetra" & results.emsci.ais.scores.new.df.3digits$AIS == "C" & results.emsci.ais.scores.new.df.3digits$order==1)] <- 'Male: Tetraplegia: AIS C'
results.emsci.ais.scores.new.df.3digits$Model[(results.emsci.ais.scores.new.df.3digits$Sex == 'm' & results.emsci.ais.scores.new.df.3digits$plegia == "tetra" & results.emsci.ais.scores.new.df.3digits$AIS == "D" & results.emsci.ais.scores.new.df.3digits$order==1)] <- 'Male: Tetraplegia: AIS D'

results.emsci.ais.scores.new.df.3digits$Model[(results.emsci.ais.scores.new.df.3digits$Sex == 'm' & results.emsci.ais.scores.new.df.3digits$plegia == "para" & results.emsci.ais.scores.new.df.3digits$AIS == "A"& results.emsci.ais.scores.new.df.3digits$order==1)] <- 'Male: Paraplegia: AIS A'
results.emsci.ais.scores.new.df.3digits$Model[(results.emsci.ais.scores.new.df.3digits$Sex == 'm' & results.emsci.ais.scores.new.df.3digits$plegia == "para" & results.emsci.ais.scores.new.df.3digits$AIS == "B" & results.emsci.ais.scores.new.df.3digits$order==1)] <- 'Male: Paraplegia: AIS B'
results.emsci.ais.scores.new.df.3digits$Model[(results.emsci.ais.scores.new.df.3digits$Sex == 'm' & results.emsci.ais.scores.new.df.3digits$plegia == "para" & results.emsci.ais.scores.new.df.3digits$AIS == "C" & results.emsci.ais.scores.new.df.3digits$order==1)] <- 'Male: Paraplegia: AIS C'
results.emsci.ais.scores.new.df.3digits$Model[(results.emsci.ais.scores.new.df.3digits$Sex == 'm' & results.emsci.ais.scores.new.df.3digits$plegia == "para" & results.emsci.ais.scores.new.df.3digits$AIS == "D"& results.emsci.ais.scores.new.df.3digits$order==1)] <- 'Male: Paraplegia: AIS D'

# Replace NA with empty cell
results.emsci.ais.scores.new.df.3digits[is.na(results.emsci.ais.scores.new.df.3digits)] <- ""
results.emsci.ais.scores.new.df.3digits[results.emsci.ais.scores.new.df.3digits == "<NA>"] <- ""

# Write csv file with only selected columns
write.csv(results.emsci.ais.scores.new.df.3digits[,c(13,4:9,12)],"/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables/ais_scores_distribution_emsci.csv", row.names = F)


#### -------------------------------------------------------------------------- Visualization ------------------------------------------------------------------------------------------------####

# Caculate the percentage of each AIS grade per year
emsci.ais.proportions = emsci.trauma.sex.va.a1 %>%
  dplyr::count(X2_year_bins,AIS,Sex) %>%
  dplyr::group_by(X2_year_bins, Sex)%>% 
  dplyr::mutate(frequency = (n / sum(n))*100)

#---------- Plot the population pyramide 'Baseline Injury Severity' - OVERALL --------#
emsci.ais.plot <-ggplot(data = emsci.ais.proportions, aes(x = X2_year_bins, y = frequency, fill = AIS)) +
  geom_bar(data = emsci.ais.proportions %>% filter(Sex == "f") %>% arrange(rev(X2_year_bins)),
           stat = "identity")+
  geom_bar(data = emsci.ais.proportions %>% filter(Sex == "m") %>% arrange(rev(X2_year_bins)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 20), expand = c(0,0)) +
  #scale_x_continuous(labels = abs, limits = c(2000, 2020), breaks = seq(2001, 2019, 1), expand = c(0,0))+ 
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

emsci.ais.plot

# Save plot
ggsave(
  "ais.overall.emsci.pdf",
  plot = emsci.ais.plot,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Plot the population pyramide 'Baseline Injury Severity' - Tetraplegic --------#

#Caculate the percentage of each AIS grade per year
emsci.ais.proportions.tetra = subset(emsci.trauma.sex.va.a1, plegia == 'tetra')%>%
  dplyr::count(YEARDOI,AIS,Sex) %>%
  dplyr::group_by(YEARDOI, Sex)%>% 
  dplyr::mutate(frequency = (n / sum(n))*100)


emsci.ais.plot.tetra <-ggplot(data = emsci.ais.proportions.tetra, aes(x = YEARDOI, y = frequency, fill = AIS)) +
  geom_bar(data = emsci.ais.proportions.tetra %>% filter(Sex == "f") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = emsci.ais.proportions.tetra %>% filter(Sex == "m") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(2000, 2020), breaks = seq(2001, 2019, 1), expand = c(0,0))+ 
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

emsci.ais.plot.tetra

# Save plot
ggsave(
  "ais.tetra.emsci.pdf",
  plot = emsci.ais.plot.tetra,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Plot the population pyramide 'Baseline Injury Severity' - Paraplegic --------#

#Caculate the percentage of each AIS grade per year
emsci.ais.proportions.tetra = subset(emsci.trauma.sex.va.a1, plegia == 'para')%>%
  dplyr::count(YEARDOI,AIS,Sex) %>%
  dplyr::group_by(YEARDOI, Sex)%>%  
  dplyr::mutate(frequency = (n / sum(n))*100)

emsci.ais.plot.para <-ggplot(data = emsci.ais.proportions.tetra, aes(x = YEARDOI, y = frequency, fill = AIS)) +
  geom_bar(data = emsci.ais.proportions.tetra %>% filter(Sex == "f") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = emsci.ais.proportions.tetra %>% filter(Sex == "m") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(2000, 2020), breaks = seq(2001, 2019, 1), expand = c(0,0))+ 
  geom_hline(yintercept = 0) +
  theme_economist(horizontal = FALSE) +
  scale_fill_economist() +
  labs(fill = "", x = "Year of Injury", y = "Proportion of Paraplegic Patients [%]")+ ggtitle("Baseline Injury Severity")+
  theme(axis.title = element_text(size = 12, face = 'bold'), 
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )

emsci.ais.plot.para

# Save plot
ggsave(
  "ais.para.emsci.pdf",
  plot = emsci.ais.plot.para,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Plot the population pyramide 'Baseline Injury Level' - OVERALL --------#

emsci.nli.proportions = emsci.trauma.sex.va.a1 %>%
  dplyr::count(YEARDOI,NLI_level,Sex) %>%
  dplyr::group_by(YEARDOI,Sex)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

#Reorder levels
emsci.nli.proportions$NLI_level <- factor(emsci.nli.proportions$NLI_level, levels = c("cervical", "thoracic", "lumbar"))

emsci.nli.plot <- ggplot(data = emsci.nli.proportions, aes(x = YEARDOI, y = frequency, fill = NLI_level)) +
  geom_bar(data = emsci.nli.proportions %>% filter(Sex == "f") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = emsci.nli.proportions %>% filter(Sex == "m") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 20), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(2000, 2020), breaks = seq(2001, 2019, 1), expand = c(0,0))+ 
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
emsci.nli.plot

#Save plot
ggsave(
  "nli.overall.emsci.pdf",
  plot = emsci.nli.plot,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Plot the population pyramide 'Baseline Injury Level' - AIS A --------#

emsci.nli.proportions = subset(emsci.trauma.sex.va.a1, AIS=='A') %>%
  dplyr::count(YEARDOI,NLI_level,Sex,AIS) %>%
  dplyr::group_by(YEARDOI,Sex)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

# Reorder levels
emsci.nli.proportions$NLI_level <- factor(emsci.nli.proportions$NLI_level, levels = c("cervical", "thoracic", "lumbar"))

emsci.nli.ais.a <- ggplot(data = subset(emsci.nli.proportions, AIS=='A'), aes(x = YEARDOI, y = frequency, fill = NLI_level)) +
  geom_bar(data = emsci.nli.proportions %>% filter(Sex == "f") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = emsci.nli.proportions %>% filter(Sex == "m") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 110), breaks = seq(-100, 100, 20), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(2000, 2020), breaks = seq(2001, 2019, 1), expand = c(0,0))+ 
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
emsci.nli.ais.a

# Save plot
ggsave(
  "nli.ais.a.emsci.pdf",
  plot = emsci.nli.ais.a,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Plot the population pyramide 'Baseline Injury Level' - AIS B --------#

emsci.nli.proportions = subset(emsci.trauma.sex.va.a1, AIS=='B') %>%
  dplyr::count(YEARDOI,NLI_level,Sex,AIS) %>%
  dplyr::group_by(YEARDOI,Sex)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

#Reorder levels
emsci.nli.proportions$NLI_level <- factor(emsci.nli.proportions$NLI_level, levels = c("cervical", "thoracic", "lumbar"))

emsci.nli.ais.b <- ggplot(data = subset(emsci.nli.proportions, AIS=='B'), aes(x = YEARDOI, y = frequency, fill = NLI_level)) +
  geom_bar(data = emsci.nli.proportions %>% filter(Sex == "f") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = emsci.nli.proportions %>% filter(Sex == "m") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 110), breaks = seq(-100, 100, 20), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(2000, 2020), breaks = seq(2001, 2019, 1), expand = c(0,0))+ 
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
emsci.nli.ais.b

# Save plot
ggsave(
  "nli.ais.b.emsci.pdf",
  plot = emsci.nli.ais.b,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Plot the population pyramide 'Baseline Injury Level' - AIS C --------#

emsci.nli.proportions = subset(emsci.trauma.sex.va.a1, AIS=='C') %>%
  dplyr::count(YEARDOI,NLI_level,Sex,AIS) %>%
  dplyr::group_by(YEARDOI,Sex)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

# Reorder levels
emsci.nli.proportions$NLI_level <- factor(emsci.nli.proportions$NLI_level, levels = c("cervical", "thoracic", "lumbar"))


emsci.nli.ais.c <- ggplot(data = subset(emsci.nli.proportions, AIS=='C'), aes(x = YEARDOI, y = frequency, fill = NLI_level)) +
  geom_bar(data = emsci.nli.proportions %>% filter(Sex == "f") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = emsci.nli.proportions %>% filter(Sex == "m") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 110), breaks = seq(-100, 100, 20), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(2000, 2020), breaks = seq(2001, 2019, 1), expand = c(0,0))+ 
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
emsci.nli.ais.c

# Save plot
ggsave(
  "nli.ais.c.emsci.pdf",
  plot = emsci.nli.ais.c,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()


#----Plot the population pyramide 'Baseline Injury Level' - AIS D --------#

emsci.nli.proportions = subset(emsci.trauma.sex.va.a1, AIS=='D') %>%
  dplyr::count(YEARDOI,NLI_level,Sex,AIS) %>%
  dplyr::group_by(YEARDOI,Sex)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

# Reorder levels
emsci.nli.proportions$NLI_level <- factor(emsci.nli.proportions$NLI_level, levels = c("cervical", "thoracic", "lumbar"))

emsci.nli.proportions.d <-subset(emsci.nli.proportions)

emsci.nli.ais.d <- ggplot(data = subset(emsci.nli.proportions, AIS=='D'), aes(x = YEARDOI, y = frequency, fill = NLI_level)) +
  geom_bar(data = emsci.nli.proportions %>% filter(Sex == "f") %>% arrange(rev(YEARDOI)),
           stat = "identity")+
  geom_bar(data = emsci.nli.proportions %>% filter(Sex == "m") %>% arrange(rev(YEARDOI)),
           stat = "identity",
           mapping = aes(y = -frequency)) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-101, 110), breaks = seq(-100, 100, 20), expand = c(0,0)) +
  scale_x_continuous(labels = abs, limits = c(2000, 2020), breaks = seq(2001, 2019, 1), expand = c(0,0))+ 
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
emsci.nli.ais.d

# Save plot
ggsave(
  "nli.ais.d.emsci.pdf",
  plot = emsci.nli.ais.d,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Assess whether the proportion of injury severities (AIS A, B, C, D) changed over time and display in table --------#

# Subset data: all patients with entry of AIS grade at stage acute I
emsci.ais <- subset(emsci.trauma.sex, AIS=="A"| AIS=="B"| AIS=="C"| AIS=="D")

# Create age groups
emsci.ais$agegr <- cut(emsci.ais$AgeAtDOI, breaks=c(0,19,29,49,69,100)) 
table(emsci.ais$agegr)

# Calculate ratios of AIS grades per age group per year for female subjects
ais_ratios.by.agegroup.female = subset(emsci.ais, Sex=='f') %>%
  dplyr::count(AIS, YEARDOI, agegr) %>%
  dplyr::group_by(agegr, YEARDOI)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

# Fit regression model with the proportion of AIS grade as the response, and time as the predictor
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

# Calculate ratios of AIS grades per age group per year for male subjects
ais_ratios.by.agegroup.male= subset(emsci.ais, Sex=='m') %>%
  dplyr::count(AIS, YEARDOI, agegr) %>%
  dplyr::group_by(agegr, YEARDOI)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

# Fit regression model with the proportion of AIS grade as the response, and time as the predictor
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

# Combine plots
grid.arrange(estimate.plot.female,
             estimate.plot.male,
             widths=c(0.4,0.4),
             ncol=2)


#---------- Histograms of baseline injury characteristics over 20 years --------#

label(emsci.trauma.sex.va.a1$AIS) <- c("AIS-A", "AIS-B", "AIS-C", "AIS-D")


histogram.baseline.tms <-emsci.trauma.sex.va.a1%>% 
  dplyr::filter(plegia == "tetra") %>% 
  dplyr::select(AIS, X5_year_bins, TMS, Sex, AgeAtDOI)%>%
  dplyr::group_by(AIS, X5_year_bins) %>%
  #dplyr::mutate(percent = LEMS/sum(LEMS))%>%
  as.data.frame()%>%
  ggplot(aes(TMS)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +facet_grid(X5_year_bins~AIS)+
  scale_fill_viridis_d(option="plasma") + theme_bw()+
  theme(legend.position = 'none')+
  xlab("TMS at baseline") +
  ylab("Proportions")+
  ggtitle("Paraplegic Spinal Cord Injury")
histogram.baseline.tms

# Save plot
ggsave(
  "histogram.baseline.tms.para.pdf",
  plot = histogram.baseline.tms,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()



  

#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####



