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
## ---------------------------
##
## load up the packages we will need:  
library(lme4)
library(sjPlot)
library(scales)
library(lmerTest)
library(dplyr)
library(splitstackshape)
##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
# if(!require(lme4)){install.packages("lme4")}
# if(!require(sjPlot)){install.packages("sjPlot")}
# if(!require(scales)){install.packages("scales")}
# if(!require(lmerTest)){install.packages("lmerTest")}
# if(!require(dplyr)){install.packages("dplyr")}
# if(!require(splitstackshape)){install.packages("splitstackshape")}
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
setwd("/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI") 
##
#### ---------------------------
##
## Set output directorypaths
outdir_figures='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Figures'
outdir_tables='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables'
##
##
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

# Load original dataset
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T,  na.strings=c("","NA"))

# Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
emsci.trauma.sex <- subset(emsci, (AgeAtDOI > 8) & (Sex=='f' | Sex=='m') & ###Age at DOI and Sex
                             (Cause=="ischemic" | Cause=="traumatic" | Cause=="haemorragic" |Cause=="disc herniation") & 
                             (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar')&   ## Neurological level
                             (AIS=="A"| AIS=="B"| AIS=="C"| AIS=="D")) #AIS Grades


# Subset data to only patients with valid entry at stage 'very acute' or 'acute I' and remove duplicate patient numbers
emsci.trauma.sex.va.a1<-distinct(subset(emsci.trauma.sex, ExamStage=='acute I' | ExamStage=='very acute') , Patientennummer, .keep_all = TRUE)

# Create new variable: Baseline AIS grade
emsci.trauma.sex.va.a1$baseline.ais <-emsci.trauma.sex.va.a1$AIS

# Merge
emsci.trauma.sex.baseline.ais <-merge(emsci.trauma.sex, emsci.trauma.sex.va.a1[,c(2,243)])

#Convert certain columns to numeric
emsci.trauma.sex.baseline.ais[,c(5,6,8,11,23,26,29,32,35,186, 188,190, 192)] <- sapply(emsci.trauma.sex.baseline.ais[,c(5,6,8,11,23,26,29,32,35,186, 188,190, 192)], as.numeric)

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

emsci.rescaled <-rescale.many(emsci.trauma.sex.baseline.ais, c(5,6,8,11)) 

# Prepare selection variables
ais.score<-unique(emsci.rescaled$baseline.ais)
rescaled.nli <- unique(emsci.rescaled$plegia)
rescaled.sex <- unique(emsci.rescaled$Sex)
emsci.rescaled$Patientennummer <- as.factor(emsci.rescaled$Patientennummer)

# Create data frame to store results
results <- data.frame()
for (h in rescaled.sex) {
  for (j in rescaled.nli){
    for (i in ais.score){
      print(paste("MODEL",h,j, i,  sep = " "))
      df1 = subset(emsci.rescaled, (baseline.ais == i & plegia == j & Sex == h))
      #if (nrow(df1) == 0) next
      #mixed.lmer <- nlme(LEMS~ ExamStage_weeks.rescaled*YEARDOI.rescaled+AgeAtDOI.rescaled + (1|Patientennummer), data = df1)
      #print(summary(mixed.lmer))
      
      mixed.lmer <- nlme::nlme(LEMS ~ SSasympOff(ExamStage_weeks.rescaled*YEARDOI.rescaled+AgeAtDOI.rescaled, Asym, R0, lrc),
                 data = df1,
                 fixed = Asym + R0 + lrc ~ 1,
                 random = Asym ~ 1,
                 na.action=na.exclude, 
                 na.fail(emsci.rescaled),
                 naPattern = ~ !is.na(LEMS),
                 method="ML",verbose=TRUE)
      
      
      # Capture summary stats
      intercept.estimate <- coef(summary(mixed.lmer))[1]
      time_rescaled.estimate <- coef(summary(mixed.lmer))[2]
      yeardoi.estimate <- coef(summary(mixed.lmer))[3]
      age.estimate <- coef(summary(mixed.lmer))[4]
      time.yeardoi.estimate <- coef(summary(mixed.lmer))[5]
      intercept.std <- coef(summary(mixed.lmer))[6]
      time_rescaled.std <- coef(summary(mixed.lmer))[7]
      yeardoi.std <- coef(summary(mixed.lmer))[8]
      age.std <- coef(summary(mixed.lmer))[9]
      time.yeardoi.std <- coef(summary(mixed.lmer))[10]
      intercept.df <- coef(summary(mixed.lmer))[11]
      time_rescaled.df <- coef(summary(mixed.lmer))[12]
      yeardoi.df <- coef(summary(mixed.lmer))[13]
      age.df <- coef(summary(mixed.lmer))[14]
      time.yeardoi.df <- coef(summary(mixed.lmer))[15]
      intercept.tval <- coef(summary(mixed.lmer))[16]
      time_rescaled.tval <- coef(summary(mixed.lmer))[17]
      yeardoi.tval <- coef(summary(mixed.lmer))[18]
      age.tval <- coef(summary(mixed.lmer))[19]
      time.yeardoi.tval <- coef(summary(mixed.lmer))[20]
      intercept.pval <- coef(summary(mixed.lmer))[21]
      time_rescaled.pval <- coef(summary(mixed.lmer))[22]
      yeardoi.pval <- coef(summary(mixed.lmer))[23]
      age.pval <- coef(summary(mixed.lmer))[24]
      time.yeardoi.pval <- coef(summary(mixed.lmer))[25]
        
      # Get coefficents of mixed.lmer
      cfit <- coef(summary(mixed.lmer))
  
      # Create temporary data frame
      df <- data.frame(Sex= h, plegia= j,AIS = i, intercept.estimate = cfit[1], time_rescaled.estimate = cfit[2], yeardoi.estimate =cfit[3],
                       age.estimate= cfit[4], time.yeardoi.estimate= cfit[5],intercept.std= cfit[6],
                         time_rescaled.std =cfit[7],
                       yeardoi.std =cfit[8],
                       age.std =cfit[9],
                       time.yeardoi.std =cfit[10],
                       intercept.df =cfit[11],
                       time_rescaled.df =cfit[12],
                       yeardoi.df =cfit[13],
                       age.df =cfit[14],
                       time.yeardoi.df =cfit[15],
                       intercept.tval =cfit[16],
                       time_rescaled.tval =cfit[17],
                       yeardoi.tval =cfit[18],
                       age.tval =cfit[19],
                       time.yeardoi.tval =cfit[20],
                       intercept.pval =cfit[21],
                       time_rescaled.pval =cfit[22],
                       yeardoi.pval =cfit[23],
                       age.pval =cfit[24],
                       time.yeardoi.pval =cfit[25],
                              stringsAsFactors = F)
  
      # Bind rows of temporary data frame to the results data frame
      results <- rbind(results, df)
      
    }
  }
}


# Reformat the data frame created above
new_data <-merged.stack(results,                ## Add the id if it doesn't exist
             var.stubs = c("estimate", "std", "df", "tval", "pval"),   ## Specify the stubs
             sep = "var.stubs",                   ## The sep is just the stubs 
             atStart = FALSE)   

new_data.2 <- as.data.frame(new_data)

# Rename variables
names(new_data.2)[names(new_data.2) == '.time_1'] <- 'Variable'
names(new_data.2)[names(new_data.2) == 'estimate'] <- 'Estimate'
names(new_data.2)[names(new_data.2) == 'std'] <- 'Standard Error'
names(new_data.2)[names(new_data.2) == 'df'] <- 'DF'
names(new_data.2)[names(new_data.2) == 'tval'] <- 't-value'
names(new_data.2)[names(new_data.2) == 'pval'] <- 'p-value'

# Create a new variable based on condition
new_data.2$order[(new_data.2$Variable == 'intercept.')] <- 1
new_data.2$order[(new_data.2$Variable == 'age.')] <- 2
new_data.2$order[(new_data.2$Variable == 'time_rescaled.')] <- 3
new_data.2$order[(new_data.2$Variable == 'yeardoi.')] <- 4
new_data.2$order[(new_data.2$Variable == 'time.yeardoi.')] <- 5

# Create a new variable based on condition
new_data.2$Variable[(new_data.2$Variable == 'intercept.')] <- "Intercept"
new_data.2$Variable[(new_data.2$Variable == 'age.')] <- "Age"
new_data.2$Variable[(new_data.2$Variable == 'time_rescaled.')] <- "Time"
new_data.2$Variable[(new_data.2$Variable == 'yeardoi.')] <- "YEARDOI"
new_data.2$Variable[(new_data.2$Variable == 'time.yeardoi.')] <- "Time*YEARDOI"

# Create a new variable based on condition
new_data.2$model_temp[(new_data.2$Sex == 'f' & new_data.2$plegia == "tetra" & new_data.2$AIS == "A")] <- 'Female:Tetraplegia:AIS A'
new_data.2$model_temp[(new_data.2$Sex == 'f' & new_data.2$plegia == "tetra" & new_data.2$AIS == "B")] <- 'Female:Tetraplegia:AIS B'
new_data.2$model_temp[(new_data.2$Sex == 'f' & new_data.2$plegia == "tetra" & new_data.2$AIS == "C" )] <- 'Female:Tetraplegia:AIS C'
new_data.2$model_temp[(new_data.2$Sex == 'f' & new_data.2$plegia == "tetra" & new_data.2$AIS == "D" )] <- 'Female:Tetraplegia:AIS D'

new_data.2$model_temp[(new_data.2$Sex == 'f' & new_data.2$plegia == "para" & new_data.2$AIS == "A" )] <- 'Female:Paraplegia:AIS A'
new_data.2$model_temp[(new_data.2$Sex == 'f' & new_data.2$plegia == "para" & new_data.2$AIS == "B" )] <- 'Female:Paraplegia:AIS B'
new_data.2$model_temp[(new_data.2$Sex == 'f' & new_data.2$plegia == "para" & new_data.2$AIS == "C" )] <- 'Female:Paraplegia:AIS C'
new_data.2$model_temp[(new_data.2$Sex == 'f' & new_data.2$plegia == "para" & new_data.2$AIS == "D" )] <- 'Female:Paraplegia:AIS D'

new_data.2$model_temp[(new_data.2$Sex == 'm' & new_data.2$plegia == "tetra" & new_data.2$AIS == "A" )] <- 'Male:Tetraplegia:AIS A'
new_data.2$model_temp[(new_data.2$Sex == 'm' & new_data.2$plegia == "tetra" & new_data.2$AIS == "B" )] <- 'Male:Tetraplegia:AIS B'
new_data.2$model_temp[(new_data.2$Sex == 'm' & new_data.2$plegia == "tetra" & new_data.2$AIS == "C" )] <- 'Male:Tetraplegia:AIS C'
new_data.2$model_temp[(new_data.2$Sex == 'm' & new_data.2$plegia == "tetra" & new_data.2$AIS == "D" )] <- 'Male:Tetraplegia:AIS D'

new_data.2$model_temp[(new_data.2$Sex == 'm' & new_data.2$plegia == "para" & new_data.2$AIS == "A")] <- 'Male:Paraplegia:AIS A'
new_data.2$model_temp[(new_data.2$Sex == 'm' & new_data.2$plegia == "para" & new_data.2$AIS == "B" )] <- 'Male:Paraplegia:AIS B'
new_data.2$model_temp[(new_data.2$Sex == 'm' & new_data.2$plegia == "para" & new_data.2$AIS == "C" )] <- 'Male:Paraplegia:AIS C'
new_data.2$model_temp[(new_data.2$Sex == 'm' & new_data.2$plegia == "para" & new_data.2$AIS == "D")] <- 'Male:Paraplegia:AIS D'

# Add adjusted p-value column
new_data.2$Adjusted.pval<- as.numeric(new_data.2$`p-value`)*8

# Rename column
names(new_data.2)[names(new_data.2) == 'Adjusted.pval'] <- 'Adjusted p-value'

# Make t-value, p-value, and Adjusted p-value numeric
new_data.2$`t-value`<-as.numeric(new_data.2$`t-value`)
new_data.2$`p-value`<-as.numeric(new_data.2$`p-value`)
new_data.2$`Adjusted p-value`<-as.numeric(new_data.2$`Adjusted p-value`)

# Function to round to 3 digits
round_df <- function(x, digits) {
# round all numeric variables
# x: data frame 
# digits: number of digits to round
numeric_columns <- sapply(x, mode) == 'numeric'
x[numeric_columns] <-  round(x[numeric_columns], digits)
x
}

new_data_3 <- round_df(new_data.2, 4)

# Sort data
new_data_4 <- arrange(new_data_3,model_temp,order)

# Create a new variable based on condition
new_data_4$Model[(new_data_4$Sex == 'f' & new_data_4$plegia == "tetra" & new_data_4$AIS == "A" & new_data_4$order==1)] <- 'Female: Tetraplegia: AIS A'
new_data_4$Model[(new_data_4$Sex == 'f' & new_data_4$plegia == "tetra" & new_data_4$AIS == "B"& new_data_4$order==1)] <- 'Female: Tetraplegia: AIS B'
new_data_4$Model[(new_data_4$Sex == 'f' & new_data_4$plegia == "tetra" & new_data_4$AIS == "C" & new_data_4$order==1)] <- 'Female: Tetraplegia: AIS C'
new_data_4$Model[(new_data_4$Sex == 'f' & new_data_4$plegia == "tetra" & new_data_4$AIS == "D" & new_data_4$order==1)] <- 'Female: Tetraplegia: AIS D'

new_data_4$Model[(new_data_4$Sex == 'f' & new_data_4$plegia == "para" & new_data_4$AIS == "A" & new_data_4$order==1)] <- 'Female: Paraplegia: AIS A'
new_data_4$Model[(new_data_4$Sex == 'f' & new_data_4$plegia == "para" & new_data_4$AIS == "B" & new_data_4$order==1)] <- 'Female: Paraplegia: AIS B'
new_data_4$Model[(new_data_4$Sex == 'f' & new_data_4$plegia == "para" & new_data_4$AIS == "C" & new_data_4$order==1)] <- 'Female: Paraplegia: AIS C'
new_data_4$Model[(new_data_4$Sex == 'f' & new_data_4$plegia == "para" & new_data_4$AIS == "D" & new_data_4$order==1)] <- 'Female: Paraplegia: AIS D'

new_data_4$Model[(new_data_4$Sex == 'm' & new_data_4$plegia == "tetra" & new_data_4$AIS == "A" & new_data_4$order==1)] <- 'Male: Tetraplegia: AIS A'
new_data_4$Model[(new_data_4$Sex == 'm' & new_data_4$plegia == "tetra" & new_data_4$AIS == "B" & new_data_4$order==1)] <- 'Male: Tetraplegia: AIS B'
new_data_4$Model[(new_data_4$Sex == 'm' & new_data_4$plegia == "tetra" & new_data_4$AIS == "C" & new_data_4$order==1)] <- 'Male: Tetraplegia: AIS C'
new_data_4$Model[(new_data_4$Sex == 'm' & new_data_4$plegia == "tetra" & new_data_4$AIS == "D" & new_data_4$order==1)] <- 'Male: Tetraplegia: AIS D'

new_data_4$Model[(new_data_4$Sex == 'm' & new_data_4$plegia == "para" & new_data_4$AIS == "A"& new_data_4$order==1)] <- 'Male: Paraplegia: AIS A'
new_data_4$Model[(new_data_4$Sex == 'm' & new_data_4$plegia == "para" & new_data_4$AIS == "B" & new_data_4$order==1)] <- 'Male: Paraplegia: AIS B'
new_data_4$Model[(new_data_4$Sex == 'm' & new_data_4$plegia == "para" & new_data_4$AIS == "C" & new_data_4$order==1)] <- 'Male: Paraplegia: AIS C'
new_data_4$Model[(new_data_4$Sex == 'm' & new_data_4$plegia == "para" & new_data_4$AIS == "D"& new_data_4$order==1)] <- 'Male: Paraplegia: AIS D'

# Replace NA with empty cell
new_data_4[is.na(new_data_4)] <- ""
new_data_4[new_data_4 == "<NA>"] <- ""

# Write csv file with only selected columns
write.csv(new_data_4[,c(13,4:9,12)],"/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables/emsci.longitudinal.results_10m.csv", row.names = F)

#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

