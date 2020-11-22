## ---------------------------
##
## Script name: Longitudinal_analysis_sygen
##
## Purpose of script: To determine if the neurological and functional recovery changed over the course of the Sygen trial (1192-1997).
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
## Notes: Code for the publication XXXX et al., 2021
##   
#### ---------------------------

## set working directory for Mac and PC

setwd("/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen")

## ---------------------------
## load up the packages we will need:  
library(lme4)
library(sjPlot)

## ----------------------------
## Install packages needed:  (uncomment as required)

#if(!require(lme4)){install.packages("lme4")}
#if(!require(sjPlot)){install.packages("sjPlot")}

#### ---------------------------
#Set output directorypaths
outdir_figures='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen/Figures'
outdir_tables='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen/Tables'


#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

#load original dataset
sygen<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/df_sygen_formatted.csv", sep = ',', header = T,  na.strings=c("","NA"))

#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
sygen.included.cohort<- subset(sygen, (!is.na(Age)) & (Sex=="Female" | Sex=="Male") & ###Age at DOI and Sex
                                 (NLI == 'cervical' | NLI == 'thoracic')&   ## Neurological level
                                 (AIS=="AIS A"| AIS=="AIS B"| AIS=="AIS C"| AIS=="AIS D")) #AIS Grades


#install.packages("scales")
library(scales)

rescale.many <- function(dat, column.nos) { 
  nms <- names(dat) 
  for(col in column.nos) { 
    name <- paste(nms[col],".rescaled", sep = "") 
    dat[name] <- rescale(dat[,col], center = TRUE, scale = TRUE) 
  } 
  cat(paste("Rescaled ", length(column.nos),      " variable(s)n")) 
  dat 
} 

rescaled <-rescale.many(sygen.included.cohort, c(4,9:23)) 


library(lmerTest)


ais.score<-unique(rescaled$AIS)
rescaled.nli <- unique(rescaled$NLI)
rescaled.sex <- unique(rescaled$Sex)

# create data frame to store results
results <- data.frame()
for (h in rescaled.sex) {
  for (j in rescaled.nli){
    for (i in ais.score){
      print(paste("MODEL",h,j, i,  sep = " "))
      df1 = subset(rescaled, (AIS == i & NLI== j & Sex == h))
      mixed.lmer <- lmer(TMS ~ as.numeric(Time.rescaled)*YEARDOI.rescaled+Age.rescaled + (1|ID), data = df1)
      print(summary(mixed.lmer))
      
      # ## capture summary stats
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
        
      # get coefficents of mixed.lmer
      cfit <- coef(summary(mixed.lmer))
  
      # # create temporary data frame
      df <- data.frame(Sex= h, NLI= j,ais = i, intercept.estimate = cfit[1], time_rescaled.estimate = cfit[2], yeardoi.estimate =cfit[3],
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
  
      #bind rows of temporary data frame to the results data frame
      results <- rbind(results, df)
      
    }
  }
}



library(splitstackshape)

new_data <-merged.stack(results,                ## Add the id if it doesn't exist
             var.stubs = c("estimate", "std", "df", "tval", "pval"),   ## Specify the stubs
             sep = "var.stubs",                   ## The sep is just the stubs 
             atStart = FALSE)   

new_data.2 <- as.data.frame(new_data)

#Rename variables
names(new_data.2)[names(new_data.2) == '.time_1'] <- 'Variable'
names(new_data.2)[names(new_data.2) == 'estimate'] <- 'Estimate'
names(new_data.2)[names(new_data.2) == 'std'] <- 'Standard Error'
names(new_data.2)[names(new_data.2) == 'df'] <- 'DF'
names(new_data.2)[names(new_data.2) == 'tval'] <- 't-value'
names(new_data.2)[names(new_data.2) == 'pval'] <- 'p-value'

#Create a new variable based on condition
new_data.2$order[(new_data.2$Variable == 'intercept.')] <- 1
new_data.2$order[(new_data.2$Variable == 'age.')] <- 2
new_data.2$order[(new_data.2$Variable == 'time_rescaled.')] <- 3
new_data.2$order[(new_data.2$Variable == 'yeardoi.')] <- 4
new_data.2$order[(new_data.2$Variable == 'time.yeardoi.')] <- 5

#Create a new variable based on condition
new_data.2$Variable[(new_data.2$Variable == 'intercept.')] <- "Intercept"
new_data.2$Variable[(new_data.2$Variable == 'age.')] <- "Age"
new_data.2$Variable[(new_data.2$Variable == 'time_rescaled.')] <- "Time"
new_data.2$Variable[(new_data.2$Variable == 'yeardoi.')] <- "YEARDOI"
new_data.2$Variable[(new_data.2$Variable == 'time.yeardoi.')] <- "Time*YEARDOI"

#Create a new variable based on condition
new_data.2$model_temp[(new_data.2$Sex == 'Female' & new_data.2$NLI == "cervical" & new_data.2$ais == "AIS A")] <- 'Female:Tetraplegia:AIS A'
new_data.2$model_temp[(new_data.2$Sex == 'Female' & new_data.2$NLI == "cervical" & new_data.2$ais == "AIS B")] <- 'Female:Tetraplegia:AIS B'
new_data.2$model_temp[(new_data.2$Sex == 'Female' & new_data.2$NLI == "cervical" & new_data.2$ais == "AIS C" )] <- 'Female:Tetraplegia:AIS C'
new_data.2$model_temp[(new_data.2$Sex == 'Female' & new_data.2$NLI == "cervical" & new_data.2$ais == "AIS D" )] <- 'Female:Tetraplegia:AIS D'

new_data.2$model_temp[(new_data.2$Sex == 'Female' & new_data.2$NLI == "thoracic" & new_data.2$ais == "AIS A" )] <- 'Female:Paraplegia:AIS A'
new_data.2$model_temp[(new_data.2$Sex == 'Female' & new_data.2$NLI == "thoracic" & new_data.2$ais == "AIS B" )] <- 'Female:Paraplegia:AIS B'
new_data.2$model_temp[(new_data.2$Sex == 'Female' & new_data.2$NLI == "thoracic" & new_data.2$ais == "AIS C" )] <- 'Female:Paraplegia:AIS C'
new_data.2$model_temp[(new_data.2$Sex == 'Female' & new_data.2$NLI == "thoracic" & new_data.2$ais == "AIS D" )] <- 'Female:Paraplegia:AIS D'

new_data.2$model_temp[(new_data.2$Sex == 'Male' & new_data.2$NLI == "cervical" & new_data.2$ais == "AIS A" )] <- 'Male:Tetraplegia:AIS A'
new_data.2$model_temp[(new_data.2$Sex == 'Male' & new_data.2$NLI == "cervical" & new_data.2$ais == "AIS B" )] <- 'Male:Tetraplegia:AIS B'
new_data.2$model_temp[(new_data.2$Sex == 'Male' & new_data.2$NLI == "cervical" & new_data.2$ais == "AIS C" )] <- 'Male:Tetraplegia:AIS C'
new_data.2$model_temp[(new_data.2$Sex == 'Male' & new_data.2$NLI == "cervical" & new_data.2$ais == "AIS D" )] <- 'Male:Tetraplegia:AIS D'

new_data.2$model_temp[(new_data.2$Sex == 'Male' & new_data.2$NLI == "thoracic" & new_data.2$ais == "AIS A")] <- 'Male:Paraplegia:AIS A'
new_data.2$model_temp[(new_data.2$Sex == 'Male' & new_data.2$NLI == "thoracic" & new_data.2$ais == "AIS B" )] <- 'Male:Paraplegia:AIS B'
new_data.2$model_temp[(new_data.2$Sex == 'Male' & new_data.2$NLI == "thoracic" & new_data.2$ais == "AIS C" )] <- 'Male:Paraplegia:AIS C'
new_data.2$model_temp[(new_data.2$Sex == 'Male' & new_data.2$NLI == "thoracic" & new_data.2$ais == "AIS D")] <- 'Male:Paraplegia:AIS D'


#Add adjusted p-value column
new_data.2$Adjusted.pval<- as.numeric(new_data.2$`p-value`)*16

#Rename column
names(new_data.2)[names(new_data.2) == 'Adjusted.pval'] <- 'Adjusted p-value'

#Make t-value, p-value, and Adjusted p-value numeric
new_data.2$`t-value`<-as.numeric(new_data.2$`t-value`)
new_data.2$`p-value`<-as.numeric(new_data.2$`p-value`)
new_data.2$`Adjusted p-value`<-as.numeric(new_data.2$`Adjusted p-value`)

#Function to round to 3 digits
round_df <- function(x, digits) {
# round all numeric variables
# x: data frame 
# digits: number of digits to round
numeric_columns <- sapply(x, mode) == 'numeric'
x[numeric_columns] <-  round(x[numeric_columns], digits)
x
}

new_data_3 <- round_df(new_data.2, 4)


#Sort data
new_data_4 <- arrange(new_data_3,model_temp,order)

#Create a new variable based on condition
new_data_4$Model[(new_data_4$Sex == 'Female' & new_data_4$NLI == "cervical" & new_data_4$ais == "AIS A" & new_data_4$order==1)] <- 'Female: Tetraplegia: AIS A'
new_data_4$Model[(new_data_4$Sex == 'Female' & new_data_4$NLI == "cervical" & new_data_4$ais == "AIS B"& new_data_4$order==1)] <- 'Female: Tetraplegia: AIS B'
new_data_4$Model[(new_data_4$Sex == 'Female' & new_data_4$NLI == "cervical" & new_data_4$ais == "AIS C" & new_data_4$order==1)] <- 'Female: Tetraplegia: AIS C'
new_data_4$Model[(new_data_4$Sex == 'Female' & new_data_4$NLI == "cervical" & new_data_4$ais == "AIS D" & new_data_4$order==1)] <- 'Female: Tetraplegia: AIS D'

new_data_4$Model[(new_data_4$Sex == 'Female' & new_data_4$NLI == "thoracic" & new_data_4$ais == "AIS A" & new_data_4$order==1)] <- 'Female: Paraplegia: AIS A'
new_data_4$Model[(new_data_4$Sex == 'Female' & new_data_4$NLI == "thoracic" & new_data_4$ais == "AIS B" & new_data_4$order==1)] <- 'Female: Paraplegia: AIS B'
new_data_4$Model[(new_data_4$Sex == 'Female' & new_data_4$NLI == "thoracic" & new_data_4$ais == "AIS C" & new_data_4$order==1)] <- 'Female: Paraplegia: AIS C'
new_data_4$Model[(new_data_4$Sex == 'Female' & new_data_4$NLI == "thoracic" & new_data_4$ais == "AIS D" & new_data_4$order==1)] <- 'Female: Paraplegia: AIS D'

new_data_4$Model[(new_data_4$Sex == 'Male' & new_data_4$NLI == "cervical" & new_data_4$ais == "AIS A" & new_data_4$order==1)] <- 'Male: Tetraplegia: AIS A'
new_data_4$Model[(new_data_4$Sex == 'Male' & new_data_4$NLI == "cervical" & new_data_4$ais == "AIS B" & new_data_4$order==1)] <- 'Male: Tetraplegia: AIS B'
new_data_4$Model[(new_data_4$Sex == 'Male' & new_data_4$NLI == "cervical" & new_data_4$ais == "AIS C" & new_data_4$order==1)] <- 'Male: Tetraplegia: AIS C'
new_data_4$Model[(new_data_4$Sex == 'Male' & new_data_4$NLI == "cervical" & new_data_4$ais == "AIS D" & new_data_4$order==1)] <- 'Male: Tetraplegia: AIS D'

new_data_4$Model[(new_data_4$Sex == 'Male' & new_data_4$NLI == "thoracic" & new_data_4$ais == "AIS A"& new_data_4$order==1)] <- 'Male: Paraplegia: AIS A'
new_data_4$Model[(new_data_4$Sex == 'Male' & new_data_4$NLI == "thoracic" & new_data_4$ais == "AIS B" & new_data_4$order==1)] <- 'Male: Paraplegia: AIS B'
new_data_4$Model[(new_data_4$Sex == 'Male' & new_data_4$NLI == "thoracic" & new_data_4$ais == "AIS C" & new_data_4$order==1)] <- 'Male: Paraplegia: AIS C'
new_data_4$Model[(new_data_4$Sex == 'Male' & new_data_4$NLI == "thoracic" & new_data_4$ais == "AIS D"& new_data_4$order==1)] <- 'Male: Paraplegia: AIS D'

# #Replace NA with empty cell
new_data_4[is.na(new_data_4)] <- ""
new_data_4[new_data_4 == "<NA>"] <- ""

#Write csv file with only selected columns
write.csv(new_data_4[,c(13,4:9,12)],"/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/results_TMS.csv", row.names = F)

#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####
