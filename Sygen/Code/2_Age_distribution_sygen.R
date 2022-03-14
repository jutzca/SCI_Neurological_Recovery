## ---------------------------
##
## Script name: 2_Age_distribution_sygen
##
## Purpose of script: To determine if and to what extent the age at injury changed between 1992-1197.
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
## ---------------------------
## 
## load up the packages we will need:  (uncomment as required)
## 
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
sygen<- read.csv("/Volumes/jutzelec/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/df_sygen_formatted.csv", sep = ',', header = T,  na.strings=c("","NA"))

# Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
sygen.included.cohort.all.times<- subset(sygen, (!is.na(Age)) & (Sex=="Female" | Sex=="Male") & ###Age at DOI and Sex
                                 (NLI == 'cervical' | NLI == 'thoracic')&   ## Neurological level
                                 (AIS=="AIS A"| AIS=="AIS B"| AIS=="AIS C"| AIS=="AIS D")) #AIS Grades

sygen.included.cohort <- distinct(subset(sygen.included.cohort.all.times, Time==0 | Time==1), ID, .keep_all = TRUE)

# Rescale Data
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


#### ---------------------------Age distribution over time: Data analysis ---------------------------

#---------- Calculate the change in age distribution over time - OVERALL --------#
age_model.overall.sygen <-lm(Age~YEARDOI.rescaled*Sex, data=sygen.included.cohort)
summary(age_model.overall.sygen)

#---------- Calculate the change in age distribution over time - OVERALL FEMALE --------#
age_model.overall.female.sygen <-lm(Age~YEARDOI.rescaled, data=subset(sygen.included.cohort, Sex=='Female'))
summary(age_model.overall.female.sygen)

#---------- Calculate the change in age distribution over time - OVERALL MALE --------#
age_model.overall.male.sygen <-lm(Age~YEARDOI.rescaled, data=subset(sygen.included.cohort, Sex=='Male'))
summary(age_model.overall.male.sygen)

#---------- Create a table with summary statistics --------#
tab_model(
  age_model.overall.sygen, age_model.overall.female.sygen, age_model.overall.male.sygen,
  pred.labels = c("Intercept", "Year of injury"),
  dv.labels = c("Overall","Overall Female", "Overall Male"),
  string.pred = "Coeffcient",
  string.ci = "Conf. Int (95%)",
  string.p = "P-Value",
  digits.p = 3
)

#---------- Calculate the change sex distribution over time for subgroups --------#
ais.score.sygen<- unique(sygen.included.cohort$AIS)
rescaled.nli.sygen <- unique(sygen.included.cohort$Plegia)
rescaled.sex.sygen <- unique(sygen.included.cohort$Sex)


#---------- create data frame to store results --------#
results.sygen.age <- data.frame()
  for (j in rescaled.nli.sygen){
    for (i in ais.score.sygen){
      print(paste("MODEL",j, i,  sep = " "))
      df1 = subset(sygen.included.cohort, (AIS == i & Plegia== j))
      mixed.lmer <- lm(Age ~ YEARDOI.rescaled, data = df1, na.action = na.omit)
      print(summary(mixed.lmer))
      
      # Capture summary stats
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
      df <- data.frame(plegia= j,AIS = i, intercept.estimate = cfit[1], YEARDOI.estimate = cfit[2], intercept.std =cfit[3],
                       YEARDOI.std= cfit[4], intercept.tval= cfit[5],YEARDOI.tval= cfit[6],
                       intercept.pval =cfit[7],
                       YEARDOI.pval =cfit[8],
                       stringsAsFactors = F)
      
      #bind rows of temporary data frame to the results data frame
      results.sygen.age <- rbind(results.sygen.age, df)
      
    }
  }

#------Create Table to export --------#

results.sygen.age.new <-merged.stack(results.sygen.age,                ## Add the id if it doesn't exist
                                     var.stubs = c("estimate", "std", "tval", "pval"),   ## Specify the stubs
                                     sep = "var.stubs",                   ## The sep is just the stubs 
                                     atStart = FALSE)   

results.sygen.age.new.df <- as.data.frame(results.sygen.age.new)

# Rename variables
names(results.sygen.age.new.df)[names(results.sygen.age.new.df) == '.time_1'] <- 'Variable'
names(results.sygen.age.new.df)[names(results.sygen.age.new.df) == 'estimate'] <- 'Estimate'
names(results.sygen.age.new.df)[names(results.sygen.age.new.df) == 'std'] <- 'Standard Error'
names(results.sygen.age.new.df)[names(results.sygen.age.new.df) == 'tval'] <- 't-value'
names(results.sygen.age.new.df)[names(results.sygen.age.new.df) == 'pval'] <- 'p-value'

# Create a new variable based on condition
results.sygen.age.new.df$order[(results.sygen.age.new.df$Variable == 'intercept.')] <- 1
results.sygen.age.new.df$order[(results.sygen.age.new.df$Variable == 'YEARDOI.')] <- 2

# Create a new variable based on condition
results.sygen.age.new.df$Variable[(results.sygen.age.new.df$Variable == 'intercept.')] <- "Intercept"
results.sygen.age.new.df$Variable[(results.sygen.age.new.df$Variable == 'YEARDOI.')] <- "YEARDOI"

# Create a new variable based on condition
results.sygen.age.new.df$model_temp[(results.sygen.age.new.df$plegia == "para" & results.sygen.age.new.df$AIS == "AIS A")] <- 'Paraplegia:AIS A'
results.sygen.age.new.df$model_temp[(results.sygen.age.new.df$plegia == "para" & results.sygen.age.new.df$AIS == "AIS B")] <- 'Paraplegia:AIS B'
results.sygen.age.new.df$model_temp[(results.sygen.age.new.df$plegia == "para" & results.sygen.age.new.df$AIS == "AIS C" )] <- 'Paraplegia:AIS C'
results.sygen.age.new.df$model_temp[(results.sygen.age.new.df$plegia == "para" & results.sygen.age.new.df$AIS == "AIS D" )] <- 'Paraplegia:AIS D'

results.sygen.age.new.df$model_temp[(results.sygen.age.new.df$plegia == "tetra" & results.sygen.age.new.df$AIS == "AIS A")] <- 'Tetraplegia:AIS A'
results.sygen.age.new.df$model_temp[(results.sygen.age.new.df$plegia == "tetra" & results.sygen.age.new.df$AIS == "AIS B")] <- 'Tetraplegia:AIS B'
results.sygen.age.new.df$model_temp[(results.sygen.age.new.df$plegia == "tetra" & results.sygen.age.new.df$AIS == "AIS C" )] <- 'Tetraplegia:AIS C'
results.sygen.age.new.df$model_temp[(results.sygen.age.new.df$plegia == "tetra" & results.sygen.age.new.df$AIS == "AIS D" )] <- 'Tetraplegia:AIS D'

# Add adjusted p-value column
results.sygen.age.new.df$Adjusted.pval<- as.numeric(results.sygen.age.new.df$`p-value`)*8

# Rename column
names(results.sygen.age.new.df)[names(results.sygen.age.new.df) == 'Adjusted.pval'] <- 'Adjusted p-value'

# Make t-value, p-value, and Adjusted p-value numeric
results.sygen.age.new.df$`t-value`<-as.numeric(results.sygen.age.new.df$`t-value`)
results.sygen.age.new.df$`p-value`<-as.numeric(results.sygen.age.new.df$`p-value`)
results.sygen.age.new.df$`Adjusted p-value`<-as.numeric(results.sygen.age.new.df$`Adjusted p-value`)

# Function to round to 3 digits
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

results.sygen.age.new.df.2 <- round_df(results.sygen.age.new.df, 3)

# Sort data
results.sygen.age.new.df.3digits <- arrange(results.sygen.age.new.df.2,model_temp,order)

# Create a new variable based on condition
results.sygen.age.new.df.3digits$Model[(results.sygen.age.new.df.3digits$plegia == "tetra" & results.sygen.age.new.df.3digits$AIS == "AIS A" & results.sygen.age.new.df.3digits$order==1)] <- 'Tetraplegia: AIS A'
results.sygen.age.new.df.3digits$Model[(results.sygen.age.new.df.3digits$plegia == "tetra" & results.sygen.age.new.df.3digits$AIS == "AIS B"& results.sygen.age.new.df.3digits$order==1)] <- 'Tetraplegia: AIS B'
results.sygen.age.new.df.3digits$Model[(results.sygen.age.new.df.3digits$plegia == "tetra" & results.sygen.age.new.df.3digits$AIS == "AIS C" & results.sygen.age.new.df.3digits$order==1)] <- 'Tetraplegia: AIS C'
results.sygen.age.new.df.3digits$Model[( results.sygen.age.new.df.3digits$plegia == "tetra" & results.sygen.age.new.df.3digits$AIS == "AIS D" & results.sygen.age.new.df.3digits$order==1)] <- 'Tetraplegia: AIS D'

results.sygen.age.new.df.3digits$Model[(results.sygen.age.new.df.3digits$plegia == "para" & results.sygen.age.new.df.3digits$AIS == "AIS A" & results.sygen.age.new.df.3digits$order==1)] <- 'Paraplegia: AIS A'
results.sygen.age.new.df.3digits$Model[(results.sygen.age.new.df.3digits$plegia == "para" & results.sygen.age.new.df.3digits$AIS == "AIS B" & results.sygen.age.new.df.3digits$order==1)] <- 'Paraplegia: AIS B'
results.sygen.age.new.df.3digits$Model[(results.sygen.age.new.df.3digits$plegia == "para" & results.sygen.age.new.df.3digits$AIS == "AIS C" & results.sygen.age.new.df.3digits$order==1)] <- 'Paraplegia: AIS C'
results.sygen.age.new.df.3digits$Model[(results.sygen.age.new.df.3digits$plegia == "para" & results.sygen.age.new.df.3digits$AIS == "AIS D" & results.sygen.age.new.df.3digits$order==1)] <- 'Paraplegia: AIS D'

# #Replace NA with empty cell
results.sygen.age.new.df.3digits[is.na(results.sygen.age.new.df.3digits)] <- ""
results.sygen.age.new.df.3digits[results.sygen.age.new.df.3digits == "<NA>"] <- ""

# Write csv file with only selected columns
write.csv(results.sygen.age.new.df.3digits[,c(11,3:7,10)],"/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen/Tables/Age_distribution_sygen.csv", row.names = F)


#---------- Age distribution over time: Data analysis --------#

#---------- Visualization: Change in age distribution between 1992-1997 years - OVERALL --------#

# Set theme
theme_set(theme_ridges())

# Change labels
levels(sygen.included.cohort$Sex) <- c("Female", "Male")

# Create plot
age_overall.sygen <- ggplot(
  sygen.included.cohort, 
  aes(y = as.factor(YEARDOI) , x = Age)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 1992 and 1997: Overall') +facet_grid(.~sygen.included.cohort$Sex)+
  xlab("Age at Injury")+ylab("Year of Injury")+
  theme_minimal()+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        #plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text.align = 0)
age_overall.sygen

# Save plot
ggsave(
  "age_overall.sygen.pdf",
  plot = age_overall.sygen,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Visualization: Change in age distribution between 1992-1997 years - PARAPLEGIA --------#

# Set theme
theme_set(theme_ridges())

# Change labels
levels(sygen.included.cohort$Sex) <- c("Female", "Male")

# Subset data
sygen.included.cohort.para <-subset(sygen.included.cohort, Plegia =='para')

# Create plot
age_para.sygen <- ggplot(
  sygen.included.cohort.para, 
  aes(y = as.factor(YEARDOI) , x = Age)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 1992 to 1997: Paraplegic Patients') +facet_grid(.~sygen.included.cohort.para$Sex)+
  xlab("Age at Injury")+ylab("Year of Injury")+
  theme_minimal()+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        #plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text.align = 0)
age_para.sygen

# Save plot
ggsave(
  "age_para.sygen.pdf",
  plot = age_para.sygen,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Visualization: Change in age distribution between 1992-1997 years - TETRAPLEGIA --------#

# Change labels
levels(sygen.included.cohort$Sex) <- c("Female", "Male")

# Subset data
sygen.included.cohort.tetra <-subset(sygen.included.cohort, Plegia =='tetra')

# Create plot
age_tetra.sygen <- ggplot(
  sygen.included.cohort.tetra, 
  aes(y = as.factor(YEARDOI) , x = Age)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 1992 to 1997: Tetraplegic Patients') +facet_grid(.~sygen.included.cohort.tetra$Sex)+
  xlab("Age at Injury")+ylab("Year of Injury")+
  theme_minimal()+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        #plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text.align = 0)
age_tetra.sygen

# Save plot
ggsave(
  "age_tetra.sygen.pdf",
  plot = age_tetra.sygen,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####



