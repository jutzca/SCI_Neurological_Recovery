## ---------------------------
##
## Script name: Age_distribution_emsci
##
## Purpose of script: To determine if and to what extent the age at injury changed between 2001 and 2019.
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

#### ---------- Data wrangling ---------

# Load original dataset
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T,  na.strings=c("","NA"))


# Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
emsci.trauma.sex <- subset(emsci, (AgeAtDOI > 8) & (Sex=='f' | Sex=='m') & ###Age at DOI and Sex
                             (Cause=="ischemic" | Cause=="traumatic" | Cause=="haemorragic" |Cause=="disc herniation") & 
                             (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar')&   ## Neurological level
                             (AIS=="A"| AIS=="B"| AIS=="C"| AIS=="D")) #AIS Grades


#### ---------- Data analysis and visualization ---------
# Subset data to only patients with valid entry at stage 'very acute' or 'acute I' and remove duplicate patient numbers
emsci.trauma.sex.va.a1<-distinct(subset(emsci.trauma.sex, ExamStage=='acute I' | ExamStage=='very acute') , Patientennummer, .keep_all = TRUE)

#---------- Rescale Data ---------#
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


#### ---------------------------Age distribution over time: Data analysis ---------------------------

#---------- Calculate the mean and sd of age per year --------#
age.year <- emsci.trauma.sex.va.a1 %>%
  dplyr::select(YEARDOI, AgeAtDOI, Sex)%>%
  dplyr::group_by(YEARDOI, Sex) %>%
  dplyr::mutate(mean.age =sprintf("%0.2f", mean(AgeAtDOI)))%>%
  dplyr::mutate(sd.age =sprintf("%0.2f", sd(AgeAtDOI)))%>%
  dplyr::select(YEARDOI, Sex, mean.age,sd.age)%>%
  dplyr::distinct()%>%
  dplyr::arrange(Sex, YEARDOI)%>%
  as.data.frame()

write.csv(age.year,"/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/mean_age_per_year.csv" )

#---------- Calculate the change in age distribution over time - OVERALL --------#
age_model.overall <-lm(AgeAtDOI~YEARDOI.rescaled, data=emsci.trauma.sex.va.a1)
summary(age_model.overall)
nobs(age_model.overall)

#---------- Calculate the change in age distribution over time - OVERALL FEMALE --------#
age_model.overall.female <-lm(AgeAtDOI~YEARDOI.rescaled, data=subset(emsci.trauma.sex.va.a1, Sex=='f'))
summary(age_model.overall.female)
nobs(age_model.overall.female)

#---------- Calculate the change in age distribution over time - OVERALL MALE --------#
age_model.overall.male <-lm(AgeAtDOI~YEARDOI.rescaled, data=subset(emsci.trauma.sex.va.a1, Sex=='m'))
summary(age_model.overall.male)
nobs(age_model.overall.male)

# Create table with model summary
tab_model(
  age_model.overall, age_model.overall.female, age_model.overall.male,
  pred.labels = c("Intercept", "Year of injury"),
  dv.labels = c("Overall","Overall Female", "Overall Male"),
  string.pred = "Coeffcient",
  string.ci = "Conf. Int (95%)",
  string.p = "P-Value",
  digits.p = 3
)

#---------- Calculate the change sex distribution over time for subgroups --------#
ais.score<- unique(emsci.trauma.sex.va.a1$AIS)
rescaled.nli <- unique(emsci.trauma.sex.va.a1$plegia)
rescaled.sex <- unique(emsci.trauma.sex.va.a1$Sex)

# Create data frame to store results
results.emsci.age <- data.frame()
for (h in rescaled.sex) {
  for (j in rescaled.nli){
    for (i in ais.score){
      print(paste("MODEL",h,j, i,  sep = " "))
      df1 = subset(emsci.trauma.sex.va.a1, (AIS == i & plegia== j & Sex == h))
      mixed.lmer <- lm(AgeAtDOI ~ YEARDOI.rescaled, data = df1, na.action = na.omit)
      print(summary(mixed.lmer))
      
      n <- nobs(mixed.lmer)
      
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
      results.emsci.age <- rbind(results.emsci.age, df2)
      
    }
  }
}


#------ Create table to export  --------#

results.emsci.age.new <-merged.stack(results.emsci.age,                ## Add the id if it doesn't exist
                        var.stubs = c("estimate", "std", "tval", "pval"),   ## Specify the stubs
                        sep = "var.stubs",                   ## The sep is just the stubs 
                        atStart = FALSE)   

results.emsci.age.new.df <- as.data.frame(results.emsci.age.new)

# Rename variables
names(results.emsci.age.new.df)[names(results.emsci.age.new.df) == '.time_1'] <- 'Variable'
names(results.emsci.age.new.df)[names(results.emsci.age.new.df) == 'estimate'] <- 'Estimate'
names(results.emsci.age.new.df)[names(results.emsci.age.new.df) == 'std'] <- 'Standard Error'
names(results.emsci.age.new.df)[names(results.emsci.age.new.df) == 'tval'] <- 't-value'
names(results.emsci.age.new.df)[names(results.emsci.age.new.df) == 'pval'] <- 'p-value'

# Create a new variable based on condition
results.emsci.age.new.df$order[(results.emsci.age.new.df$Variable == 'intercept.')] <- 1
results.emsci.age.new.df$order[(results.emsci.age.new.df$Variable == 'YEARDOI.')] <- 2

# Create a new variable based on condition
results.emsci.age.new.df$Variable[(results.emsci.age.new.df$Variable == 'intercept.')] <- "Intercept"
results.emsci.age.new.df$Variable[(results.emsci.age.new.df$Variable == 'YEARDOI.')] <- "YEARDOI"

# Create a new variable based on condition
results.emsci.age.new.df$model_temp[(results.emsci.age.new.df$Sex == 'f' & results.emsci.age.new.df$plegia == "para" & results.emsci.age.new.df$AIS == "A")] <- 'Female:Paraplegia:AIS A'
results.emsci.age.new.df$model_temp[(results.emsci.age.new.df$Sex == 'f' & results.emsci.age.new.df$plegia == "para" & results.emsci.age.new.df$AIS == "B")] <- 'Female:Paraplegia:AIS B'
results.emsci.age.new.df$model_temp[(results.emsci.age.new.df$Sex == 'f' & results.emsci.age.new.df$plegia == "para" & results.emsci.age.new.df$AIS == "C" )] <- 'Female:Paraplegia:AIS C'
results.emsci.age.new.df$model_temp[(results.emsci.age.new.df$Sex == 'f' & results.emsci.age.new.df$plegia == "para" & results.emsci.age.new.df$AIS == "D" )] <- 'Female:Paraplegia:AIS D'

results.emsci.age.new.df$model_temp[(results.emsci.age.new.df$Sex == 'f' & results.emsci.age.new.df$plegia == "tetra" & results.emsci.age.new.df$AIS == "A")] <- 'Female:Tetraplegia:AIS A'
results.emsci.age.new.df$model_temp[(results.emsci.age.new.df$Sex == 'f' & results.emsci.age.new.df$plegia == "tetra" & results.emsci.age.new.df$AIS == "B")] <- 'Female:Tetraplegia:AIS B'
results.emsci.age.new.df$model_temp[(results.emsci.age.new.df$Sex == 'f' & results.emsci.age.new.df$plegia == "tetra" & results.emsci.age.new.df$AIS == "C" )] <- 'Female:Tetraplegia:AIS C'
results.emsci.age.new.df$model_temp[(results.emsci.age.new.df$Sex == 'f' & results.emsci.age.new.df$plegia == "tetra" & results.emsci.age.new.df$AIS == "D" )] <- 'Female:Tetraplegia:AIS D'

results.emsci.age.new.df$model_temp[(results.emsci.age.new.df$Sex == 'm' & results.emsci.age.new.df$plegia == "para" & results.emsci.age.new.df$AIS == "A")] <- 'Male:Paraplegia:AIS A'
results.emsci.age.new.df$model_temp[(results.emsci.age.new.df$Sex == 'm' & results.emsci.age.new.df$plegia == "para" & results.emsci.age.new.df$AIS == "B")] <- 'Male:Paraplegia:AIS B'
results.emsci.age.new.df$model_temp[(results.emsci.age.new.df$Sex == 'm' & results.emsci.age.new.df$plegia == "para" & results.emsci.age.new.df$AIS == "C" )] <- 'Male:Paraplegia:AIS C'
results.emsci.age.new.df$model_temp[(results.emsci.age.new.df$Sex == 'm' & results.emsci.age.new.df$plegia == "para" & results.emsci.age.new.df$AIS == "D" )] <- 'Male:Paraplegia:AIS D'

results.emsci.age.new.df$model_temp[(results.emsci.age.new.df$Sex == 'm' & results.emsci.age.new.df$plegia == "tetra" & results.emsci.age.new.df$AIS == "A")] <- 'Male:Tetraplegia:AIS A'
results.emsci.age.new.df$model_temp[(results.emsci.age.new.df$Sex == 'm' & results.emsci.age.new.df$plegia == "tetra" & results.emsci.age.new.df$AIS == "B")] <- 'Male:Tetraplegia:AIS B'
results.emsci.age.new.df$model_temp[(results.emsci.age.new.df$Sex == 'm' & results.emsci.age.new.df$plegia == "tetra" & results.emsci.age.new.df$AIS == "C" )] <- 'Male:Tetraplegia:AIS C'
results.emsci.age.new.df$model_temp[(results.emsci.age.new.df$Sex == 'm' & results.emsci.age.new.df$plegia == "tetra" & results.emsci.age.new.df$AIS == "D" )] <- 'Male:Tetraplegia:AIS D'


# Add adjusted p-value column
results.emsci.age.new.df$Adjusted.pval<- as.numeric(results.emsci.age.new.df$`p-value`)*16

# Rename column
names(results.emsci.age.new.df)[names(results.emsci.age.new.df) == 'Adjusted.pval'] <- 'Adjusted p-value'

# Make t-value, p-value, and Adjusted p-value numeric
results.emsci.age.new.df$`t-value`<-as.numeric(results.emsci.age.new.df$`t-value`)
results.emsci.age.new.df$`p-value`<-as.numeric(results.emsci.age.new.df$`p-value`)
results.emsci.age.new.df$`Adjusted p-value`<-as.numeric(results.emsci.age.new.df$`Adjusted p-value`)

# Function to round to 3 digits
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

results.emsci.age.new.df.2 <- round_df(results.emsci.age.new.df, 3)

# Sort data
results.emsci.age.new.df.3digits <- arrange(results.emsci.age.new.df.2,model_temp,order)

#Create a new variable based on condition
results.emsci.age.new.df.3digits$Model[(results.emsci.age.new.df.3digits$Sex == 'f' & results.emsci.age.new.df.3digits$plegia == "tetra" & results.emsci.age.new.df.3digits$AIS == "A" & results.emsci.age.new.df.3digits$order==1)] <- 'Female: Tetraplegia: AIS A'
results.emsci.age.new.df.3digits$Model[(results.emsci.age.new.df.3digits$Sex == 'f' & results.emsci.age.new.df.3digits$plegia == "tetra" & results.emsci.age.new.df.3digits$AIS == "B"& results.emsci.age.new.df.3digits$order==1)] <- 'Female: Tetraplegia: AIS B'
results.emsci.age.new.df.3digits$Model[(results.emsci.age.new.df.3digits$Sex == 'f' & results.emsci.age.new.df.3digits$plegia == "tetra" & results.emsci.age.new.df.3digits$AIS == "C" & results.emsci.age.new.df.3digits$order==1)] <- 'Female: Tetraplegia: AIS C'
results.emsci.age.new.df.3digits$Model[(results.emsci.age.new.df.3digits$Sex == 'f' & results.emsci.age.new.df.3digits$plegia == "tetra" & results.emsci.age.new.df.3digits$AIS == "D" & results.emsci.age.new.df.3digits$order==1)] <- 'Female: Tetraplegia: AIS D'

results.emsci.age.new.df.3digits$Model[(results.emsci.age.new.df.3digits$Sex == 'f' & results.emsci.age.new.df.3digits$plegia == "para" & results.emsci.age.new.df.3digits$AIS == "A" & results.emsci.age.new.df.3digits$order==1)] <- 'Female: Paraplegia: AIS A'
results.emsci.age.new.df.3digits$Model[(results.emsci.age.new.df.3digits$Sex == 'f' & results.emsci.age.new.df.3digits$plegia == "para" & results.emsci.age.new.df.3digits$AIS == "B" & results.emsci.age.new.df.3digits$order==1)] <- 'Female: Paraplegia: AIS B'
results.emsci.age.new.df.3digits$Model[(results.emsci.age.new.df.3digits$Sex == 'f' & results.emsci.age.new.df.3digits$plegia == "para" & results.emsci.age.new.df.3digits$AIS == "C" & results.emsci.age.new.df.3digits$order==1)] <- 'Female: Paraplegia: AIS C'
results.emsci.age.new.df.3digits$Model[(results.emsci.age.new.df.3digits$Sex == 'f' & results.emsci.age.new.df.3digits$plegia == "para" & results.emsci.age.new.df.3digits$AIS == "D" & results.emsci.age.new.df.3digits$order==1)] <- 'Female: Paraplegia: AIS D'

results.emsci.age.new.df.3digits$Model[(results.emsci.age.new.df.3digits$Sex == 'm' & results.emsci.age.new.df.3digits$plegia == "tetra" & results.emsci.age.new.df.3digits$AIS == "A" & results.emsci.age.new.df.3digits$order==1)] <- 'Male: Tetraplegia: AIS A'
results.emsci.age.new.df.3digits$Model[(results.emsci.age.new.df.3digits$Sex == 'm' & results.emsci.age.new.df.3digits$plegia == "tetra" & results.emsci.age.new.df.3digits$AIS == "B" & results.emsci.age.new.df.3digits$order==1)] <- 'Male: Tetraplegia: AIS B'
results.emsci.age.new.df.3digits$Model[(results.emsci.age.new.df.3digits$Sex == 'm' & results.emsci.age.new.df.3digits$plegia == "tetra" & results.emsci.age.new.df.3digits$AIS == "C" & results.emsci.age.new.df.3digits$order==1)] <- 'Male: Tetraplegia: AIS C'
results.emsci.age.new.df.3digits$Model[(results.emsci.age.new.df.3digits$Sex == 'm' & results.emsci.age.new.df.3digits$plegia == "tetra" & results.emsci.age.new.df.3digits$AIS == "D" & results.emsci.age.new.df.3digits$order==1)] <- 'Male: Tetraplegia: AIS D'

results.emsci.age.new.df.3digits$Model[(results.emsci.age.new.df.3digits$Sex == 'm' & results.emsci.age.new.df.3digits$plegia == "para" & results.emsci.age.new.df.3digits$AIS == "A"& results.emsci.age.new.df.3digits$order==1)] <- 'Male: Paraplegia: AIS A'
results.emsci.age.new.df.3digits$Model[(results.emsci.age.new.df.3digits$Sex == 'm' & results.emsci.age.new.df.3digits$plegia == "para" & results.emsci.age.new.df.3digits$AIS == "B" & results.emsci.age.new.df.3digits$order==1)] <- 'Male: Paraplegia: AIS B'
results.emsci.age.new.df.3digits$Model[(results.emsci.age.new.df.3digits$Sex == 'm' & results.emsci.age.new.df.3digits$plegia == "para" & results.emsci.age.new.df.3digits$AIS == "C" & results.emsci.age.new.df.3digits$order==1)] <- 'Male: Paraplegia: AIS C'
results.emsci.age.new.df.3digits$Model[(results.emsci.age.new.df.3digits$Sex == 'm' & results.emsci.age.new.df.3digits$plegia == "para" & results.emsci.age.new.df.3digits$AIS == "D"& results.emsci.age.new.df.3digits$order==1)] <- 'Male: Paraplegia: AIS D'

# #Replace NA with empty cell
results.emsci.age.new.df.3digits[is.na(results.emsci.age.new.df.3digits)] <- ""
results.emsci.age.new.df.3digits[results.emsci.age.new.df.3digits == "<NA>"] <- ""

# Write csv file with only selected columns
write.csv(results.emsci.age.new.df.3digits[,c(13,4:9,12)],"/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables/Age_distribution_emsci.csv", row.names = F)
names(results.emsci.age.new.df.3digits)

#---------- Visualization: Change in age distribution over 20 years - OVERALL --------#
# Set theme
theme_set(theme_ridges())

# Change labels of levels 
levels(emsci.trauma.sex.va.a1$Sex) <- c("Female", "Male")

# Create plot
age_overall <- ggplot(
  emsci.trauma.sex.va.a1, 
  aes(y = as.factor(YEARDOI) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 2001 and 2019: Overall') +facet_grid(.~emsci.trauma.sex.va.a1$Sex)+
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

age_overall

ggsave(
  "age.overall.emsci.pdf",
  plot = age_overall,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Visualization: Change in age distribution over 20 years - PARAPLEGIA --------#
# Set theme
theme_set(theme_ridges())

# Subset data
emsci.trauma.sex.va.a1.para <-subset(emsci.trauma.sex.va.a1, plegia =='para')

# Change labels of levels 
levels(emsci.trauma.sex.va.a1.para$Sex) <- c("Female", "Male")

# Create plot
age_para <- ggplot(
  emsci.trauma.sex.va.a1.para, 
  aes(y = as.factor(YEARDOI) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 2001 and 2019: Paraplegic Patients') +facet_grid(.~emsci.trauma.sex.va.a1.para$Sex)+
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

age_para

ggsave(
  "age.para.emsci.pdf",
  plot = age_para,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Visualization: Change in age distribution over 20 years - TETRAPLEGIA --------#
# Set theme
theme_set(theme_ridges())

# Subset data
emsci.trauma.sex.va.a1.tetra <-subset(emsci.trauma.sex.va.a1, plegia =='tetra')

# Change labels of levels 
levels(emsci.trauma.sex.va.a1.tetra$Sex) <- c("Female", "Male")

# Create plot
age_tetra <- ggplot(
  emsci.trauma.sex.va.a1.tetra, 
  aes(y = as.factor(YEARDOI) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 2001 and 2019: Tetraplegic Patients') +facet_grid(.~emsci.trauma.sex.va.a1.tetra$Sex)+
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

age_tetra

ggsave(
  "age.tetra.emsci.pdf",
  plot = age_tetra,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Visualization: Change in age distribution over 20 years - AIS A --------#
# Set theme
theme_set(theme_ridges())

#Subset data
emsci.trauma.sex.va.a1.ais_a <-subset(emsci.trauma.sex.va.a1, AIS =='A')

# Change labels of levels 
levels(emsci.trauma.sex.va.a1.ais_a$Sex) <- c("Female", "Male")

# Create plot
age_ais_a <- ggplot(
  emsci.trauma.sex.va.a1.ais_a, 
  aes(y = as.factor(YEARDOI) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 2001 and 2019: AIS A Patients') +facet_grid(.~emsci.trauma.sex.va.a1.ais_a$Sex)+
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

age_ais_a

ggsave(
  "age.ais_a.emsci.pdf",
  plot = age_ais_a,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()



#---------- Visualization: Change in age distribution over 20 years - AIS B --------#
# Set theme
theme_set(theme_ridges())

#Subset data
emsci.trauma.sex.va.a1.ais_b <-subset(emsci.trauma.sex.va.a1, AIS =='B')

# Change labels of levels 
levels(emsci.trauma.sex.va.a1.ais_b$Sex) <- c("Female", "Male")

# Create plot
age_ais_b <- ggplot(
  emsci.trauma.sex.va.a1.ais_b, 
  aes(y = as.factor(YEARDOI) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 2001 and 2019: AIS B Patients') +facet_grid(.~emsci.trauma.sex.va.a1.ais_b$Sex)+
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

age_ais_b

ggsave(
  "age.ais_b.emsci.pdf",
  plot = age_ais_b,
  device = 'pdf',
  path = outdir_figures,    
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Visualization: Change in age distribution over 20 years - AIS C --------#
# Set theme
theme_set(theme_ridges())

#Subset data
emsci.trauma.sex.va.a1.ais_c <-subset(emsci.trauma.sex.va.a1, AIS =='C')

# Change labels of levels 
levels(emsci.trauma.sex.va.a1.ais_c$Sex) <- c("Female", "Male")

# Create plot
age_ais_c <- ggplot(
  emsci.trauma.sex.va.a1.ais_c, 
  aes(y = as.factor(YEARDOI) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 2001 and 2019: AIS C Patients') +facet_grid(.~emsci.trauma.sex.va.a1.ais_c$Sex)+
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

age_ais_c

ggsave(
  "age.ais_c.emsci.pdf",
  plot = age_ais_c,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()



#---------- Visualization: Change in age distribution over 20 years - AIS D --------#
# Set theme
theme_set(theme_ridges())

#Subset data
emsci.trauma.sex.va.a1.ais_d <-subset(emsci.trauma.sex.va.a1, AIS =='D')

# Change labels of levels 
levels(emsci.trauma.sex.va.a1.ais_d$Sex) <- c("Female", "Male")

# Create plot
age_ais_d <- ggplot(
  emsci.trauma.sex.va.a1.ais_d, 
  aes(y = as.factor(YEARDOI) , x = AgeAtDOI)
) + geom_density_ridges_gradient(
  aes(fill = ..x..), scale = 3, size = 0.3
) + scale_fill_gradientn(
  #colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
  #colours = c("#8C3F4D","#3E606F"),
  name = "Age [years]"
)+
  labs(title = 'Age at Injury between 2001 and 2019: AIS D Patients') +facet_grid(.~emsci.trauma.sex.va.a1.ais_d$Sex)+
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

age_ais_d

ggsave(
  "age.ais_d.emsci.pdf",
  plot = age_ais_d,
  device = 'pdf',
  path = outdir_figures,    
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()
#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

