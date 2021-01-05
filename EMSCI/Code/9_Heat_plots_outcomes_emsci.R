## ---------------------------
##
## Script name: 9_Heat_plots_outcomes_emsci
##
## Purpose of script: To visualize the change of means of UEMS, LEMS, and TSS using geom_tile.
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2020-11-23
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

## set working directory for Mac and PC

setwd("/Users/jutzca/Documents/GitHub/SCI_Neurological_Recovery/EMSCI") 

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
library(data.table)
library(magrittr)
library(gridExtra)
library(grid)
library(forcats)
library(viridis)

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
# if(!require(data.table)){install.packages("data.table")}
# if(!require(magrittr)){install.packages("magrittr")}
# if(!require(gridExtra)){install.packages("gridExtra")}
# if(!require(grid)){install.packages("grid")}
# if(!require(forcats)){install.packages("forcats")}
# if(!require(viridis)){install.packages("viridis")}

#### ---------------------------
#Set output directorypaths
outdir_figures='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Figures'
outdir_tables='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables'


#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####


#load original dataset
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T,  na.strings=c("","NA"))

#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar
emsci.trauma.sex <- subset(emsci, (AgeAtDOI > 8) & (Sex=='f' | Sex=='m') & 
                             (Cause=="ischemic" | Cause=="traumatic" | Cause=="haemorragic" |Cause=="disc herniation") & 
                             (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar') & (YEARDOI >= 2000) & (AIS=="A"| AIS=="B"| AIS=="C"| AIS=="D"))

emsci.trauma.sex.va.a1<-distinct(subset(emsci.trauma.sex, ExamStage=='acute I' | ExamStage=='very acute') , Patientennummer, .keep_all = TRUE)

# Create new variable: Baseline AIS grade
emsci.trauma.sex.va.a1$baseline.ais <-emsci.trauma.sex.va.a1$AIS

# Merge
emsci.trauma.sex.baseline.ais <-merge(emsci.trauma.sex, emsci.trauma.sex.va.a1[,c(2,243)])

# Change levels of AIS grade and plegia
levels(emsci.trauma.sex.baseline.ais$baseline.ais) <- c("A", "B ", "C", "D", " ", "")
levels(emsci.trauma.sex.baseline.ais$plegia) <- c("Paraplegia", "Tetraplegia ")

emsci.trauma.sex.baseline.ais2 <- emsci.trauma.sex.baseline.ais

#------LEMS EMSCI ----

emsci.trauma.sex.baseline.ais2$LEMS <- as.numeric(as.character(emsci.trauma.sex.baseline.ais2$LEMS))

#Create data frame with mean and sd for lems for plot
new.data.lems =emsci.trauma.sex.baseline.ais2 %>%
  group_by(baseline.ais, X5_year_bins, ExamStage_weeks, plegia) %>%
  dplyr::summarize( 
    n = n(),
    mean_LEMS = mean(LEMS, na.rm=TRUE),
    sd_LEMS = sd(LEMS, na.rm=TRUE))

#Write data file
write.csv(new.data.lems, '/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables/lems.emsci.csv')

#To reverse the order of levels of baseline.ais
new.data.lems$baseline.ais <- factor(new.data.lems$baseline.ais, levels=rev(levels(new.data.lems$baseline.ais)))
levels(new.data.lems$plegia)<-c("Paraplegia\n", 'Tetraplegia\n') 

#Round values to 1 digits
new.data.lems$mean_LEMS <- round(new.data.lems$mean_LEMS,1 )
new.data.lems$mean_LEMS <- round(new.data.lems$mean_LEMS, 1)

#------Plot the data
heatplots.longitudinal_lems.emsci <- ggplot(new.data.lems,aes(x = as.factor(ExamStage_weeks),y = baseline.ais, fill = mean_LEMS)) + 
  geom_tile()+scale_fill_viridis(option = "inferno",  direction = -1, limits = c(-8, 50))+
  facet_grid(as.factor(new.data.lems$X5_year_bins)~new.data.lems$plegia)+theme_economist()+
  geom_text(aes(label=as.numeric(mean_LEMS)), size=3, color='white')+
  labs(title = "Lower Extremity Motor Score (LEMS)", x = "Weeks post injury", y = "AIS Score", fill='LEMS') +
    theme(plot.title = element_text(hjust = 0.5, size= 14),
          axis.title = element_text(size=12),
          axis.text = element_text(size=10),
          strip.text = element_text(size=12), 
          legend.text = element_text(size=10),
          legend.position = 'right',
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
heatplots.longitudinal_lems.emsci 

ggsave(
  "heatplots.longitudinal_lems.emsci.pdf",
  plot = heatplots.longitudinal_lems.emsci,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)

dev.off()

#------UEMS EMSCI ----
emsci.trauma.sex.baseline.ais2$UEMS <- as.numeric(as.character(emsci.trauma.sex.baseline.ais2$UEMS))

#Create data frame with mean and sd for lems
new.data.uems =emsci.trauma.sex.baseline.ais2 %>%
  group_by(baseline.ais, X5_year_bins,ExamStage_weeks, plegia) %>%
  dplyr::summarize( 
    n = n(),
    mean_UEMS = mean(UEMS, na.rm=TRUE),
    sd_UEMS = sd(UEMS, na.rm=TRUE))

#Write data file
write.csv(new.data.uems, '/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables/uems.emsci.csv')

#To reverse the order of levels of baseline.ais
new.data.uems$baseline.ais <- factor(new.data.uems$baseline.ais, levels=rev(levels(new.data.uems$baseline.ais)))
levels(new.data.uems$plegia)<-c("Paraplegia\n", 'Tetraplegia\n') 

#Round values to 1 digits
new.data.uems$mean_UEMS <- round(new.data.uems$mean_UEMS,1 )
new.data.uems$mean_UEMS <- round(new.data.uems$mean_UEMS, 1)

#------Plot the data
uems.emsci.plot <- ggplot(new.data.uems,aes(x = as.factor(ExamStage_weeks),y = baseline.ais,fill = mean_UEMS)) + 
  geom_tile()+scale_fill_viridis(option = "inferno",  direction = -1, limits = c(-8, 50))+
  facet_grid(as.factor(new.data.uems$X5_year_bins)~new.data.uems$plegia)+theme_economist()+
  geom_text(aes(label=as.numeric(mean_UEMS)), size=3, color='white')+
  labs(title = "Upper Extremity Motor Score (UEMS)", x = "Weeks post injury", y = "AIS Score", fill='UEMS') +
  theme(plot.title = element_text(hjust = 0.5, size= 14),
        axis.title = element_text(size=12),
        axis.text = element_text(size=10),
        strip.text = element_text(size=12), 
        legend.text = element_text(size=10),
        legend.position = 'right',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
uems.emsci.plot 

ggsave(
  "longitudinal_uems.emsci.pdf",
  plot = uems.emsci.plot,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)

dev.off()


#------TSS EMSCI ----
emsci.trauma.sex.baseline.ais2$TSS <- as.numeric(as.character(emsci.trauma.sex.baseline.ais2$TSS))

#Create data frame with mean and sd for lems
new.data.tss =emsci.trauma.sex.baseline.ais2 %>%
  group_by(baseline.ais, X5_year_bins,ExamStage_weeks, plegia) %>%
  dplyr::summarize( 
    n = n(),
    mean_TSS = mean(TSS, na.rm=TRUE),
    sd_TSS = sd(TSS, na.rm=TRUE))

#Write data file
write.csv(new.data.tss, '/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables/tss.emsci.csv')

#To reverse the order of levels of baseline.ais
new.data.tss$baseline.ais <- factor(new.data.tss$baseline.ais, levels=rev(levels(new.data.tss$baseline.ais)))
levels(new.data.tss$plegia)<-c("Paraplegia\n", 'Tetraplegia\n') 

#Round values to 1 digits
new.data.tss$mean_TSS <- round(new.data.tss$mean_TSS,1 )
new.data.tss$mean_TSS <- round(new.data.tss$mean_TSS, 1)

#------Plot the data
tss.emsci.plot <- ggplot(new.data.tss,aes(x = as.factor(ExamStage_weeks),y = baseline.ais,fill = mean_TSS)) + 
  geom_tile()+scale_fill_viridis(option = "viridis",  direction = -1, limits = c(0, 180))+
  facet_grid(as.factor(new.data.tss$X5_year_bins)~new.data.tss$plegia)+theme_economist()+
  geom_text(aes(label=as.numeric(mean_TSS)), size=3, color='white')+
  labs(title = "Total Sensory Score (TSS)", x = "Weeks post injury", y = "AIS Score", fill='TSS') +
  theme(plot.title = element_text(hjust = 0.5, size= 14),
        axis.title = element_text(size=12),
        axis.text = element_text(size=10),
        strip.text = element_text(size=12), 
        legend.text = element_text(size=10),
        legend.position = 'right',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
tss.emsci.plot 

ggsave(
  "longitudinal_tss.emsci.pdf",
  plot = tss.emsci.plot,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)

dev.off()






emsci.trauma.sex.ais.baseline2$SCIM23_TotalScore <- as.numeric(emsci.trauma.sex.baseline.ais$SCIM23_TotalScore)


tms.trajectory.merged.plot <- ggplot() +
  stat_summary(aes(x = as.numeric(ExamStage_weeks),y = as.numeric(SCIM23_TotalScore)), 
               data=emsci.trauma.sex.ais.baseline2, 
               fun.data = "mean_sdl", geom="smooth", se = TRUE, size=1)+
  facet_grid(emsci.trauma.sex.ais.baseline2$plegia~emsci.trauma.sex.ais.baseline2$baseline.ais)+
  ylab('Total Motor Score')+xlab("Weeks Post Injury")+
  scale_x_continuous( limits = c(0, 52), breaks = seq(0, 52, 10), expand = c(0,0))+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text.x = element_text(face='bold'))
tms.trajectory.merged.plot


#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

