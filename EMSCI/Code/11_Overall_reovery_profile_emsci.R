## ---------------------------
##
## Script name: 11_Overall_recovery_profile_emsci
##
## Purpose of script: To visualize the recovery profiles stratified by AIS Scores and plegia. All years will be pooled for this visualization.
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2020-12-21
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

## Clear working space

rm(list=ls())

## set working directory

setwd("/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI") 

## ---------------------------
## load up the packages we will need:  (uncomment as required)
library(ggplot2)
library(grid)

## ----------------------------
## Install packages needed:  (uncomment as required)

#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(grid)){install.packages("grid")}

#### ---------------------------
#Set output directorypaths
outdir_figures='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Figures'
outdir_tables='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables'


#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####


#load original dataset
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T)

#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, level of injury either cervical, thoracic, or lumbar,
#and AIS score A, B, C, or D
emsci.trauma.sex <- subset(emsci, (AgeAtDOI > 8) & (Sex=='f' | Sex=='m') & 
                             (Cause=="ischemic" | Cause=="traumatic" | Cause=="haemorragic" |Cause=="disc herniation") & 
                             (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar') & (YEARDOI >= 2000) &(AIS=='A' | AIS=="B"| AIS=="C"| AIS=="D"))


emsci.trauma.sex.1 <- subset(emsci.trauma.sex, (plegia=='para' | plegia=="tetra"))



#---- Upper extremity motor score ----
emsci.trauma.sex.2 <- subset(emsci.trauma.sex.1, (!is.na(UEMS)))

uems.trajectory.emsci.plot <- ggplot(emsci.trauma.sex.2,aes(x = as.numeric(ExamStage_weeks),y = as.numeric(as.character(UEMS)), group=AIS)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="red", size=0.5)+
  facet_grid(emsci.trauma.sex.2$plegia~emsci.trauma.sex.2$AIS)+
  ylab('Upper Extremity Motor Score')+xlab("Weeks Post Injury")+
  #scale_x_continuous( limits = c(0, 52), breaks = seq(0, 52, 10), expand = c(0,0))+
  #scale_y_continuous( limits = c(0, 120), breaks = seq(0, 120, 20), expand = c(0,0))+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text.x = element_text(face='bold'))
uems.trajectory.emsci.plot


ggsave(
  "uems.trajectory.emsci.plot.pdf",
  plot = uems.trajectory.emsci.plot,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#---- Lower extremity motor score ----
lems.trajectory.emsci.plot <- ggplot(emsci.trauma.sex.1,aes(x = as.numeric(ExamStage_weeks), y= as.numeric(as.character(LEMS)), group=AIS)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="red", size=0.5)+
  facet_grid(emsci.trauma.sex.1$plegia~emsci.trauma.sex.1$AIS)+
  ylab('Lower Extremity Motor Score')+xlab("Weeks Post Injury")+
  #scale_x_continuous( limits = c(0, 52), breaks = seq(0, 52, 10), expand = c(0,0))+
  #scale_y_continuous( limits = c(0, 120), breaks = seq(0, 120, 20), expand = c(0,0))+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text.x = element_text(face='bold'))
lems.trajectory.emsci.plot


ggsave(
  "lems.trajectory.emsci.plot.pdf",
  plot = lems.trajectory.emsci.plot,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---- Total motor score ----
tms.trajectory.emsci.plot <- ggplot(emsci.trauma.sex.1,aes(x = as.numeric(ExamStage_weeks),y = as.numeric(as.character(TMS)), group=AIS)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="red", size=0.5)+
  facet_grid(emsci.trauma.sex.1$plegia~emsci.trauma.sex.1$AIS)+
  ylab('Total Motor Score')+xlab("Weeks Post Injury")+
  #scale_x_continuous( limits = c(0, 52), breaks = seq(0, 52, 10), expand = c(0,0))+
  #scale_y_continuous( limits = c(0, 120), breaks = seq(0, 120, 20), expand = c(0,0))+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text.x = element_text(face='bold'))
tms.trajectory.emsci.plot


ggsave(
  "tms.trajectory.emsci.plot.pdf",
  plot = tms.trajectory.emsci.plot,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()



#---- Total sensory score ----

emsci.trauma.sex.2 <- subset(emsci.trauma.sex.1, (!is.na(TSS)))

tss.trajectory.emsci.plot <- ggplot(emsci.trauma.sex.1,aes(x = as.numeric(ExamStage_weeks),y = as.numeric(as.character(TSS)), group=AIS)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="red", size=0.5)+
  facet_grid(emsci.trauma.sex.1$plegia~emsci.trauma.sex.1$AIS)+
  ylab('Total Sensory Score')+xlab("Weeks Post Injury")+
  #scale_x_continuous( limits = c(0, 52), breaks = seq(0, 52, 10), expand = c(0,0))+
  #scale_y_continuous( limits = c(0, 120), breaks = seq(0, 120, 20), expand = c(0,0))+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text.x = element_text(face='bold'))
tss.trajectory.emsci.plot


ggsave(
  "tms.trajectory.emsci.plot.pdf",
  plot = tms.trajectory.emsci.plot,
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
