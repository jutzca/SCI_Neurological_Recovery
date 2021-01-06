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
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T,  na.strings=c("","NA"))

#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar
emsci.trauma.sex <- subset(emsci, (AgeAtDOI > 8) & (Sex=='f' | Sex=='m') & 
                             (Cause=="ischemic" | Cause=="traumatic" | Cause=="haemorragic" |Cause=="disc herniation") & 
                             (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar') & (YEARDOI >= 2000) & (AIS=="A"| AIS=="B"| AIS=="C"| AIS=="D"))

emsci.trauma.sex.va.a1<-distinct(subset(emsci.trauma.sex, ExamStage=='acute I' | ExamStage=='very acute') , Patientennummer, .keep_all = TRUE)

# Create new variable: Baseline AIS grade
emsci.trauma.sex.va.a1$baseline.ais <-emsci.trauma.sex.va.a1$AIS

# Merge
emsci.trauma.sex.ais.baseline <-merge(emsci.trauma.sex, emsci.trauma.sex.va.a1[,c(2,243)])


# Change levels of AIS grade and plegia
levels(emsci.trauma.sex.ais.baseline$baseline.ais) <- c("AIS-A", "AIS-B ", "AIS-C", "AIS-D", " ", "")
levels(emsci.trauma.sex.ais.baseline$plegia) <- c("Paraplegia", "Tetraplegia")


#---- Upper extremity motor score ----
emsci.trauma.sex.ais.baseline.rm.na <- subset(emsci.trauma.sex.ais.baseline, (!is.na(UEMS)))

emsci.trauma.sex.ais.baseline.rm.na.para <- subset(emsci.trauma.sex.ais.baseline.rm.na, plegia=="Tetraplegia")

overall.uems.recovery.trajectory.emsci.plot <- ggplot(emsci.trauma.sex.ais.baseline.rm.na.para,aes(x = as.numeric(ExamStage_weeks),y = as.numeric(as.character(UEMS)), group=baseline.ais)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="red", size=0.5)+
  facet_grid(emsci.trauma.sex.ais.baseline.rm.na.para$plegia~emsci.trauma.sex.ais.baseline.rm.na.para$baseline.ais)+
  ylab('Upper Extremity Motor Score')+xlab("Weeks Post Injury")+
  #scale_x_continuous( limits = c(0, 52), breaks = seq(0, 52, 10), expand = c(0,0))+
  #scale_y_continuous( limits = c(0, 120), breaks = seq(0, 120, 20), expand = c(0,0))+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text = element_text(face='plain', size=10, family='Times'),axis.title = element_text(face='bold', size=12, family='Times'), 
        strip.text = element_text(face='bold', size=10, family='Times'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"))
overall.uems.recovery.trajectory.emsci.plot


ggsave(
  "overall.uems.recovery.trajectory.emsci.plot.pdf",
  plot = overall.uems.recovery.trajectory.emsci.plot,
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
overall.lems.recovery.trajectory.emsci.plot <- ggplot(emsci.trauma.sex.ais.baseline.rm.na,aes(x = as.numeric(ExamStage_weeks), y= as.numeric(as.character(LEMS)), group=baseline.ais)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="red", size=0.5)+
  facet_grid(emsci.trauma.sex.ais.baseline.rm.na$plegia~emsci.trauma.sex.ais.baseline.rm.na$baseline.ais)+
  ylab('Lower Extremity Motor Score')+xlab("Weeks Post Injury")+
  #scale_x_continuous( limits = c(0, 52), breaks = seq(0, 52, 10), expand = c(0,0))+
  #scale_y_continuous( limits = c(0, 120), breaks = seq(0, 120, 20), expand = c(0,0))+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text = element_text(face='plain', size=10, family='Times'),axis.title = element_text(face='bold', size=12, family='Times'), 
        strip.text = element_text(face='bold', size=10, family='Times'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"))
overall.lems.recovery.trajectory.emsci.plot


ggsave(
  "overall.lems.recovery.trajectory.emsci.plot.pdf",
  plot = overall.lems.recovery.trajectory.emsci.plot,
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
overall.tms.recovery.trajectory.emsci.plot <- ggplot(emsci.trauma.sex.ais.baseline.rm.na,aes(x = as.numeric(ExamStage_weeks),y = as.numeric(as.character(TMS)), group=baseline.ais)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="red", size=0.5)+
  facet_grid(emsci.trauma.sex.ais.baseline.rm.na$plegia~emsci.trauma.sex.ais.baseline.rm.na$baseline.ais)+
  ylab('Total Motor Score')+xlab("Weeks Post Injury")+
  #scale_x_continuous( limits = c(0, 52), breaks = seq(0, 52, 10), expand = c(0,0))+
  #scale_y_continuous( limits = c(0, 120), breaks = seq(0, 120, 20), expand = c(0,0))+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text = element_text(face='plain', size=10, family='Times'),axis.title = element_text(face='bold', size=12, family='Times'), 
        strip.text = element_text(face='bold', size=10, family='Times'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"))
overall.tms.recovery.trajectory.emsci.plot


ggsave(
  "overall.tms.recovery.trajectory.emsci.plot.pdf",
  plot = overall.tms.recovery.trajectory.emsci.plot,
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

overall.tss.recovery.trajectory.emsci.plot <- ggplot(emsci.trauma.sex.ais.baseline.rm.na,aes(x = as.numeric(ExamStage_weeks),y = as.numeric(as.character(TSS)), group=baseline.ais)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="red", size=0.5)+
  facet_grid(emsci.trauma.sex.ais.baseline.rm.na$plegia~emsci.trauma.sex.ais.baseline.rm.na$baseline.ais)+
  ylab('Total Sensory Score')+xlab("Weeks Post Injury")+
  #scale_x_continuous( limits = c(0, 52), breaks = seq(0, 52, 10), expand = c(0,0))+
  #scale_y_continuous( limits = c(0, 120), breaks = seq(0, 120, 20), expand = c(0,0))+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text = element_text(face='plain', size=10, family='Times'),axis.title = element_text(face='bold', size=12, family='Times'), 
        strip.text = element_text(face='bold', size=10, family='Times'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"))
overall.tss.recovery.trajectory.emsci.plot


ggsave(
  "overall.tss.recovery.trajectory.emsci.plot.pdf",
  plot = overall.tss.recovery.trajectory.emsci.plot,
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
