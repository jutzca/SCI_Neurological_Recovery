## ---------------------------
##
## Script name: 15_Sensitivity_analysis_country
##
## Purpose of script: To assess if there is a geographical bias.
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2021-01-27
##
## Copyright (c) Catherine Jutzeler, 2021
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
library(ggplot2)
library(grid)
library(ggthemes)
##   
## ----------------------------
##   
## Install packages needed:  (uncomment as required)
##   
#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(grid)){install.packages("grid")}
#if(!require(grid)){install.packages("ggthemes")}
##   
#### ---------------------------
# R Studio Clean-Up:
cat("\014") # clear console
rm(list=ls()) # clear workspace
gc() # garbage collector
##   
#### ---------------------------
## set working directory
setwd("/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI") 
##   
# Set output directorypaths
outdir_figures='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Figures'
outdir_tables='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables'
##   
##   
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

# Load original dataset
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T,  na.strings=c("","NA"))

# Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar
emsci.trauma.sex <- subset(emsci, (AgeAtDOI > 8) & (Sex=='f' | Sex=='m') & 
                             (Cause=="ischemic" | Cause=="traumatic" | Cause=="haemorragic" |Cause=="disc herniation") & 
                             (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar') & (YEARDOI >= 2000) & (AIS=="A"| AIS=="B"| AIS=="C"| AIS=="D"))

emsci.trauma.sex.va.a1<-distinct(subset(emsci.trauma.sex, ExamStage=='acute I' | ExamStage=='very acute') , Patientennummer, .keep_all = TRUE)

# Create new variable: Baseline AIS grade
emsci.trauma.sex.va.a1$baseline.ais <-emsci.trauma.sex.va.a1$AIS

# Merge
emsci.trauma.sex.ais.baseline <-merge(emsci.trauma.sex, emsci.trauma.sex.va.a1[,c(2,243)])

# Change names of levels of AIS grade and plegia
levels(emsci.trauma.sex.ais.baseline$baseline.ais) <- c("AIS-A", "AIS-B", "AIS-C", "AIS-D", " ", "")
levels(emsci.trauma.sex.ais.baseline$plegia) <- c("Paraplegia", "Tetraplegia ")

#---------- Total Motor Score --------#

# Generate plot
longitudinal.trajectory.tms.geography <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline,aes(x=ExamStage_weeks, y=as.numeric(as.character(TMS))), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline$plegia~emsci.trauma.sex.ais.baseline$Country, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Total Motor Score')+xlab("Time since injury [weeks]")+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        plot.title = element_text(size=12, hjust=0.5, face="bold", family='Times'),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.tms.geography

# Save plot
ggsave(
  "longitudinal.trajectory.tms.geography.pdf",
  plot = longitudinal.trajectory.tms.geography,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Total Sensory Score --------#

# Generate plot
longitudinal.trajectory.tss.geography <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline,aes(x=ExamStage_weeks, y=as.numeric(as.character(TSS)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline$plegia~emsci.trauma.sex.ais.baseline$Country, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Total Sensory Score')+xlab("Time since injury [weeks]")+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        plot.title = element_text(size=12, hjust=0.5, face="bold", family='Times'),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.tss.geography

# Save plot
ggsave(
  "longitudinal.trajectory.tss.geography.pdf",
  plot = longitudinal.trajectory.tss.geography,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

