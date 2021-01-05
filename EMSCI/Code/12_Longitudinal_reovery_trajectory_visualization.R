## ---------------------------
##
## Script name: 12_Longitudinal_recovery_trajectory_visualization
##
## Purpose of script: To visualize the longitudinal changes in functional and neurological outcomes after spinal cord injury and to compare the recovery trajectories between 2001 and 2019.
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
#### ---------------------------

## Clear working space

rm(list=ls())

## set working directory

setwd("/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI") 

## ---------------------------
## load up the packages we will need:  (uncomment as required)
library(ggplot2)
library(grid)
library(ggthemes)

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
levels(emsci.trauma.sex.ais.baseline$plegia) <- c("Paraplegia", "Tetraplegia ")


#-----Total Motor Score----
longitudinal.trajectory.tms <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline,aes(x=ExamStage_weeks, y=as.numeric(as.character(TMS)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline$plegia~emsci.trauma.sex.ais.baseline$baseline.ais, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Lower Extremity Motor Score')+xlab("Time since injury [weeks]")+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.tms


## Save plot
ggsave(
  "longitudinal.trajectory.tms.pdf",
  plot = longitudinal.trajectory.tms,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#-----Total Sensory Score----
longitudinal.trajectory.tss <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline,aes(x=ExamStage_weeks, y=as.numeric(as.character(TSS)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline$plegia~emsci.trauma.sex.ais.baseline$baseline.ais, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Total Sensory Score')+xlab("Time since injury [weeks]")+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.tss


## Save plot
ggsave(
  "longitudinal.trajectory.tss.pdf",
  plot = longitudinal.trajectory.tss,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#-----Lower Extremity Motor Score----
longitudinal.trajectory.lems <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline,aes(x=ExamStage_weeks, y=as.numeric(as.character(LEMS)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline$plegia~emsci.trauma.sex.ais.baseline$baseline.ais, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Lower Extremity Motor Score')+xlab("Time since injury [weeks]")+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
          axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
          axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
          strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
          plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
          panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.lems


## Save plot
ggsave(
  "longitudinal.trajectory.lems.pdf",
  plot = longitudinal.trajectory.lems,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#-----Upper Extremity Motor Score----
longitudinal.trajectory.uems <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline,aes(x=ExamStage_weeks, y=as.numeric(as.character(UEMS)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline$plegia~emsci.trauma.sex.ais.baseline$baseline.ais, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Upper Extremity Motor Score')+xlab("Time since injury [weeks]")+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.uems


## Save plot
ggsave(
  "longitudinal.trajectory.uems.pdf",
  plot = longitudinal.trajectory.uems,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#-----SCIM2 and 3 Total score----
longitudinal.trajectory.scim <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline,aes(x=ExamStage_weeks, y=as.numeric(as.character(SCIM23_TotalScore)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline$plegia~emsci.trauma.sex.ais.baseline$baseline.ais, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Total SCIM Score')+xlab("Time since injury [weeks]")+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.scim


## Save plot
ggsave(
  "longitudinal.trajectory.scim.pdf",
  plot = longitudinal.trajectory.scim,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#-----WISCI----
longitudinal.trajectory.wisci <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline,aes(x=ExamStage_weeks, y=as.numeric(as.character(WISCI)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline$plegia~emsci.trauma.sex.ais.baseline$baseline.ais, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Walking Index for Spinal Cord Injury')+xlab("Time since injury [weeks]")+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.wisci


## Save plot
ggsave(
  "longitudinal.trajectory.wisci.pdf",
  plot = longitudinal.trajectory.wisci,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()



#----- 6min Walking Test----

# Subset data to AIS C and D patients
emsci.trauma.sex.ais.baseline.walking <-subset(emsci.trauma.sex.ais.baseline, baseline.ais=='AIS-C' | baseline.ais=="AIS-D")

#Create plot
longitudinal.trajectory.x6min <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline.walking,aes(x=ExamStage_weeks, y=as.numeric(as.character(X6min)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline.walking$plegia~emsci.trauma.sex.ais.baseline.walking$baseline.ais, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('6min Walking Test [m]')+xlab("Time since injury [weeks]")+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.x6min


## Save plot
ggsave(
  "longitudinal.trajectory.x6min.pdf",
  plot = longitudinal.trajectory.x6min,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#----- 10m Walking Test----

# Subset data to AIS C and D patients
emsci.trauma.sex.ais.baseline.walking <-subset(emsci.trauma.sex.ais.baseline, baseline.ais=='AIS-C' | baseline.ais=="AIS-D")

#Create plot
longitudinal.trajectory.X10m <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline.walking,aes(x=ExamStage_weeks, y=as.numeric(as.character(X10m)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline.walking$plegia~emsci.trauma.sex.ais.baseline.walking$baseline.ais, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('10m Walking Test [s]')+xlab("Time since injury [weeks]")+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.X10m


## Save plot
ggsave(
  "longitudinal.trajectory.X10m.pdf",
  plot = longitudinal.trajectory.X10m,
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

