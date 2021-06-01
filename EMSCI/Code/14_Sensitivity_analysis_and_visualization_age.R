## ---------------------------
##
## Script name: 14_Sensitivity_analysis_age
##
## Purpose of script: To assess if there is a bias related to age.
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2021-01-22
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
library(table1)
##   
## ----------------------------
##   
## Install packages needed:  (uncomment as required)
##   
# if(!require(ggplot2)){install.packages("ggplot2")}
# if(!require(grid)){install.packages("grid")}
# if(!require(grid)){install.packages("ggthemes")}
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
emsci.trauma.sex.ais.baseline <-merge(emsci.trauma.sex, emsci.trauma.sex.va.a1[,c(2,245)])

# Change names of levels of AIS grade and plegia
label(emsci.trauma.sex.ais.baseline$baseline.ais) <- c("AIS-A", "AIS-B", "AIS-C", "AIS-D", " ", "")
levels(emsci.trauma.sex.ais.baseline$plegia) <- c("Paraplegia", "Tetraplegia ")

# Create age groups
labs <- c(paste(seq(0, 89, by = 30), seq(0 + 30 - 1, 90 - 1, by = 30),
              sep = "-"))
labs

# Add new varianle AgeGroup to the main dataframe
emsci.trauma.sex.ais.baseline$AgeGroup <- cut(emsci.trauma.sex.ais.baseline$AgeAtDOI, breaks = c(seq(0, 89, by = 30), Inf), labels = labs, right = FALSE)

# Create subset according to AIS grades
emsci.trauma.sex.ais.baseline.ais.a= subset(emsci.trauma.sex.ais.baseline, (baseline.ais=='A') )
emsci.trauma.sex.ais.baseline.ais.b= subset(emsci.trauma.sex.ais.baseline, (baseline.ais=='B') )
emsci.trauma.sex.ais.baseline.ais.c= subset(emsci.trauma.sex.ais.baseline, (baseline.ais=='C') )
emsci.trauma.sex.ais.baseline.ais.d= subset(emsci.trauma.sex.ais.baseline, (baseline.ais=='D') )

#---------- Total Motor Score - AIS-A Patients --------#

# Generate plot
longitudinal.trajectory.tms.age.ais.a <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline.ais.a,aes(x=ExamStage_weeks, y=as.numeric(as.character(TMS)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline.ais.a$plegia~emsci.trauma.sex.ais.baseline.ais.a$AgeGroup, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Total Motor Score')+xlab("Time since injury [weeks]")+ggtitle('AIS-A')+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        plot.title = element_text(size=12, hjust=0.5, face="bold", family='Times'),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.tms.age.ais.a

# Save plot
ggsave(
  "longitudinal.trajectory.tms.age.ais.a.pdf",
  plot = longitudinal.trajectory.tms.age.ais.a,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Total Motor Score - AIS-B Patients --------#

# Generate plot
longitudinal.trajectory.tms.age.ais.b <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline.ais.b,aes(x=ExamStage_weeks, y=as.numeric(as.character(TMS)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline.ais.b$plegia~emsci.trauma.sex.ais.baseline.ais.b$AgeGroup, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Total Motor Score')+xlab("Time since injury [weeks]")+ggtitle('AIS-B')+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        plot.title = element_text(size=12, hjust=0.5, face="bold", family='Times'),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.tms.age.ais.b

# Save plot
ggsave(
  "longitudinal.trajectory.tms.age.ais.b.pdf",
  plot = longitudinal.trajectory.tms.age.ais.b,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Total Motor Score - AIS-C Patients --------#

# Generate plot
longitudinal.trajectory.tms.age.ais.c <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline.ais.c,aes(x=ExamStage_weeks, y=as.numeric(as.character(TMS)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline.ais.c$plegia~emsci.trauma.sex.ais.baseline.ais.c$AgeGroup, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Total Motor Score')+xlab("Time since injury [weeks]")+ggtitle('AIS-C')+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        plot.title = element_text(size=12, hjust=0.5, face="bold", family='Times'),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.tms.age.ais.c

# Save plot
ggsave(
  "longitudinal.trajectory.tms.age.ais.c.pdf",
  plot = longitudinal.trajectory.tms.age.ais.c,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Total Motor Score - AIS-D Patients --------#

# Generate plot
longitudinal.trajectory.tms.age.ais.d <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline.ais.d,aes(x=ExamStage_weeks, y=as.numeric(as.character(TMS)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline.ais.d$plegia~emsci.trauma.sex.ais.baseline.ais.d$AgeGroup, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Total Motor Score')+xlab("Time since injury [weeks]")+ggtitle('AIS-D')+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        plot.title = element_text(size=12, hjust=0.5, face="bold", family='Times'),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.tms.age.ais.d

# Save plot
ggsave(
  "longitudinal.trajectory.tms.age.ais.d.pdf",
  plot = longitudinal.trajectory.tms.age.ais.d,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Total Sensory Score - AIS-A Patients --------#

# Generate plot
longitudinal.trajectory.tss.age.ais.a <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline.ais.a,aes(x=ExamStage_weeks, y=as.numeric(as.character(TSS)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline.ais.a$plegia~emsci.trauma.sex.ais.baseline.ais.a$AgeGroup, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Total Sensory Score')+xlab("Time since injury [weeks]")+ggtitle('AIS-A')+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        plot.title = element_text(size=12, hjust=0.5, face="bold", family='Times'),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.tss.age.ais.a

# Save plot
ggsave(
  "longitudinal.trajectory.tss.age.ais.a.pdf",
  plot = longitudinal.trajectory.tss.age.ais.a,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Total Sensory Score - AIS-B Patients --------#

# Generate plot
longitudinal.trajectory.tss.age.ais.b <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline.ais.b,aes(x=ExamStage_weeks, y=as.numeric(as.character(TSS)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline.ais.b$plegia~emsci.trauma.sex.ais.baseline.ais.b$AgeGroup, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Total Sensory Score')+xlab("Time since injury [weeks]")+ggtitle('AIS-B')+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        plot.title = element_text(size=12, hjust=0.5, face="bold", family='Times'),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.tss.age.ais.b

# Save plot
ggsave(
  "longitudinal.trajectory.tss.age.ais.b.pdf",
  plot = longitudinal.trajectory.tss.age.ais.b,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Total Sensory Score - AIS-C Patients --------#

# Generate plot
longitudinal.trajectory.tss.age.ais.c <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline.ais.c,aes(x=ExamStage_weeks, y=as.numeric(as.character(TSS)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline.ais.c$plegia~emsci.trauma.sex.ais.baseline.ais.c$AgeGroup, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Total Sensory Score')+xlab("Time since injury [weeks]")+ggtitle('AIS-C')+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        plot.title = element_text(size=12, hjust=0.5, face="bold", family='Times'),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.tss.age.ais.c

# Save plot
ggsave(
  "longitudinal.trajectory.tss.age.ais.c.pdf",
  plot = longitudinal.trajectory.tss.age.ais.c,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Total Sensory Score - AIS-D Patients --------#

# Generate plot
longitudinal.trajectory.tss.age.ais.d <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline.ais.d,aes(x=ExamStage_weeks, y=as.numeric(as.character(TSS)), color=X5_year_bins, fill=X5_year_bins), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline.ais.d$plegia~emsci.trauma.sex.ais.baseline.ais.d$AgeGroup, scales = 'free')+scale_fill_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c('#218317',"#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Total Sensory Score')+xlab("Time since injury [weeks]")+ggtitle('AIS-D')+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        plot.title = element_text(size=12, hjust=0.5, face="bold", family='Times'),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.tss.age.ais.d

# Save plot
ggsave(
  "longitudinal.trajectory.tss.age.ais.d.pdf",
  plot = longitudinal.trajectory.tss.age.ais.d,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#---------- 2. ANALYSIS --------


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
emsci.trauma.sex.va.a1$baseline.tms <-emsci.trauma.sex.va.a1$TMS
emsci.trauma.sex.va.a1$baseline.scim <-emsci.trauma.sex.va.a1$SCIM23_TotalScore

# Merge
emsci.trauma.sex.baseline.ais <-merge(emsci.trauma.sex, emsci.trauma.sex.va.a1[,c(2,245,246,247)])


# Create age groups
labs <- c(paste(seq(0, 89, by = 30), seq(0 + 30 - 1, 90 - 1, by = 30),
                sep = "-"))
labs

# Add new varianle AgeGroup to the main dataframe
emsci.trauma.sex.baseline.ais$AgeGroup <- cut(emsci.trauma.sex.baseline.ais$AgeAtDOI, breaks = c(seq(0, 89, by = 30), Inf), labels = labs, right = FALSE)

#Convert certain columns to numeric
emsci.trauma.sex.baseline.ais[,c(5,6,8,11,24,25,30,31,36,187, 189,191, 193)] <- sapply(emsci.trauma.sex.baseline.ais[,c(5,6,8,11,24,25,30,31,36,187, 189,191, 193)], as.numeric)

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

emsci.rescaled <-rescale.many(emsci.trauma.sex.baseline.ais, c(6,8,11)) 

# Prepare selection variables
ais.score<-unique(emsci.rescaled$baseline.ais)
rescaled.nli <- unique(emsci.rescaled$plegia)
emsci.rescaled$Patientennummer <- as.factor(emsci.rescaled$Patientennummer)
AgeGroup<-unique(as.factor(emsci.trauma.sex.baseline.ais$AgeGroup))

# Create data frame to store results
results <- data.frame()
for (j in rescaled.nli){
  for (i in ais.score){
    for (h in AgeGroup){
    print(paste("MODEL",j, i, h,sep = " "))
    df1 = subset(emsci.rescaled, (baseline.ais == i & plegia == j & AgeGroup==h))
    
    if (nrow(df1) == 0) next
    mixed.lmer<- nlme::lme(WISCI ~ ExamStage_weeks.rescaled*YEARDOI.rescaled, df1, random = ~ 1|Patientennummer, na.action = na.exclude)
    print(summary(mixed.lmer))
    
    # mixed.lmer <- nlme::nlme(LEMS ~ SSasympOff(ExamStage_weeks.rescaled*YEARDOI.rescaled+AgeAtDOI.rescaled, Asym, R0, lrc),
    #            data = df1,
    #            fixed = Asym + R0 + lrc ~ 1,
    #            random = Asym ~ 1,
    #            na.action=na.exclude, 
    #            na.fail(emsci.rescaled),
    #            naPattern = ~ !is.na(LEMS),
    #            method="ML",verbose=TRUE)
    
    
    n <- count(ranef(mixed.lmer))
    
    # Capture summary stats
    intercept.estimate <- coef(summary(mixed.lmer))[1]
    time_rescaled.estimate <- coef(summary(mixed.lmer))[2]
    yeardoi.estimate <- coef(summary(mixed.lmer))[3]
    yeardoi_weeks.estimate <- coef(summary(mixed.lmer))[4]
    intercept.std <- coef(summary(mixed.lmer))[5]
    time_rescaled.std <- coef(summary(mixed.lmer))[6]
    yeardoi.std <- coef(summary(mixed.lmer))[7]
    yeardoi_weeks.std <- coef(summary(mixed.lmer))[8]
    intercept.df <- coef(summary(mixed.lmer))[9]
    time_rescaled.df <- coef(summary(mixed.lmer))[10]
    yeardoi.df <- coef(summary(mixed.lmer))[11]
    yeardoi_weeks.df <- coef(summary(mixed.lmer))[12]
    intercept.tval <- coef(summary(mixed.lmer))[13]
    time_rescaled.tval <- coef(summary(mixed.lmer))[14]
    yeardoi.tval <- coef(summary(mixed.lmer))[15]
    yeardoi_weeks.tval <- coef(summary(mixed.lmer))[16]
    intercept.pval <- coef(summary(mixed.lmer))[17]
    time_rescaled.pval <- coef(summary(mixed.lmer))[18]
    yeardoi.pval <- coef(summary(mixed.lmer))[19]
    yeardoi_weeks.pval <- coef(summary(mixed.lmer))[20]
    
    # Get coefficents of mixed.lmer
    cfit <- coef(summary(mixed.lmer))
    
    # Create temporary data frame
    df <- data.frame( plegia= j,AIS = i,AgeGroup=h, intercept.estimate = cfit[1], time_rescaled.estimate = cfit[2],
                      yeardoi.estimate= cfit[3], yeardoi_weeks.estimate= cfit[4],intercept.std= cfit[5],
                      time_rescaled.std =cfit[6],
                      yeardoi.std =cfit[7],
                      yeardoi_weeks.std =cfit[8],
                      intercept.df =cfit[9],
                      time_rescaled.df =cfit[10],
                      yeardoi.df =cfit[11],
                      yeardoi_weeks.df =cfit[12],
                      intercept.tval =cfit[13],
                      time_rescaled.tval =cfit[14],
                      yeardoi.tval =cfit[15],
                      yeardoi_weeks.tval =cfit[16],
                      intercept.pval =cfit[17],
                      time_rescaled.pval =cfit[18],
                      yeardoi.pval =cfit[19],
                      yeardoi_weeks.pval =cfit[20],
                      stringsAsFactors = F)
    
    
    df2<- cbind(df, n)
    
    # Bind rows of temporary data frame to the results data frame
    results <- rbind(results, df2)
    #results<- cbind(results, n)
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
new_data.2$order[(new_data.2$Variable == 'ExamStage_weeks.rescaled')] <- 2
new_data.2$order[(new_data.2$Variable == 'yeardoi')] <- 3
new_data.2$order[(new_data.2$Variable == 'yeardoi_weeks')] <- 4

# Create a new variable based on condition
new_data.2$Variable[(new_data.2$Variable == 'intercept.')] <- "Intercept"
new_data.2$Variable[(new_data.2$Variable == 'age.')] <- "Age"
new_data.2$Variable[(new_data.2$Variable == 'yeardoi')] <- "Time since injury"
new_data.2$Variable[(new_data.2$Variable == 'yeardoi_weeks')] <- "Year*Time since injury"

# Create a new variable based on condition
new_data.2$model_temp[(new_data.2$plegia == "tetra" & new_data.2$AIS == "A"& new_data.2$AgeGroup=="0-29")] <- 'Tetraplegia:AIS A, 0-29 years'
new_data.2$model_temp[(new_data.2$plegia == "tetra" & new_data.2$AIS == "B")] <- 'Tetraplegia:AIS B'
new_data.2$model_temp[(new_data.2$plegia == "tetra" & new_data.2$AIS == "C" )] <- 'Tetraplegia:AIS C'
new_data.2$model_temp[(new_data.2$plegia == "tetra" & new_data.2$AIS == "D" )] <- 'Tetraplegia:AIS D'

new_data.2$model_temp[(new_data.2$plegia == "para" & new_data.2$AIS == "A" )] <- 'Paraplegia:AIS A'
new_data.2$model_temp[(new_data.2$plegia == "para" & new_data.2$AIS == "B" )] <- 'Paraplegia:AIS B'
new_data.2$model_temp[(new_data.2$plegia == "para" & new_data.2$AIS == "C" )] <- 'Paraplegia:AIS C'
new_data.2$model_temp[(new_data.2$plegia == "para" & new_data.2$AIS == "D" )] <- 'Paraplegia:AIS D'

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
new_data_4$Model[(new_data_4$plegia == "tetra" & new_data_4$AIS == "A" & new_data_4$order==1)] <- 'Tetraplegia: AIS A'
new_data_4$Model[(new_data_4$plegia == "tetra" & new_data_4$AIS == "B"& new_data_4$order==1)] <- 'Tetraplegia: AIS B'
new_data_4$Model[(new_data_4$plegia == "tetra" & new_data_4$AIS == "C" & new_data_4$order==1)] <- 'Tetraplegia: AIS C'
new_data_4$Model[(new_data_4$plegia == "tetra" & new_data_4$AIS == "D" & new_data_4$order==1)] <- 'Tetraplegia: AIS D'

new_data_4$Model[(new_data_4$plegia == "para" & new_data_4$AIS == "A" & new_data_4$order==1)] <- 'Paraplegia: AIS A'
new_data_4$Model[(new_data_4$plegia == "para" & new_data_4$AIS == "B" & new_data_4$order==1)] <- 'Paraplegia: AIS B'
new_data_4$Model[(new_data_4$plegia == "para" & new_data_4$AIS == "C" & new_data_4$order==1)] <- 'Paraplegia: AIS C'
new_data_4$Model[(new_data_4$plegia == "para" & new_data_4$AIS == "D" & new_data_4$order==1)] <- 'Paraplegia: AIS D'

# Replace NA with empty cell
new_data_4[is.na(new_data_4)] <- ""
new_data_4[new_data_4 == "<NA>"] <- ""

# Write csv file with only selected columns
write.csv(new_data_4[,c(13,3:9,12)],"/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables/Sensitivity.Analysis_Age/emsci.mixed.models.results_WISCI_age.csv", row.names = F)

#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####









#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

