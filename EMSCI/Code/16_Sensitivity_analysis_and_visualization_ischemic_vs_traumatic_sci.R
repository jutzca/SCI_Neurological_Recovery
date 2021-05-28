## ---------------------------
##
## Script name: 16_Sensitivity_analysis_ischemic_vs_traumatic_sci
##
## Purpose of script: To assess if there is a bias related to cause of injury, specifically traumatic vs. ischemic injuries
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
emsci.trauma.sex.va.a1$baseline.tms <-emsci.trauma.sex.va.a1$TMS
emsci.trauma.sex.va.a1$baseline.scim <-emsci.trauma.sex.va.a1$SCIM23_TotalScore

# Merge
emsci.trauma.sex.ais.baseline <-merge(emsci.trauma.sex, emsci.trauma.sex.va.a1[,c(2,244,245,246)])

# Change levels of AIS grade and plegia
levels(emsci.trauma.sex.ais.baseline$baseline.ais) <- c("AIS-A", "AIS-B", "AIS-C", "AIS-D", " ", "")
levels(emsci.trauma.sex.ais.baseline$plegia) <- c("Paraplegia", "Tetraplegia")

#---------- 1. VISUALIZATION --------

#---------- Total Motor Score --------#

# Generate plot 
longitudinal.trajectory.tms.ischemic_traumatic <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline,aes(x=ExamStage_weeks, y=as.numeric(as.character(TMS)), color=Cause_new, fill=Cause_new), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline$plegia~emsci.trauma.sex.ais.baseline$baseline.ais, scales = 'free')+scale_fill_manual(values = c("#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c("#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Total Motor Score')+xlab("Time since injury [weeks]")+
  scale_y_continuous( limits = c(0, 100), breaks = seq(0, 100, 25), expand = c(0,0))+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#FFFFFF', color="#FFFFFF"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.tms.ischemic_traumatic

# Save plot
ggsave(
  "longitudinal.trajectory.tms.ischemic_traumatic.pdf",
  plot = longitudinal.trajectory.tms.ischemic_traumatic,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Total SCIM Score --------#

# Generate plot 
longitudinal.trajectory.scim.ischemic_traumatic <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline,aes(x=ExamStage_weeks, y=as.numeric(as.character(SCIM23_TotalScore)), color=Cause_new, fill=Cause_new), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline$plegia~emsci.trauma.sex.ais.baseline$baseline.ais, scales = 'free')+scale_fill_manual(values = c("#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c("#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Total SCIM Score')+xlab("Time since injury [weeks]")+
  scale_y_continuous( limits = c(0, 100), breaks = seq(0, 100, 25), expand = c(0,0))+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#FFFFFF', color="#FFFFFF"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.scim.ischemic_traumatic

# Save plot
ggsave(
  "longitudinal.trajectory.scim.ischemic_traumatic.pdf",
  plot = longitudinal.trajectory.scim.ischemic_traumatic,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Total Walking Index Score --------#

# Generate plot 
longitudinal.trajectory.wisci.ischemic_traumatic <-ggplot() +
  stat_summary(data=emsci.trauma.sex.ais.baseline,aes(x=ExamStage_weeks, y=as.numeric(as.character(WISCI)), color=Cause_new, fill=Cause_new), fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, linetype=1, alpha=0.2) +
  facet_grid(emsci.trauma.sex.ais.baseline$plegia~emsci.trauma.sex.ais.baseline$baseline.ais, scales = 'free')+scale_fill_manual(values = c("#457fe1", "#b30099", "#ffba00" ))+scale_color_manual(values = c("#457fe1", "#b30099", "#ffba00" ))+
  theme_bw()+ ylab('Total SCIM Score')+xlab("Time since injury [weeks]")+
  #scale_y_continuous( limits = c(0, 100), breaks = seq(0, 100, 25), expand = c(0,0))+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#FFFFFF', color="#FFFFFF"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
longitudinal.trajectory.wisci.ischemic_traumatic

# Save plot
ggsave(
  "longitudinal.trajectory.wisci.ischemic_traumatic.pdf",
  plot = longitudinal.trajectory.wisci.ischemic_traumatic,
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


rescale.many <- function(dat, column.nos) { 
  nms <- names(dat) 
  for(col in column.nos) { 
    name <- paste(nms[col],".rescaled", sep = "") 
    dat[name] <- rescale(dat[,col], center = TRUE, scale = TRUE) 
  } 
  cat(paste("Rescaled ", length(column.nos),      " variable(s)n")) 
  dat 
} 

emsci.rescaled <-rescale.many(emsci.trauma.sex.ais.baseline, c(6,8,11)) 

#---------- Total Motor Score --------

# Paraplegic patients
mixed.lmer.ischemic.ais.a.para <- lmer(TMS~ ExamStage_weeks.rescaled+baseline.tms+Cause_new+ (1|Patientennummer), data = subset(emsci.rescaled, (baseline.ais=="AIS-A" & plegia =='Paraplegia')))
print(summary(mixed.lmer.ischemic.ais.a.para))
tab_model(mixed.lmer.ischemic.ais.a.para, show.se = TRUE, show.ci = NULL, digits = 3)

mixed.lmer.ischemic.ais.b.para <- lmer(TMS~ ExamStage_weeks.rescaled+baseline.tms+Cause_new+ (1|Patientennummer), data = subset(emsci.rescaled, (baseline.ais=="AIS-B" & plegia =='Paraplegia')))
print(summary(mixed.lmer.ischemic.ais.b.para))
tab_model(mixed.lmer.ischemic.ais.b.para, show.se = TRUE, show.ci = NULL, digits = 3)

mixed.lmer.ischemic.ais.c.para <- lmer(TMS~ ExamStage_weeks.rescaled+baseline.tms+Cause_new+ (1|Patientennummer), data = subset(emsci.rescaled, (baseline.ais=="AIS-C" & plegia =='Paraplegia')))
print(summary(mixed.lmer.ischemic.ais.c.para))
tab_model(mixed.lmer.ischemic.ais.c.para, show.se = TRUE, show.ci = NULL, digits = 3)

mixed.lmer.ischemic.ais.d.para <- lmer(TMS~ ExamStage_weeks.rescaled+baseline.tms+Cause_new+ (1|Patientennummer), data = subset(emsci.rescaled, (baseline.ais=="AIS-D" & plegia =='Paraplegia')))
print(summary(mixed.lmer.ischemic.ais.d.para))
tab_model(mixed.lmer.ischemic.ais.d.para, show.se = TRUE, show.ci = NULL, digits = 3)

# Tetraplegic patients
mixed.lmer.ischemic.ais.a.tetra <- lmer(TMS~ ExamStage_weeks.rescaled+baseline.tms+Cause_new+ (1|Patientennummer), data = subset(emsci.rescaled, (baseline.ais=="AIS-A" & plegia =='Tetraplegia')))
print(summary(mixed.lmer.ischemic.ais.a.tetra))
tab_model(mixed.lmer.ischemic.ais.a.tetra, show.se = TRUE, show.ci = NULL, digits = 3)

mixed.lmer.ischemic.ais.b.tetra <- lmer(TMS~ ExamStage_weeks.rescaled+baseline.tms+Cause_new+ (1|Patientennummer), data = subset(emsci.rescaled, (baseline.ais=="AIS-B" & plegia =='Tetraplegia')))
print(summary(mixed.lmer.ischemic.ais.b.tetra))
tab_model(mixed.lmer.ischemic.ais.b.tetra, show.se = TRUE, show.ci = NULL, digits = 3)

mixed.lmer.ischemic.ais.c.tetra <- lmer(TMS~ ExamStage_weeks.rescaled+baseline.tms+Cause_new+ (1|Patientennummer), data = subset(emsci.rescaled, (baseline.ais=="AIS-C" & plegia =='Tetraplegia')))
print(summary(mixed.lmer.ischemic.ais.c.tetra))
tab_model(mixed.lmer.ischemic.ais.c.tetra, show.se = TRUE, show.ci = NULL, digits = 3)

mixed.lmer.ischemic.ais.d.tetra <- lmer(TMS~ ExamStage_weeks.rescaled+baseline.tms+Cause_new+ (1|Patientennummer), data = subset(emsci.rescaled, (baseline.ais=="AIS-D" & plegia =='Tetraplegia')))
print(summary(mixed.lmer.ischemic.ais.d.tetra))
tab_model(mixed.lmer.ischemic.ais.d.tetra, show.se = TRUE, show.ci = NULL, digits = 3)




#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

