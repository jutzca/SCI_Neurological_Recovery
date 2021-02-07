## ---------------------------
##
## Script name: 6_Heat_plots_outcomes_sygen
##
## Purpose of script: To visualize the longitudinal neurological scores
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
library(data.table)
library(magrittr)
library(gridExtra)
library(grid)
library(forcats)
library(viridis)
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
sygen<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/df_sygen_formatted.csv", sep = ',', header = T,  na.strings=c("","NA"))

# Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
sygen.included.cohort.all.times<- subset(sygen, (!is.na(Age)) & (Sex=="Female" | Sex=="Male") & ###Age at DOI and Sex
                                           (NLI == 'cervical' | NLI == 'thoracic')&   ## Neurological level
                                           (AIS=="AIS A"| AIS=="AIS B"| AIS=="AIS C"| AIS=="AIS D")) #AIS Grades

sygen.included.cohort.all.times.2 <- subset(sygen.included.cohort.all.times, (Plegia=='para' | Plegia=="tetra"))

#---------- LEMS Sygen --------#
sygen.included.cohort.all.times.2$LEMS <- as.numeric(sygen.included.cohort.all.times.2$LEMS)

# Create data frame with mean and sd for lems
new.data.lems.sygen =sygen.included.cohort.all.times.2 %>%
  group_by(AIS, Time, Plegia) %>%
  dplyr::summarize( 
    n = n(),
    mean_LEMS = mean(LEMS, na.rm=TRUE),
    sd_LEMS = sd(LEMS, na.rm=TRUE))

# Write data file
write.csv(new.data.lems.sygen, '/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen/Tables/lems.sygen.csv')

# To reverse the order of levels of AIS
new.data.lems.sygen$AIS <- factor(new.data.lems.sygen$AIS, levels=rev(levels(new.data.lems.sygen$AIS)))
levels(new.data.lems.sygen$Plegia)<-c("Paraplegia\n", 'Tetraplegia\n') 

# Round values to 1 digits
new.data.lems.sygen$mean_LEMS <- round(new.data.lems.sygen$mean_LEMS,1 )
new.data.lems.sygen$mean_LEMS <- round(new.data.lems.sygen$mean_LEMS, 1)

# Create plot
lems.sygen.plot <- ggplot(new.data.lems.sygen,aes(x = as.factor(Time),y = AIS,fill = mean_LEMS)) + 
  geom_tile()+scale_fill_viridis(option = "inferno",  direction = -1, limits = c(-8, 50))+
  facet_grid(.~new.data.lems.sygen$Plegia)+theme_economist()+
  geom_text(aes(label=as.numeric(new.data.lems.sygen$mean_LEMS)), size=3, color='white')+
  labs(title = "Lower Extremity Motor Score (LEMS)", x = "Weeks Post Injury", y = "AIS Score", fill='LEMS') +
  theme(plot.title = element_text(hjust = 0.5, size= 14),
        axis.title = element_text(size=12),
        axis.text = element_text(size=10),
        strip.text = element_text(size=12), 
        legend.text = element_text(size=10),
        legend.position = 'right',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
lems.sygen.plot 

# Save plot
ggsave(
  "lems.sygen.plot.pdf",
  plot = lems.sygen.plot,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 6,
  height = 3,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Create longitudinal lems trajectory figure --------#

# Create plot
lems.trajectory.sygen.plot <- ggplot() +
  stat_summary(aes(x = as.numeric(Time),y = as.numeric(LEMS), group=AIS), data=sygen.included.cohort.all.times.2, fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="red", size=0.5)+
  facet_grid(sygen.included.cohort.all.times.2$Plegia~sygen.included.cohort.all.times.2$AIS)+
  ylab('Lower Extremity Motor Score')+xlab("Weeks Post Injury")+
  scale_x_continuous( limits = c(0, 52), breaks = seq(0, 52, 10), expand = c(0,0))+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text.x = element_text(face='bold'))
lems.trajectory.sygen.plot

# Save plot
ggsave(
  "lems.trajectory.sygen.plot.pdf",
  plot = lems.trajectory.sygen.plot,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#---------- UEMS Sygen --------#
sygen.included.cohort.all.times.2$UEMS <- as.numeric(sygen.included.cohort.all.times.2$UEMS)

# Create data frame with mean and sd for UEMS
new.data.uems.sygen =sygen.included.cohort.all.times.2 %>%
  group_by(AIS, Time, Plegia) %>%
  dplyr::summarize( 
    n = n(),
    mean_uems = mean(UEMS, na.rm=TRUE),
    sd_uems = sd(UEMS, na.rm=TRUE))

# Write data file
write.csv(new.data.uems.sygen, '/Users/jutzelec/Documents/Github/SCI_Neurological_Recovery/Sygen/Tables/uems.sygen.csv')

# To reverse the order of levels of AIS
new.data.uems.sygen$AIS <- factor(new.data.uems.sygen$AIS, levels=rev(levels(new.data.uems.sygen$AIS)))
levels(new.data.uems.sygen$Plegia)<-c("Paraplegia\n", 'Tetraplegia\n') 

# Round values to 1 digits
new.data.uems.sygen$mean_uems <- round(new.data.uems.sygen$mean_uems,1 )
new.data.uems.sygen$mean_uems <- round(new.data.uems.sygen$mean_uems, 1)

# Create plot
uems.sygen.plot <- ggplot(new.data.uems.sygen,aes(x = as.factor(Time),y = AIS,fill = mean_uems)) + 
  geom_tile()+scale_fill_viridis(option = "inferno",  direction = -1, limits = c(-8, 50))+
  facet_grid(.~new.data.uems.sygen$Plegia)+theme_economist()+
  geom_text(aes(label=as.numeric(mean_uems)), size=3, color='white')+
  labs(title = "Upper Extremity Motor Score (UEMS)", x = "Weeks Post Injury", y = "AIS Score", fill='UEMS') +
  theme(plot.title = element_text(hjust = 0.5, size= 14),
        axis.title = element_text(size=12),
        axis.text = element_text(size=10),
        strip.text = element_text(size=12), 
        legend.text = element_text(size=10),
        legend.position = 'right',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
uems.sygen.plot 

# Save plot
ggsave(
  "uems.sygen.plot.pdf",
  plot = uems.sygen.plot,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 6,
  height = 3,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Create longitudinal uems trajectory figure --------#

uems.trajectory.sygen.plot <- ggplot(sygen.included.cohort.all.times.2,aes(x = as.numeric(Time),y = as.numeric(UEMS), group=AIS)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="red", size=0.5)+
  facet_grid(sygen.included.cohort.all.times.2$Plegia~sygen.included.cohort.all.times.2$AIS)+
  ylab('Upper Extremity Motor Score')+xlab("Weeks Post Injury")+
  scale_x_continuous( limits = c(0, 52), breaks = seq(0, 52, 10), expand = c(0,0))+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text.x = element_text(face='bold'))
uems.trajectory.sygen.plot

# Save plot
ggsave(
  "uems.trajectory.sygen.plot.pdf",
  plot = uems.trajectory.sygen.plot,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#---------- TSS Sygen --------#
sygen.included.cohort.all.times.2$tss <- as.numeric(sygen.included.cohort.all.times.2$TSS)

# Create data frame with mean and sd for tss
new.data.tss.sygen =sygen.included.cohort.all.times.2 %>%
  group_by(AIS, Time, Plegia) %>%
  dplyr::summarize( 
    n = n(),
    mean_tss = mean(TSS, na.rm=TRUE),
    sd_tss = sd(TSS, na.rm=TRUE))

# Write data file
write.csv(new.data.tss.sygen, '/Users/jutzelec/Documents/Github/SCI_Neurological_Recovery/Sygen/Tables/tss.sygen.csv')

# To reverse the order of levels of AIS
new.data.tss.sygen$AIS <- factor(new.data.tss.sygen$AIS, levels=rev(levels(new.data.tss.sygen$AIS)))
levels(new.data.tss.sygen$Plegia)<-c("Paraplegia\n", 'Tetraplegia\n') 

# Round values to 1 digits
new.data.tss.sygen$mean_tss <- round(new.data.tss.sygen$mean_tss,1 )
new.data.tss.sygen$mean_tss <- round(new.data.tss.sygen$mean_tss, 1)

# Create plot
tss.sygen.plot <- ggplot(new.data.tss.sygen,aes(x = as.factor(Time),y = AIS,fill = mean_tss)) + 
  geom_tile()+scale_fill_viridis(option = "viridis",  direction = -1, limits = c(0, 180))+
  facet_grid(.~new.data.tss.sygen$Plegia)+theme_economist()+
  geom_text(aes(label=as.numeric(mean_tss)), size=3, color='white')+
  labs(title = "Total Sensory Score (TSS)", x = "Weeks Post Injury", y = "AIS Score", fill='TSS') +
  theme(plot.title = element_text(hjust = 0.5, size= 14),
        axis.title = element_text(size=12),
        axis.text = element_text(size=10),
        strip.text = element_text(size=12), 
        legend.text = element_text(size=10),
        legend.position = 'right',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
tss.sygen.plot 

# Save plot
ggsave(
  "tss.sygen.plot.pdf",
  plot = tss.sygen.plot,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 7.5,
  height = 2.5,
  units = "in",
  dpi = 300
)

dev.off()



#---------- Create longitudinal tss trajectory figure --------#

# Create plot
tss.trajectory.sygen.plot <- ggplot(sygen.included.cohort.all.times.2,aes(x = as.numeric(Time),y = as.numeric(TSS), group=AIS)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="red", size=0.5)+
  facet_grid(sygen.included.cohort.all.times.2$Plegia~sygen.included.cohort.all.times.2$AIS)+
  ylab('Total Sensory Score')+xlab("Weeks Post Injury")+
  scale_x_continuous( limits = c(0, 52), breaks = seq(0, 52, 10), expand = c(0,0))+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text.x = element_text(face='bold'))
tss.trajectory.sygen.plot

# Save plot
ggsave(
  "tss.trajectory.sygen.plot.pdf",
  plot = tss.trajectory.sygen.plot,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- TMS Sygen --------#

# Create plot
tms.trajectory.sygen.plot <- ggplot(sygen.included.cohort.all.times.2,aes(x = as.numeric(Time),y = as.numeric(TMS), group=AIS)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="red", size=0.5)+
  facet_grid(sygen.included.cohort.all.times.2$Plegia~sygen.included.cohort.all.times.2$AIS)+
  ylab('Total Motor Score')+xlab("Weeks Post Injury")+
  scale_x_continuous( limits = c(0, 52), breaks = seq(0, 52, 10), expand = c(0,0))+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text.x = element_text(face='bold'))
tms.trajectory.sygen.plot

# Save plot
ggsave(
  "tms.trajectory.sygen.plot.pdf",
  plot = tms.trajectory.sygen.plot,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Pinprick Sygen --------#

# Create plot
pp.trajectory.sygen.plot <- ggplot(sygen.included.cohort.all.times.2,aes(x = as.numeric(Time),y = as.numeric(TPP), group=AIS)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="red", size=0.5)+
  facet_grid(sygen.included.cohort.all.times.2$Plegia~sygen.included.cohort.all.times.2$AIS)+
  ylab('Total Pinprick Score')+xlab("Weeks Post Injury")+
  scale_x_continuous( limits = c(0, 52), breaks = seq(0, 52, 10), expand = c(0,0))+
  scale_y_continuous( limits = c(0, 120), breaks = seq(0, 120, 20), expand = c(0,0))+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text.x = element_text(face='bold'))
pp.trajectory.sygen.plot

#Save plot
ggsave(
  "pp.trajectory.sygen.plot.pdf",
  plot = pp.trajectory.sygen.plot,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Light Touch Sygen --------#

# Create plot
lt.trajectory.sygen.plot <- ggplot(sygen.included.cohort.all.times.2,aes(x = as.numeric(Time),y = as.numeric(TLT), group=AIS)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="red", size=0.5)+
  facet_grid(sygen.included.cohort.all.times.2$Plegia~sygen.included.cohort.all.times.2$AIS)+
  ylab('Total Light Touch Score')+xlab("Weeks Post Injury")+
  scale_x_continuous( limits = c(0, 52), breaks = seq(0, 52, 10), expand = c(0,0))+
  scale_y_continuous( limits = c(0, 120), breaks = seq(0, 120, 20), expand = c(0,0))+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text.x = element_text(face='bold'))
lt.trajectory.sygen.plot

# Save plot
ggsave(
  "lt.trajectory.sygen.plot.pdf",
  plot = lt.trajectory.sygen.plot,
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
