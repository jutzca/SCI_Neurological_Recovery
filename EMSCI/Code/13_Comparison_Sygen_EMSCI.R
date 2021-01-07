## ---------------------------
##
## Script name: 13_Comparison_Sygen_EMSCI
##
## Purpose of script: To visualize the longitudinal neurological scores between sygen and emsci
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2021-1-6
##
## Copyright (c) Catherine Jutzeler, 2020
## Email: catherine.jutzeler@bsse.ethz.ch
##
## ---------------------------
##
## Data source: Sygen Clinical Trial and EMSCI study
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


#### ---------------------------
#Set output directorypaths
outdir_figures='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Figures'
outdir_tables='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI/Tables'


#load sygen original dataset
emsci.sygen.merged<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_sygen_merged_for_visualization.csv", sep = ',', header = T,  na.strings=c("","NA"))


###Create longitudinal lems trajectory figure
tms.trajectory.merged.plot <- ggplot() +
  stat_summary(aes(x = as.numeric(ExamStage_weeks),y = as.numeric(TMS), color=study), 
               data=subset(emsci.sygen.merged), 
               fun.data = "mean_sdl", geom="smooth", se = TRUE, size=1)+
  facet_grid(emsci.sygen.merged$plegia~emsci.sygen.merged$AIS)+
  ylab('Total Motor Score')+xlab("Weeks Post Injury")+
  scale_x_continuous( limits = c(0, 52), breaks = seq(0, 52, 10), expand = c(0,0))+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text.x = element_text(face='bold'))
tms.trajectory.merged.plot

ggsave(
  "tms.trajectory.merged.plott.pdf",
  plot = tms.trajectory.merged.plot,
  device = 'pdf',
  path = outdir_figures,   
  scale = 1,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)



