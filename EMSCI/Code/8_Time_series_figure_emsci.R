## ---------------------------
##
## Script name: 8_Time_series_figure_emsci
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
                             (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar') & (YEARDOI >= 2000))


emsci.ais <- subset(emsci.trauma.sex, (AIS=="A"| AIS=="B"| AIS=="C"| AIS=="D") & (NLI_level == 'thoracic'| NLI_level == 'lumbar'|NLI_level == 'cervical') )
#emsci.ais <- subset(emsci.trauma.sex,NLI_level=='thoracic'|NLI_level=='lumbar')


levels(emsci.ais$AIS) <- c("AIS-A", "AIS-B ", "AIS-C", "AIS-D", " ", "")
abbrev_x <- c("", "2001/02", ""  ,"2003 "," ","2004","","2005"," ","2006"," ","2007"," ","2008"," ","2009 "," ","2010"," ","2011"," ","2012"," ","2013 "," ","2014 "," ","2015"," ","2016"," ","2017"," ","2018/19", '')

plot <-ggplot(emsci.ais,aes(x=newtimeline, y=as.numeric(as.character(X10m)), group=AIS)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="black", size=0.5) +
  facet_grid(emsci.ais$AIS~.)+theme_bw()+ ylab('10m Walking Test [s]')+
  scale_x_continuous( limits = c(0, 884), breaks = seq(0, 884, 26), expand = c(0,0), labels=abbrev_x, position = "top" )+
  scale_y_continuous( limits = c(0, 50), breaks = seq(0, 40, 10), expand = c(0,0))+
  theme(axis.title.x = element_blank(),panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text.x = element_text(face='bold'))

plot

# Construct a grid.layout that is the same as the ggplot layout
gt = ggplotGrob(plot)
lay = grid.layout(nrow = length(gt$heights), ncol = length(gt$widths),
                  widths = gt$widths, heights = gt$heights)

# Push to this viewport
pushViewport(viewport(layout = lay))

# Within the layout, push to a viewport that spans the plot panels.
pos = gt$layout[grep("panel", gt$layout$name), c("t", "l")]  # Positions of the panels
pushViewport(viewport(layout.pos.col = pos$l, layout.pos.row = min(pos$t):max(pos$t)))

x.axis.limits = summarise_layout(ggplot_build(plot))[1, c('xmin', 'xmax')]


# Set up a data viewport,
# so that the x-axis units are, in effect, "native", 
# but y-axis units are, in effect, "npc".
# And push to the data viewport.
pushViewport(dataViewport(yscale = c(0, 1), 
                          xscale = x.axis.limits))

# Draw the rectangle
grid.rect(x = 52, y = 0,
          width = 52, height = 1,
          just = c("left", "bottom"), default.units = "native",
          gp = gpar(fill = "red", col = "transparent", alpha = .15))

grid.rect(x = 156, y = 0,
          width = 52, height = 1,
          just = c("left", "bottom"), default.units = "native",
          gp = gpar(fill = "red", col = "transparent", alpha = .15))
grid.rect(x = 260, y = 0,
          width = 52, height = 1,
          just = c("left", "bottom"), default.units = "native",
          gp = gpar(fill = "red", col = "transparent", alpha = .15))
grid.rect(x = 364, y = 0,
          width = 52, height = 1,
          just = c("left", "bottom"), default.units = "native",
          gp = gpar(fill = "red", col = "transparent", alpha = .15))
grid.rect(x = 468, y = 0,
          width = 52, height = 1,
          just = c("left", "bottom"), default.units = "native",
          gp = gpar(fill = "red", col = "transparent", alpha = .15))
grid.rect(x = 572, y = 0,
          width = 52, height = 1,
          just = c("left", "bottom"), default.units = "native",
          gp = gpar(fill = "red", col = "transparent", alpha = .15))
grid.rect(x = 676, y = 0,
          width = 52, height = 1,
          just = c("left", "bottom"), default.units = "native",
          gp = gpar(fill = "red", col = "transparent", alpha = .15))
grid.rect(x = 780, y = 0,
          width = 52, height = 1,
          just = c("left", "bottom"), default.units = "native",
          gp = gpar(fill = "red", col = "transparent", alpha = .15))

# Back to the root viewport
upViewport(0)

#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####
