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
## ---------------------------
##
## load up the packages we will need:  (uncomment as required)
library(ggplot2)
library(grid)
##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
# if(!require(ggplot2)){install.packages("ggplot2")}
# if(!require(grid)){install.packages("grid")}
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
setwd("/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/EMSCI") 
##
#### ---------------------------
##
## Set output directorypaths
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

levels(emsci.trauma.sex.ais.baseline$baseline.ais) <- c("AIS-A", "AIS-B ", "AIS-C", "AIS-D", " ", "")
abbrev_x <- c("", "'01/02", ""  ,"2003 "," ","2004","","2005"," ","2006"," ","2007"," ","2008"," ","2009 "," ","2010"," ","2011"," ","2012"," ","2013 "," ","2014 "," ","2015"," ","2016"," ","2017"," ","'18/19", '')

# Create plot for time series
plot <-ggplot(emsci.trauma.sex.ais.baseline,aes(x=newtimeline, y=as.numeric(as.character(SCIM23_TotalScore)), fill=baseline.ais, color=baseline.ais)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE,  size=0.5, color='black', fill="gray") +
  facet_grid(emsci.trauma.sex.ais.baseline$baseline.ais~.)+theme_bw()+ ylab('SCIM 2 and 3 Total Score')+
  scale_fill_manual(values = c('#0000b2',"#0000b2", "#0000b2", "#0000b2"))+scale_color_manual(values = c('#0000b2',"#0000b2", "#0000b2", "#0000b2" ))+
  # scale_fill_manual(values = c('#067eec',"#457fe1", "#0000b2", "#000047"))+scale_color_manual(values = c('#067eec',"#457fe1", "#0000b2", "#000047" ))+
  #scale_fill_manual(values = c("#457fe1", "#ff0000", "#ff9a00",'#059800' ))+
  #scale_color_manual(values = c("#457fe1", "#ff0000", "#ff9a00",'#059800' ))+
  scale_x_continuous( limits = c(0, 884), breaks = seq(0, 884, 26), expand = c(0,0), labels=abbrev_x, position = "top" )+
  scale_y_continuous( limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0,0))+
  theme(axis.title.x = element_blank(),panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text.x = element_text(face='plain', family = 'Times', color="black", size=10), 
       axis.text.y = element_text(face='plain', family = 'Times', color="black", size=10), legend.position = 'none')

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
          gp = gpar(fill = "#000047", col = "transparent", alpha = 0.15))

grid.rect(x = 156, y = 0,
          width = 52, height = 1,
          just = c("left", "bottom"), default.units = "native",
          gp = gpar(fill = "#000047", col = "transparent", alpha = 0.15))
grid.rect(x = 260, y = 0,
          width = 52, height = 1,
          just = c("left", "bottom"), default.units = "native",
          gp = gpar(fill = "#000047", col = "transparent", alpha = 0.15))
grid.rect(x = 364, y = 0,
          width = 52, height = 1,
          just = c("left", "bottom"), default.units = "native",
          gp = gpar(fill = "#000047", col = "transparent", alpha = 0.15))
grid.rect(x = 468, y = 0,
          width = 52, height = 1,
          just = c("left", "bottom"), default.units = "native",
          gp = gpar(fill = "#000047", col = "transparent", alpha = 0.15))
grid.rect(x = 572, y = 0,
          width = 52, height = 1,
          just = c("left", "bottom"), default.units = "native",
          gp = gpar(fill = "#000047", col = "transparent", alpha = 0.15))
grid.rect(x = 676, y = 0,
          width = 52, height = 1,
          just = c("left", "bottom"), default.units = "native",
          gp = gpar(fill = "#000047", col = "transparent", alpha = 0.15))
grid.rect(x = 780, y = 0,
          width = 52, height = 1,
          just = c("left", "bottom"), default.units = "native",
          gp = gpar(fill = "#000047", col = "transparent", alpha = 0.15))
# Back to the root viewport
upViewport(0)

# Save: Dimensions 4x7 inches

#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####
