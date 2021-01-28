## ---------------------------
##
## Script name: 7_Annual_incidence_figure_emsci
##
## Purpose of script: To determine the annual incidence of SCI between 2001 and 2019.
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
## load up the packages we will need:  (uncomment as required)
library(easyGgplot2)
library(ggthemes)
library(dplyr)
##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
# if(!require(easyGgplot2)){install.packages("easyGgplot2")}
# if(!require(ggthemes)){install.packages("ggthemes")}
# if(!require(dplyr)){install.packages("dplyr")}
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
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

# Load original dataset
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T)
emsci_incidence <- subset(emsci, ExamStage=='acute I' &  AgeAtDOI >= 0 & (!(is.na( Sex))))

# Calculate the annual incidence
value <-as.data.frame(emsci_incidence%>%
                        dplyr::count(YEARDOI))

# Plot the annual incidence
annual.incidence.emsci <- ggplot2.barplot(data=value, xName="YEARDOI", yName='n',
                width=0.9, color="black")+
  geom_text(aes(label=n), vjust=-0.12, color="black", size=3)+
  scale_y_continuous(expand = c(0.1,0)) +
  scale_x_discrete(breaks = c(2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019), expand=c(0,0) )+ 
  geom_hline(yintercept = 0) +
  #theme_economist(horizontal = FALSE) +
  labs(fill = "", x = "Year of Injury", y = "Number of Patients")+ ggtitle("Annual incidence (EMSCI)")+
  theme(axis.title = element_text(size = 13, face = 'bold'), 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
  )
annual.incidence.emsci

# Save Plot
ggsave(
  "annual.incidence.emsci.pdf",
  plot = annual.incidence.emsci,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

dev.off()


#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

  
 

