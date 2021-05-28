## ---------------------------
##
## Script name: Sex_ratio_distribution_sygen
##
## Purpose of script: To determine if and to what extent the ratio between female and male SCI patients changed between 1992 and 1997.
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
## Data source: Sygen Clinical Trial
##
## Notes: Code for the publication XXX
##   
#### ---------------------------

## set working directory for Mac and PC

setwd("/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen") 

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

sygen.included.cohort <- distinct(subset(sygen.included.cohort.all.times, Time==0 | Time==1), ID, .keep_all = TRUE)

# Rescale Data
rescale.many <- function(dat, column.nos) { 
  nms <- names(dat) 
  for(col in column.nos) { 
    name <- paste(nms[col],".rescaled", sep = "") 
    dat[name] <- rescale(dat[,col], center = TRUE, scale = TRUE) 
  } 
  cat(paste("Rescaled ", length(column.nos),      " variable(s)n")) 
  dat 
} 

sygen.included.cohort <-rescale.many(sygen.included.cohort, c(9)) 

#---------- Count percentage of female and male subjects by Year of Injury - OVERAL --------#

sex_ratios.overall.sygen = sygen.included.cohort %>%
  dplyr::count(Sex, YEARDOI.rescaled) %>%
  dplyr::group_by(YEARDOI.rescaled)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

# Reshape data from long to wide in order to calculate ratios
sex_ratios.overall.sygen <- dcast(sex_ratios.overall.sygen, YEARDOI.rescaled ~ Sex, value.var="n")
sex_ratios.overall.sygen

# Calculate ratios
sex_ratios.overall.sygen$Ratios <-
  case_when(
    is.na(sex_ratios.overall.sygen$Male) & is.na(sex_ratios.overall.sygen$Female) ~ 1,
    is.na(sex_ratios.overall.sygen$Male) & sex_ratios.overall.sygen$Female >= 0 ~ 1,
    sex_ratios.overall.sygen$Male >= 0 & is.na(sex_ratios.overall.sygen$Female) ~ 0,
    TRUE ~ sex_ratios.overall.sygen$Male / sex_ratios.overall.sygen$Female
  )

# Run LM to investigate if there was a change in sex ratio between 2001 and 2019
sex_ratios.overall.sygen.lm <- lm(Ratios~YEARDOI.rescaled, data=sex_ratios.overall.sygen)
summary(sex_ratios.overall.sygen.lm)

#---------- Count percentage of female and male subjects by Year of Injury - Per AIS Grades --------#
sex_ratios.ais.sygen = sygen.included.cohort %>%
 dplyr::count(Sex, YEARDOI.rescaled, AIS) %>%
  dplyr::group_by(YEARDOI.rescaled)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

# Reshape data from long to wide in order to calculate ratios
sex_ratios.ais.sygen <- dcast(sex_ratios.ais.sygen, YEARDOI.rescaled ~ Sex+AIS, value.var="n")
sex_ratios.ais.sygen

# Calculate ratios
sex_ratios.ais.sygen$Ratios_A <-
  case_when(
    is.na(sex_ratios.ais.sygen$`Male_AIS A`) & is.na(sex_ratios.ais.sygen$`Female_AIS A`) ~ 1,
    is.na(sex_ratios.ais.sygen$`Male_AIS A`) & sex_ratios.ais.sygen$`Female_AIS A` >= 0 ~ 1,
    sex_ratios.ais.sygen$`Male_AIS A` >= 0 & is.na(sex_ratios.ais.sygen$`Female_AIS A`) ~ 0,
    TRUE ~ sex_ratios.ais.sygen$`Male_AIS A`/ sex_ratios.ais.sygen$`Female_AIS A`
  )

sex_ratios.ais.sygen$Ratios_B <-
  case_when(
    is.na(sex_ratios.ais.sygen$`Male_AIS B`) & is.na(sex_ratios.ais.sygen$`Female_AIS B`) ~ 1,
    is.na(sex_ratios.ais.sygen$`Male_AIS B`) & sex_ratios.ais.sygen$`Female_AIS B` >= 0 ~ 1,
    sex_ratios.ais.sygen$`Male_AIS B` >= 0 & is.na(sex_ratios.ais.sygen$`Female_AIS B`) ~ 0,
    TRUE ~ sex_ratios.ais.sygen$`Male_AIS B`/ sex_ratios.ais.sygen$`Female_AIS B`
  )

sex_ratios.ais.sygen$Ratios_C <-
  case_when(
    is.na(sex_ratios.ais.sygen$`Male_AIS C`) & is.na(sex_ratios.ais.sygen$`Female_AIS C`) ~ 1,
    is.na(sex_ratios.ais.sygen$`Male_AIS C`) & sex_ratios.ais.sygen$`Female_AIS C` >= 0 ~ 1,
    sex_ratios.ais.sygen$`Male_AIS C` >= 0 & is.na(sex_ratios.ais.sygen$`Female_AIS C`) ~ 0,
    TRUE ~ sex_ratios.ais.sygen$`Male_AIS C`/ sex_ratios.ais.sygen$`Female_AIS C`
  )


sex_ratios.ais.sygen$Ratios_D <-
  case_when(
    is.na(sex_ratios.ais.sygen$`Male_AIS D`) & is.na(sex_ratios.ais.sygen$`Female_AIS D`) ~ 1,
    is.na(sex_ratios.ais.sygen$`Male_AIS D`) & sex_ratios.ais.sygen$`Female_AIS D` >= 0 ~ 1,
    sex_ratios.ais.sygen$`Male_AIS D` >= 0 & is.na(sex_ratios.ais.sygen$`Female_AIS D`) ~ 0,
    TRUE ~ sex_ratios.ais.sygen$`Male_AIS D`/ sex_ratios.ais.sygen$`Female_AIS D`
  )

# Run LM to investigate if there was a change in sex ratio between 2001 and 2019
sex_ratios.sygen.ais.a.lm <- lm(Ratios_A~YEARDOI.rescaled, data=sex_ratios.ais.sygen)
summary(sex_ratios.sygen.ais.a.lm)

# Run LM to investigate if there was a change in sex ratio between 2001 and 2019
sex_ratios.sygen.ais.b.lm <- lm(Ratios_B~YEARDOI.rescaled, data=sex_ratios.ais.sygen)
summary(sex_ratios.sygen.ais.b.lm)

# Run LM to investigate if there was a change in sex ratio between 2001 and 2019
sex_ratios.sygen.ais.c.lm <- lm(Ratios_C~YEARDOI.rescaled, data=sex_ratios.ais.sygen)
summary(sex_ratios.sygen.ais.c.lm)

# Run LM to investigate if there was a change in sex ratio between 2001 and 2019
sex_ratios.sygen.d.lm <- lm(Ratios_D~YEARDOI.rescaled, data=sex_ratios.ais.sygen)
summary(sex_ratios.sygen.d.lm)


#---------- Count percentage of female and male subjects by Year of Injury - PLEGIA --------#

sex_ratios.plegia.sygen = sygen.included.cohort %>%
  dplyr::count(Sex, YEARDOI.rescaled, Plegia) %>%
  dplyr::group_by(YEARDOI.rescaled)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

# Reshape data from long to wide in order to calculate ratios
sex_ratios.plegia.sygen <- dcast(sex_ratios.plegia.sygen, YEARDOI.rescaled ~ Sex+Plegia, value.var="n")
sex_ratios.plegia.sygen

# Calculate ratios
sex_ratios.plegia.sygen$Ratios_para <-
  case_when(
    is.na(sex_ratios.plegia.sygen$Male_para) & is.na(sex_ratios.plegia.sygen$Female_para) ~ 1,
    is.na(sex_ratios.plegia.sygen$Male_para) & sex_ratios.plegia.sygen$Female_para >= 0 ~ 1,
    sex_ratios.plegia.sygen$Male_para >= 0 & is.na(sex_ratios.plegia.sygen$Female_para) ~ 0,
    TRUE ~ sex_ratios.plegia.sygen$Male_para/ sex_ratios.plegia.sygen$Female_para
  )

sex_ratios.plegia.sygen$Ratios_tetra <-
  case_when(
    is.na(sex_ratios.plegia.sygen$Male_tetra) & is.na(sex_ratios.plegia.sygen$Female_tetra) ~ 1,
    is.na(sex_ratios.plegia.sygen$Male_tetra) & sex_ratios.plegia.sygen$Female_tetra >= 0 ~ 1,
    sex_ratios.plegia.sygen$Male_tetra >= 0 & is.na(sex_ratios.plegia.sygen$Female_tetra) ~ 0,
    TRUE ~ sex_ratios.plegia.sygen$Male_tetra/ sex_ratios.plegia.sygen$Female_tetra
  )

# Run LM to investigate if there was a change in sex ratio between 2001 and 2019
sex_ratios.sygen.tetra.lm <- lm(Ratios_tetra~YEARDOI.rescaled, data=sex_ratios.plegia.sygen)
summary(sex_ratios.sygen.tetra.lm)

# Run LM to investigate if there was a change in sex ratio between 2001 and 2019
sex_ratios.sygen.para.lm <- lm(Ratios_para~YEARDOI.rescaled, data=sex_ratios.plegia.sygen)
summary(sex_ratios.sygen.para.lm)


#### --------------------------- Visualize Results ---------------------------####

#---------- Calculate the number of patients per year by sex - Overall --------#

sygen.sex.long <- sygen.included.cohort %>%
  dplyr::count(Sex, YEARDOI) %>%
  dplyr::group_by(YEARDOI)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

#---------- Plot population pyramide for year and color by sex - OVERALL --------#

# Plot data for the male patients
gg.male.sygen <- ggplot(data = subset(sygen.sex.long,Sex=='Male'), 
                        mapping = aes(
                          x = as.factor(YEARDOI), 
                          y = frequency, 
                          fill = Sex,
                          label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                        )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=as.vector("#3E606F"))+
  geom_text(hjust=(1.1), size=3.5, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 14),
        legend.text.align = 0)+ 
  ggtitle("Male") + 
  coord_flip()    
gg.male.sygen

# Plot data for the female patients
gg.female.sygen <-  ggplot(data = subset(sygen.sex.long,Sex=='Female'), 
                           mapping = aes(
                             x = as.factor(YEARDOI), 
                             y = frequency, 
                             fill = Sex,
                             label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                           )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(1), size=3.5, colour="#5D646F") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0)+ 
  ggtitle("Female") + 
  coord_flip()
gg.female.sygen

# Putting the graphs together
sex.overall.sygen <- grid.arrange(gg.female.sygen,
                                  gg.male.sygen,
                                  widths=c(0.4,0.4),
                                  ncol=2,  top = textGrob("Overall",gp=gpar(fontsize=12,font=2))
)

# Save plot
ggsave(
  "sex.overall.sygen.pdf",
  plot = sex.overall.sygen,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Calculate the number of patients per year by sex - PARAPLEGIA --------#

sygen.included.cohort.para <- subset(sygen.included.cohort, Plegia=='para') %>%
  dplyr::count(Sex, YEARDOI)%>%
  dplyr::group_by(YEARDOI)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

#---------- Plot population pyramide for year and color by sex- PARAPLEGIA --------#

# Plot data for the male patients
gg.male.para.sygen <- ggplot(data = subset(sygen.included.cohort.para,Sex=='Male'), 
                             mapping = aes(
                               x = as.factor(YEARDOI), 
                               y = frequency, 
                               fill = Sex,
                               label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                             )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=as.vector("#3E606F"))+
  geom_text(hjust=(1.1), size=3.5, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 14),
        legend.text.align = 0)+ 
  ggtitle("Male") + 
  coord_flip()    
gg.male.para.sygen

# Plot data for the female patients
gg.female.para.sygen <-  ggplot(data = subset(sygen.included.cohort.para,Sex=='Female'), 
                                mapping = aes(
                                  x = as.factor(YEARDOI), 
                                  y = frequency, 
                                  fill = Sex,
                                  label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                                )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(1), size=3.5, colour="#5D646F") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0)+ 
  ggtitle("Female") + 
  coord_flip()
gg.female.para.sygen

# Putting the graphs together
sex.paraplegia.sygen <- grid.arrange(gg.female.para.sygen,
                                     gg.male.para.sygen,
                                     widths=c(0.4,0.4),
                                     ncol=2,  top = textGrob("Paraplegia",gp=gpar(fontsize=12,font=2))
)

# Save plot
ggsave(
  "sex.paraplegia.sygen.pdf",
  plot = sex.paraplegia.sygen,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Calculate the number of patients per year by sex - TETRAPLEGIA --------#

sygen.included.cohort.tetra <- subset(sygen.included.cohort, Plegia=='tetra') %>%
  dplyr::count(Sex, YEARDOI)%>%
  dplyr::group_by(YEARDOI)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

#---------- Plot population pyramide for year and color by Sex - TETRAPLEGIA ----

# Plot data for the male patients
gg.male.tetra.sygen <- ggplot(data = subset(sygen.included.cohort.tetra,Sex=='Male'), 
                              mapping = aes(
                                x = as.factor(YEARDOI), 
                                y = frequency, 
                                fill = Sex,
                                label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                              )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=as.vector("#3E606F"))+
  geom_text(hjust=(1.1), size=3.5, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 14),
        legend.text.align = 0)+ 
  ggtitle("Male") + 
  coord_flip()   
gg.male.tetra.sygen

# Plot data for the female patients
gg.female.tetra.sygen <-  ggplot(data = subset(sygen.included.cohort.tetra,Sex=='Female'), 
                                 mapping = aes(
                                   x = as.factor(YEARDOI), 
                                   y = frequency, 
                                   fill = Sex,
                                   label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                                 )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(1), size=3.5, colour="#5D646F") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0)+ 
  ggtitle("Female") + 
  coord_flip()
gg.female.tetra.sygen

# Putting the graphs together
sex.tetrapelgia.sygen <- grid.arrange(gg.female.tetra.sygen,
                                      gg.male.tetra.sygen,
                                      widths=c(0.4,0.4),
                                      ncol=2,  top = textGrob("Tetraplegia",gp=gpar(fontsize=12,font=2))
)

# Save plot
ggsave(
  "sex.tetrapelgia.sygen.pdf",
  plot = sex.tetrapelgia.sygen,
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
