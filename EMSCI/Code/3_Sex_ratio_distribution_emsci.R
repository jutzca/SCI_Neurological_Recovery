## ---------------------------
##
## Script name: Sex_ratio_distribution_emsci
##
## Purpose of script: To determine if and to what extent the ratio between female and male SCI patients changed between 2001 and 2019.
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
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T,  na.strings=c("","NA"))


# Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
emsci.trauma.sex <- subset(emsci, (AgeAtDOI > 8) & (Sex=='f' | Sex=='m') & ###Age at DOI and Sex
                             (Cause=="ischemic" | Cause=="traumatic" | Cause=="haemorragic" |Cause=="disc herniation") & 
                             (NLI_level == 'cervical' | NLI_level == 'thoracic'| NLI_level == 'lumbar')&   ## Neurological level
                             (AIS=="A"| AIS=="B"| AIS=="C"| AIS=="D")) #AIS Grades

#---------- Data Analysis: Change in ratio between female and male patients over 20 years and display in table ----

# Subset data to only patients with valid entry at stage 'very acute' or 'acute I' and remove duplicate patient numbers
emsci.trauma.sex.va.a1<-distinct(subset(emsci.trauma.sex, ExamStage=='acute I' | ExamStage=='very acute'), Patientennummer, .keep_all = TRUE)

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

emsci.trauma.sex.va.a1 <-rescale.many(emsci.trauma.sex.va.a1, c(8)) 


#---------- Count percentage of female and male subjects by Year of Injury - OVERALL --------#
sex_ratios.overall = emsci.trauma.sex.va.a1 %>%
  dplyr::count(Sex, YEARDOI.rescaled) %>%
  dplyr::group_by(YEARDOI.rescaled)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

# Reshape data from long to wide in order to calculate ratios
sex_ratios.overall <- dcast(sex_ratios.overall, YEARDOI.rescaled ~ Sex, value.var="n")
sex_ratios.overall

# Calculate ratios
sex_ratios.overall$Ratios <-
  case_when(
    is.na(sex_ratios.overall$m) & is.na(sex_ratios.overall$f) ~ 1,
    is.na(sex_ratios.overall$m) & sex_ratios.overall$f >= 0 ~ 1,
    sex_ratios.overall$m >= 0 & is.na(sex_ratios.overall$f) ~ 0,
    TRUE ~ sex_ratios.overall$m / sex_ratios.overall$f
  )

# Run LM to investigate if there was a change in sex ratio between 2001 and 2019
sex_ratios.emsci.overall.lm <- lm(Ratios~YEARDOI.rescaled, data=sex_ratios.overall)
summary(sex_ratios.emsci.overall.lm)


#---------- Count percentage of female and male subjects by Year of Injury - AIS Grades --------#
sex_ratios = emsci.trauma.sex.va.a1 %>%
  dplyr::count(Sex, YEARDOI.rescaled, AIS) %>%
  dplyr::group_by(YEARDOI.rescaled)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

# Reshape data from long to wide in order to calculate ratios
sex_ratios <- dcast(sex_ratios, YEARDOI.rescaled ~ Sex+AIS, value.var="n")
sex_ratios

# Calculate ratios
sex_ratios$Ratios_A <-
  case_when(
    is.na(sex_ratios$m_A) & is.na(sex_ratios$f_A) ~ 1,
    is.na(sex_ratios$m_A) & sex_ratios$f_A >= 0 ~ 1,
    sex_ratios$m_A >= 0 & is.na(sex_ratios$f_A) ~ 0,
    TRUE ~ sex_ratios$m_A / sex_ratios$f_A
  )

sex_ratios$Ratios_B <-
  case_when(
    is.na(sex_ratios$m_B) & is.na(sex_ratios$f_B) ~ 1,
    is.na(sex_ratios$m_B) & sex_ratios$f_B >= 0 ~ 1,
    sex_ratios$m_B >= 0 & is.na(sex_ratios$f_B) ~ 0,
    TRUE ~ sex_ratios$m_B / sex_ratios$f_B
  )

sex_ratios$Ratios_C <-
  case_when(
    is.na(sex_ratios$m_C) & is.na(sex_ratios$f_C) ~ 1,
    is.na(sex_ratios$m_C) & sex_ratios$f_C >= 0 ~ 1,
    sex_ratios$m_C >= 0 & is.na(sex_ratios$f_C) ~ 0,
    TRUE ~ sex_ratios$m_C / sex_ratios$f_C
  )

sex_ratios$Ratios_D <-
  case_when(
    is.na(sex_ratios$m_D) & is.na(sex_ratios$f_D) ~ 1,
    is.na(sex_ratios$m_D) & sex_ratios$f_D >= 0 ~ 1,
    sex_ratios$m_D >= 0 & is.na(sex_ratios$f_D) ~ 0,
    TRUE ~ sex_ratios$m_D / sex_ratios$f_D
  )


# Run LM to investigate if there was a change in sex ratio between 2001 and 2019
sex_ratios.emsci.ais.a.lm <- lm(Ratios_A~YEARDOI.rescaled, data=sex_ratios)
summary(sex_ratios.emsci.ais.a.lm)

# Run LM to investigate if there was a change in sex ratio between 2001 and 2019
sex_ratios.emsci.ais.b.lm <- lm(Ratios_B~YEARDOI.rescaled, data=sex_ratios)
summary(sex_ratios.emsci.ais.b.lm)

# Run LM to investigate if there was a change in sex ratio between 2001 and 2019
sex_ratios.emsci.ais.c.lm <- lm(Ratios_C~YEARDOI.rescaled, data=sex_ratios)
summary(sex_ratios.emsci.ais.c.lm)

# Run LM to investigate if there was a change in sex ratio between 2001 and 2019
sex_ratios.emsci.ais.d.lm <- lm(Ratios_D~YEARDOI.rescaled, data=sex_ratios)
summary(sex_ratios.emsci.ais.d.lm)


#---------- Count percentage of female and male subjects by Year of Injury - Plegia --------#
sex_ratios.plegia = emsci.trauma.sex.va.a1 %>%
  dplyr::count(Sex, YEARDOI.rescaled, plegia) %>%
  dplyr::group_by(YEARDOI.rescaled)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

# Reshape data from long to wide in order to calculate ratios
sex_ratios.plegia <- dcast(sex_ratios.plegia, YEARDOI.rescaled ~ Sex+plegia, value.var="n")
sex_ratios.plegia

# Calculate ratios
sex_ratios.plegia$Ratios_para <-
  case_when(
    is.na(sex_ratios.plegia$m_para) & is.na(sex_ratios.plegia$f_para) ~ 1,
    is.na(sex_ratios.plegia$m_para) & sex_ratios.plegia$f_para >= 0 ~ 1,
    sex_ratios.plegia$m_para >= 0 & is.na(sex_ratios.plegia$f_para) ~ 0,
    TRUE ~ sex_ratios.plegia$m_para / sex_ratios.plegia$f_para
  )

sex_ratios.plegia$Ratios_tetra <-
  case_when(
    is.na(sex_ratios.plegia$m_tetra) & is.na(sex_ratios.plegia$f_tetra) ~ 1,
    is.na(sex_ratios.plegia$m_tetra) & sex_ratios.plegia$f_tetra >= 0 ~ 1,
    sex_ratios.plegia$m_tetra >= 0 & is.na(sex_ratios.plegia$f_tetra) ~ 0,
    TRUE ~ sex_ratios.plegia$m_tetra / sex_ratios.plegia$f_tetra
  )

# Run LM to investigate if there was a change in sex ratio between 2001 and 2019
sex_ratios.emsci.para.lm <- lm(Ratios_para~YEARDOI.rescaled, data=sex_ratios.plegia)
summary(sex_ratios.emsci.para.lm)

# Run LM to investigate if there was a change in sex ratio between 2001 and 2019
sex_ratios.emsci.tetra.lm <- lm(Ratios_tetra~YEARDOI.rescaled, data=sex_ratios.plegia)
summary(sex_ratios.emsci.tetra.lm)

#---------- Calculate the number of patients per year by sex - Overall --------#
emsci.sex.long <- emsci.trauma.sex.va.a1 %>%
  dplyr::count(Sex, X2_year_bins)%>%
  dplyr::group_by(X2_year_bins)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

#---------- Plot population pyramide for year and color by sex - OVERALL --------#
# Plot data for the male patients
gg.male <- ggplot(data = subset(emsci.sex.long,Sex=='m'), 
                  mapping = aes(
                    x = as.factor(X2_year_bins), 
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

# Plot data for the female patients
gg.female <-  ggplot(data = subset(emsci.sex.long,Sex=='f'), 
                     mapping = aes(
                       x = as.factor(X2_year_bins), 
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

# Putting the graphs together together
sex.overall <- grid.arrange(gg.female,
             gg.male,
             widths=c(0.4,0.4),
             ncol=2,  top = textGrob("Overall",gp=gpar(fontsize=12,font=2))
)


ggsave(
  "sex.ratio.overall.emsci.pdf",
  plot = sex.overall,
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
emsci.sex.long.para <- subset(emsci.trauma.sex.va.a1, plegia=='para') %>%
  dplyr::count(Sex, YEARDOI)%>%
  dplyr::group_by(YEARDOI)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

#---------- Plot population pyramide for year and color by Sex - PARAPLEGIA --------#
# Plot data for the male patients
gg.male.para <- ggplot(data = subset(emsci.sex.long.para,Sex=='m'), 
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

# Plot data for the female patients
gg.female.para <-  ggplot(data = subset(emsci.sex.long.para,Sex=='f'), 
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

# Putting the graphs together together
sex.paraplegia <- grid.arrange(gg.female.para,
             gg.male.para,
             widths=c(0.4,0.4),
             ncol=2,  top = textGrob("Paraplegia",gp=gpar(fontsize=12,font=2))
)


ggsave(
  "sex.ratio.para.emsci.pdf",
  plot = sex.paraplegia,
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
emsci.sex.long.tetra <- subset(emsci.trauma.sex.va.a1, plegia=='tetra') %>%
  dplyr::count(Sex, YEARDOI)%>%
  dplyr::group_by(YEARDOI)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

#---------- Plot population pyramide for year and color by Sex - TETRAPLEGIA --------#
# Plot data for the male patients
gg.male.tetra <- ggplot(data = subset(emsci.sex.long.tetra,Sex=='m'), 
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

# Plot data for the female patients
gg.female.tetra <-  ggplot(data = subset(emsci.sex.long.tetra,Sex=='f'), 
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

# Putting the graphs together together
sex.tetrapelgia <- grid.arrange(gg.female.tetra,
             gg.male.tetra,
             widths=c(0.4,0.4),
             ncol=2,  top = textGrob("Tetraplegia",gp=gpar(fontsize=12,font=2))
)

ggsave(
  "sex.ratio.tetra.emsci.pdf",
  plot = sex.tetrapelgia,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Calculate the number of patients per year by sex - AIS A --------#
emsci.sex.long.ais_a <- subset(emsci.trauma.sex.va.a1, AIS=='A') %>%
  dplyr::count(Sex, YEARDOI)%>%
  dplyr::group_by(YEARDOI)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

#---------- Plot population pyramide for year and color by Sex - AIS A --------#
#Plot data for the male patients
gg.male.ais_a <- ggplot(data = subset(emsci.sex.long.ais_a,Sex=='m'), 
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
gg.male.ais_a

# Plot data for the female patients
gg.female.ais_a <-  ggplot(data = subset(emsci.sex.long.ais_a,Sex=='f'), 
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

# Putting the graphs together together
sex.ais_a <- grid.arrange(gg.female.ais_a,
                               gg.male.ais_a,
                               widths=c(0.4,0.4),
                               ncol=2,  top = textGrob("AIS A",gp=gpar(fontsize=12,font=2))
)

sex.ais_a

ggsave(
  "sex.ratio.ais_a.emsci.pdf",
  plot = sex.ais_a,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Calculate the number of patients per year by sex - AIS B --------#
emsci.sex.long.ais_b <- subset(emsci.trauma.sex.va.a1, AIS=='B') %>%
  dplyr::count(Sex, YEARDOI)%>%
  dplyr::group_by(YEARDOI)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

#---------- Plot population pyramide for year and color by Sex - AIS B --------#
# Plot data for the male patients
gg.male.ais_b <- ggplot(data = subset(emsci.sex.long.ais_b,Sex=='m'), 
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
gg.male.ais_b

# Plot data for the female patients
gg.female.ais_b <-  ggplot(data = subset(emsci.sex.long.ais_b,Sex=='f'), 
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

# Putting the graphs together together
sex.ais_b <- grid.arrange(gg.female.ais_b,
                          gg.male.ais_b,
                          widths=c(0.4,0.4),
                          ncol=2,  top = textGrob("AIS B",gp=gpar(fontsize=12,font=2))
)

sex.ais_b

ggsave(
  "sex.ratio.ais_b.emsci.pdf",
  plot = sex.ais_b,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Calculate the number of patients per year by sex - AIS C --------#
emsci.sex.long.ais_c <- subset(emsci.trauma.sex.va.a1, AIS=='C') %>%
  dplyr::count(Sex, YEARDOI)%>%
  dplyr::group_by(YEARDOI)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)

#---------- Plot population pyramide for year and color by Sex - AIS C --------#
##Plot data for the male patients
gg.male.ais_c <- ggplot(data = subset(emsci.sex.long.ais_c,Sex=='m'), 
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
gg.male.ais_c

# Plot data for the female patients
gg.female.ais_c <-  ggplot(data = subset(emsci.sex.long.ais_c,Sex=='f'), 
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

# Putting the graphs together together
sex.ais_c <- grid.arrange(gg.female.ais_c,
                          gg.male.ais_c,
                          widths=c(0.4,0.4),
                          ncol=2,  top = textGrob("AIS C",gp=gpar(fontsize=12,font=2))
)

sex.ais_b

ggsave(
  "sex.ratio.ais_c.emsci.pdf",
  plot = sex.ais_c,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#---------- Calculate the number of patients per year by sex - AIS D --------#
emsci.sex.long.ais_d <- subset(emsci.trauma.sex.va.a1, AIS=='D') %>%
  dplyr::count(Sex, YEARDOI)%>%
  dplyr::group_by(YEARDOI)%>%
  dplyr::mutate(frequency = (n / sum(n))*100)
#---------- Plot population pyramide for year and color by Sex - AIS D --------#
# Plot data for the male patients
gg.male.ais_d <- ggplot(data = subset(emsci.sex.long.ais_d,Sex=='m'), 
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
gg.male.ais_d

# Plot data for the female patients
gg.female.ais_d <-  ggplot(data = subset(emsci.sex.long.ais_d,Sex=='f'), 
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

# Putting the graphs together together
sex.ais_d <- grid.arrange(gg.female.ais_d,
                          gg.male.ais_d,
                          widths=c(0.4,0.4),
                          ncol=2,  top = textGrob("AIS D",gp=gpar(fontsize=12,font=2))
)

sex.ais_d

ggsave(
  "sex.ratio.ais_d.emsci.pdf",
  plot = sex.ais_d,
  device = 'pdf',
  path = outdir_figures,    ###Set path to save figures
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

