library(ggplot2)
library(grid)


#Clear workspace
rm(list = ls())

#where libraries are stored
.libPaths()

#paths
outdir_figures='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen/Figures'
outdir_tables='/Users/jutzca/Documents/Github/SCI_Neurological_Recovery/Sygen/Tables'


#load original dataset
sygen<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/df_sygen_formatted.csv", sep = ',', header = T,  na.strings=c("","NA"))

#Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
sygen.included.cohort<- subset(sygen, (Sex=="Female" | Sex=="Male") & ###Age at DOI and Sex
                                 (NLI == 'cervical' | NLI == 'thoracic')&   ## Neurological level
                                 (AIS=="AIS A"| AIS=="AIS B"| AIS=="AIS C"| AIS=="AIS D")) #AIS Grades


levels(sygen.included.cohort$AIS) <- c("AIS-A", "AIS-B ", "AIS-C", "AIS-D")
abbrev_x <- c("", "1992", ""  ,"1993"," ","1994","","1995"," ","1996"," ","1997")

plot2 <-ggplot(sygen.included.cohort,aes(x=New_timeline, y=as.numeric(LEMS), group=AIS)) +
  stat_summary(fun.data = "mean_cl_boot", geom="smooth", se = TRUE, color="black", size=0.5) +
  facet_grid(sygen.included.cohort$AIS~.)+theme_bw()+ ylab('LEMS')+
  #scale_x_continuous( limits = c(0, 317), breaks = seq(0, 317, 13), expand = c(0,0), labels=abbrev_x, position = "top" )+
  #scale_y_continuous( limits = c(0, 50), breaks = seq(0, 50, 25), expand = c(0,0))+
  theme(axis.title.x = element_blank(),panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text.x = element_text(face='bold'))

plot2

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

