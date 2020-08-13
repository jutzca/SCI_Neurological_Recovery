library(ggplot2)
library(plyr)
library(gridExtra)

## Plotting
gg.male <- ggplot(data = subset(age_distr,Sex=='m'), 
         mapping = aes(
           x = as.factor(YEARDOI), 
           y = AgeAtDOI, 
           fill = Sex,
           label=paste(round(AgeAtDOI, 0), "%", sep="")
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
       plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10), hjust = 0.030),
       plot.subtitle = element_text(size = 16, margin = margin(b = 20), hjust = 0.030),
       plot.caption = element_text(size = 14, margin = margin(b = 10, t = 50), color = "#5D646F"),
       axis.text.y = element_text(size = 12, color = "#5D646F"),
       strip.text = element_text(color = "#5D646F", size = 18, face = "bold", hjust = 0.030),
       plot.background = element_rect(fill = "#EFF2F4"),
       plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
       legend.position = "none",
       legend.margin  = unit(0.1, "lines"),
       legend.text  = element_text(size = 14),
       legend.text.align = 0)+ 
  ggtitle("Male") + facet_grid(.~age_distr$AgeAtDOI)+
  coord_flip()    

gg.female <-  ggplot(data = subset(df1,Sex=='f'), 
                     mapping = aes(
                       x = as.factor(YEARDOI), 
                       y = frequency, 
                       fill = Sex,
                       label=paste(round(frequency, 0), "%", sep="")
                     )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(-0.1), size=3.5, colour="#FFFFFF") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 14, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 14, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        strip.text = element_text(color = "#5D646F", size = 18, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0)+ 
  ggtitle("Female") + 
  coord_flip()

## Plutting it together
grid.arrange(gg.female,
             gg.male,
             widths=c(0.4,0.4),
             ncol=2
)

