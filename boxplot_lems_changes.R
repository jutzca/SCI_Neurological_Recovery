


#------Baseline LEMS vs 52 weeks LEMS

emsci.trauma.sex.va.chronic<-distinct(subset(emsci.trauma.sex, ExamStage=='chronic'), Patientennummer, .keep_all = TRUE)

ggplot(emsci.trauma.sex.va.chronic,aes(as.factor(AIS), as.numeric(SCIM3_TotalScore), fill=X5_year_bins))+  geom_boxplot()+
  facet_grid(.~emsci.trauma.sex.va.a1$plegia)+scale_fill_brewer(palette="RdBu")+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 10, face = "bold"),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0)