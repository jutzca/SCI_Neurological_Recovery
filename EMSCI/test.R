sygen<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/df_sygen_formatted.csv", sep = ',', header = T,  na.strings=c("","NA"))

# Only include subject with information on sex, valid age at injury, traumatic or ischemic cause of injury, and level of injury either cervical, thoracic, or lumbar; as well as AIS score A, B, C, or D
sygen.included.cohort.all.times<- subset(sygen, (!is.na(Age)) & (Sex=="Female" | Sex=="Male") & ###Age at DOI and Sex
                                           (NLI == 'cervical' | NLI == 'thoracic')&   ## Neurological level
                                           (AIS=="AIS A"| AIS=="AIS B"| AIS=="AIS C"| AIS=="AIS D")) #AIS Grades

sygen.included.cohort.all.times.2 <- subset(sygen.included.cohort.all.times, (Plegia=='para' | Plegia=="tetra"))



# Add change scores baseline (week 0) to 52 weeks
change.scores <-read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/Sygen_change_scores.csv", sep = ',', header = T,  na.strings=c("","NA"))
sygen.new <- merge(sygen.included.cohort.all.times.2,change.scores, by="ID" )

# Add surgical information
surgial.timing <-read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/surgical_time2.csv", sep = ',', header = T,  na.strings=c("","NA"))

sygen.new <- merge(sygen.new,surgial.timing, by="ID" )

sygen.new2=subset(sygen.new,(ealy_vs_late=='late'| ealy_vs_late=='early'))



change.scores.1to52.lems.sygen <-ggplot(data=sygen.new2,aes(x=AIS, y=as.numeric(change_score_lems_0.52wks), fill=ealy_vs_late)) +
  geom_boxplot()+
  facet_grid(.~sygen.new2$Plegia)+
  theme_bw()+ ylab('Change in LEMS (1 week-52weeks)')+xlab("Time since injury [weeks]")+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
change.scores.1to52.lems.sygen




change.scores.0to52.lems.sygen <-ggplot(data=sygen.new,aes(x=AIS, y=as.numeric(change_score_lems_0.52wks), fill=ealy_vs_late)) +
  geom_boxplot()+
  facet_grid(.~sygen.new$Plegia)+
  theme_bw()+ ylab('Change in LEMS (0 week-52weeks)')+xlab("Time since injury [weeks]")+
  #scale_x_continuous( limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0,0), labels=abbrev_x, position = "top" )+
  theme(panel.spacing = unit(1, "lines"), axis.ticks.x = element_blank(),
        axis.text = element_text(face='plain', size=10, family='Times', color = 'black'),
        axis.title = element_text(face='bold', size=12, family='Times', color = 'black'), 
        strip.text = element_text(face='bold', size=10, family='Times', color = 'black'),
        plot.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), panel.background = element_rect(fill='#EFF2F4', color="#EFF2F4"),
        panel.grid.minor=element_line(color = "#E2E2E2"), panel.grid.major=element_line(color = "#E2E2E2"),
        legend.background = element_rect(fill='#EFF2F4', color="#EFF2F4"), legend.title = element_blank(), legend.position = 'bottom')
change.scores.0to52.lems.sygen


ggplot(sygen.new, aes(x=sygen.new$asiatot01))+
  geom_histogram() + theme_bw() +facet_grid(~sygen.new$ealy_vs_late)

model.aisa <- lm(change_score_tms_1.52wks~asiatot00+Age+Sex+ealy_vs_late, data=subset(sygen.new,((ealy_vs_late=='late'| ealy_vs_late=='early') & AIS=='AIS A')))
summary(model.aisa)

model.aisb <- lm(change_score_tms_1.52wks~asiatot00+Age+Sex+ealy_vs_late, data=subset(sygen.new,((ealy_vs_late=='late'| ealy_vs_late=='early') & AIS=='AIS B')))
summary(model.aisb)

model.aisc <- lm(change_score_tms_1.52wks~asiatot00+Age+Sex+ealy_vs_late, data=subset(sygen.new,((ealy_vs_late=='late'| ealy_vs_late=='early') & AIS=='AIS C')))
summary(model.aisc)

model.aisd <- lm(change_score_tms_1.52wks~asiatot00+Age+Sex+ealy_vs_late, data=subset(sygen.new,((ealy_vs_late=='late'| ealy_vs_late=='early') & AIS=='AIS D')))
summary(model.aisd)


tab_model(
  model.aisa, model.aisb, model.aisc, model.aisd, 
  dv.labels = c("AIS A", "AIS B", "AIS C", "AIS D"),
  show.ci = F, 
  show.p = T, 
  show.std = T,
  transform = NULL
)





mixed.lmer.ais.a <- lmer(UEMS~ Age+Sex+Time+Plegia+ealy_vs_late+ (1|ID), data=subset(sygen.new,((ealy_vs_late=='late'| ealy_vs_late=='early') & AIS=='AIS A')), control=lmerControl(check.nlev.gtr.1="ignore"))
print(summary(mixed.lmer.ais.a))


mixed.lmer.ais.b <- lmer(UEMS~ Age+Sex+Time+Plegia+ealy_vs_late+ (1|ID), data=subset(sygen.new,((ealy_vs_late=='late'| ealy_vs_late=='early') & AIS=='AIS B')), control=lmerControl(check.nlev.gtr.1="ignore"))
print(summary(mixed.lmer.ais.b))

mixed.lmer.ais.c <- lmer(UEMS~ Age+Sex+Time+Plegia+ealy_vs_late+ (1|ID), data=subset(sygen.new,((ealy_vs_late=='late'| ealy_vs_late=='early') & AIS=='AIS C')), control=lmerControl(check.nlev.gtr.1="ignore"))
print(summary(mixed.lmer.ais.c))

mixed.lmer.ais.d <- lmer(UEMS~ Age+Sex+Time+Plegia+ealy_vs_late+ (1|ID), data=subset(sygen.new,((ealy_vs_late=='late'| ealy_vs_late=='early') & AIS=='AIS D')), control=lmerControl(check.nlev.gtr.1="ignore"))
print(summary(mixed.lmer.ais.d))



mixed.lmer.ais.d <- lmer(UEMS~ Age+Sex+Time+Plegia+ealy_vs_late+AIS+ (1|ID), data=subset(sygen.new,((ealy_vs_late=='late'| ealy_vs_late=='early'))), control=lmerControl(check.nlev.gtr.1="ignore"))
print(summary(mixed.lmer.ais.d))


tab_model(
  mixed.lmer.ais.a,  mixed.lmer.ais.c, mixed.lmer.ais.d, 
  dv.labels = c("AIS A",  "AIS C", "AIS D"),
  show.ci = F, 
  show.p = T, 
  transform = NULL
)









baseline score, age, mechanism of injury, AIS grade, level of injury, and administration of methylprednisolone

ais.score<-unique(sygen.new$AIS)

results <- data.frame()
    for (i in ais.score){
      print(paste("MODEL", i,  sep = " "))
      model <- lm(change_score_tms_1.52wks~lower01+Age+Sex+ealy_vs_late, data=subset(sygen.new,(ealy_vs_late=='late'| ealy_vs_late=='early')))
      print(summary(model))
      
      tab_model(model)
      }
      
      
      # Capture summary stats
      intercept.estimate <- coef(summary(model))[1]
      lower01 <- coef(summary(model))[2]
      age.estimate <- coef(summary(model))[3]
      sex.estimate <- coef(summary(model))[4]
      surgery.estimate <- coef(summary(model))[5]
      intercept.std <- coef(summary(model))[6]
      lower01.std <- coef(summary(model))[7]
      age.std <- coef(summary(model))[8]
      sex.std <- coef(summary(model))[9]
      surgery.std <- coef(summary(model))[10]
      intercept.df <- coef(summary(model))[11]
      lower01.df <- coef(summary(model))[12]
      age.df <- coef(summary(model))[13]
      sex.df <- coef(summary(model))[14]
      surgery.df <- coef(summary(model))[15]
      intercept.tval <- coef(summary(model))[16]
      lower01.tval <- coef(summary(model))[17]
      age.tval <- coef(summary(model))[18]
      sex.tval <- coef(summary(model))[19]
      surgery.tval <- coef(summary(model))[20]
      intercept.pval <- coef(summary(model))[21]
      lower01.pval <- coef(summary(model))[22]
      age.pval <- coef(summary(model))[23]
      sex.pval <- coef(summary(model))[24]
      surgery.pval <- coef(summary(model))[25]
      
      # Get coefficents of mixed.lmer
      cfit <- coef(summary(mixed.lmer))
      
      # Create temporary data frame
      df <- data.frame(AIS = i, intercept.estimate = cfit[1], 
                       lower01.estimate = cfit[2], 
                       age.estimate =cfit[3],
                       sex.estimate= cfit[4], 
                       surgery.estimate= cfit[5],
                       intercept.std= cfit[6],
                       lower01.std =cfit[7],
                       age.std =cfit[8],
                       sex.std =cfit[9],
                       surgery.std =cfit[10],
                       intercept.df =cfit[11],
                       lower01.df =cfit[12],
                       age.df =cfit[13],
                       sex.df =cfit[14],
                       surgery.df =cfit[15],
                       intercept.tval =cfit[16],
                       lower01.tval =cfit[17],
                       age.tval =cfit[18],
                       sex.tval =cfit[19],
                       surgery.tval =cfit[20],
                       intercept.pval =cfit[21],
                       lower01.pval =cfit[22],
                       age.pval =cfit[23],
                       sex.pval =cfit[24],
                       surgery.pval =cfit[25],
                       stringsAsFactors = F)
      
     
      
      # Bind rows of temporary data frame to the results data frame
      results <- rbind(results, df)
      #results<- cbind(results, n)
    }








