df<- read.csv(file.choose())  #spssv1_2.csv
surgery<- read.csv(file.choose()) #
################
# Calculate day and time which motor scores were measured
################
library(lubridate)
df$DTFW00_date<-as.Date(df$INJDT_date, format="%m/%d/%Y") + days(as.numeric(df$DTFW00_d))
df$DTFW01_date<-as.Date(df$INJDT_date, format="%m/%d/%Y") + days(as.numeric(df$DTFW01_d))
df$DTFW04_date<-as.Date(df$INJDT_date, format="%m/%d/%Y") + days(as.numeric(df$DTFW04_d))
df$DTFW08_date<-as.Date(df$INJDT_date, format="%m/%d/%Y") + days(as.numeric(df$DTFW08_d))
df$DTFW16_date<-as.Date(df$INJDT_date, format="%m/%d/%Y") + days(as.numeric(df$DTFW16_d))
df$DTFW26_date<-as.Date(df$INJDT_date, format="%m/%d/%Y") + days(as.numeric(df$DTFW26_d))
df$DTFW52_date<-as.Date(df$INJDT_date, format="%m/%d/%Y") + days(as.numeric(df$DTFW52_d))

#################
# merge traction time
df1<- merge(df, surgery, by.y="new_PTID", by.x = "PTID")

# calculate traction time post injury
# remove minutes and seconds from injury moment
df1$INJ_moment_n<- gsub(":.*", "", df1$INJ_moment)
df1$tract_mins_post_inj<- difftime(as.POSIXct(df1$tract_date_time, format="%Y-%m-%d %H:%M:%S"), as.POSIXct(df1$INJ_moment_n, format="%m/%d/%Y %H"), units="min" )
summary(as.numeric(df1$tract_mins_post_inj))

#subset dataframe to pts who had positive time for traction
df2<- subset(df1, df1$tract_mins_post_inj >=0)

#imputing ASIA evaluation score times, if blank, put in 12pm
df2$ASMTRT00_im<- ifelse(is.na(df2$ASMTRT00), "12", round(df2$ASMTRT00, digits=2))
df2$ASMTRT00_im<-round(as.numeric(df2$ASMTRT00_im), digits=2)
df2$ASMTRT01_im<- ifelse(is.na(df2$ASMTRT01), "12.00", df2$ASMTRT01)
df2$ASMTRT01_im<-round(as.numeric(df2$ASMTRT01_im), digits=2)

#put together date & time for motor score eval
df2$asia00_datetime<- strptime(paste(as.Date(df2$DTFW00_date, format="%m/%d/%Y"), 
                                 format(df2$ASMTRT00_im, digits=2)),format="%Y-%m-%d %H.%M")
df2$asia01_datetime<- strptime(paste(as.Date(df2$DTFW01_date, format="%m/%d/%Y"), 
                                 format(df2$ASMTRT01_im, digits=2)),format="%Y-%m-%d %H.%M")

#calculate minutes post injury for asia evaluation
df2$asia00_datetime_pi<- difftime(df2$asia00_datetime, as.POSIXct(df2$INJ_moment_n, format="%m/%d/%Y %H"), units = "min")
summary(as.numeric(df2$asia00_datetime_pi))
df2$asia00_datetime_pi_n<- ifelse(as.numeric(df2$asia00_datetime_pi)< 0, 0, as.numeric(df2$asia00_datetime_pi))
df2$asia01_datetime_pi<- difftime(df2$asia01_datetime, as.POSIXct(df2$INJ_moment_n, format="%m/%d/%Y %H"), units = "min")
summary(as.numeric(df2$asia01_datetime_pi))
df2$asia01_datetime_pi_n<-ifelse(as.numeric(df2$asia01_datetime_pi)< 0, NA, as.numeric(df2$asia01_datetime_pi))

#create variable where traction falls in relation to asia score exam
df2$trac_asiatest<- ifelse(df2$tract_mins_post_inj>= df2$asia00_datetime_pi_n & df2$tract_mins_post_inj<= df2$asia01_datetime_pi_n, "00_01",
                           ifelse(df2$tract_mins_post_inj>= df2$asia01_datetime_pi_n, "01_04",
                                  ifelse(df2$tract_mins_post_inj<= df2$asia00_datetime_pi_n, "b400", NA)))

summary(as.factor(df2$trac_asiatest))

#create variable of early vs late traction
df2$tract_EL<- ifelse(df2$tract_mins_post_inj<480, "Early",
                      ifelse(df2$tract_mins_post_inj>=480,"Late", NA))
summary(as.factor(df2$tract_EL))

######
# Analysis 1: check the change score from time-point 00 (pre-surgery) to 
# time-point 01 (first post surgery) and see if there is significant 
# difference in LEMS, UEMS, TMS for patients who had early surgery (<24hrs)
# vs late (>24hrs)
df2$LEMSchange_prepost_tract<- ifelse(df2$trac_asiatest=="00_01", df2$Lower01-df2$Lower00,
                                      ifelse(df2$trac_asiatest=="01_04", df2$Lower04-df2$Lower01, NA))
summary(df2$LEMSchange_prepost_tract)
t.test(df2$LEMSchange_prepost_tract~df2$tract_EL)

df2$UEMSchange_prepost_tract<- ifelse(df2$trac_asiatest=="00_01", df2$Upper01-df2$Upper00,
                                      ifelse(df2$trac_asiatest=="01_04", df2$Upper04-df2$Upper01, NA))
summary(df2$UEMSchange_prepost_tract)
t.test(df2$UEMSchange_prepost_tract~df2$tract_EL)

df2$TMSchange_prepost_tract<- ifelse(df2$trac_asiatest=="00_01", df2$asiatot01-df2$asiatot00,
                                      ifelse(df2$trac_asiatest=="01_04", df2$asiatot04-df2$asiatot01, NA))
summary(df2$TMSchange_prepost_tract)
t.test(df2$TMSchange_prepost_tract~df2$tract_EL)

lm(LEMSchange_prepost_tract~tract_EL+ as.factor(SEXCD) + as.factor(ASIMPC01_A) + AGE + as.factor(SPLVL1_A), data=df2)
lm(UEMSchange_prepost_tract~tract_EL+ as.factor(SEXCD) + as.factor(ASIMPC01_A) + AGE + as.factor(SPLVL1_A), data=df2)
lm(TMSchange_prepost_tract~tract_EL+ as.factor(SEXCD) + as.factor(ASIMPC01_A) + AGE + as.factor(SPLVL1_A), data=df2)

# Analysis 2: Create histograms of LEMS, UEMS, TMS for patients who had 
# early surgery (<24hrs) vs late (>24hrs) pre and post surgery. And then 
# test if these distributions are the same for early surgery (<24hrs) vs 
# late (>24hrs) 
df2$pre_tract_LEMS<- ifelse(df2$trac_asiatest=="00_01", df2$Lower00,
                            ifelse(df2$trac_asiatest=="01_04", df2$Lower01, NA))
df2$post_tract_LEMS<- ifelse(df2$trac_asiatest=="00_01", df2$Lower01,
                            ifelse(df2$trac_asiatest=="01_04", df2$Lower04, NA))
df2$pre_tract_UEMS<- ifelse(df2$trac_asiatest=="00_01", df2$Upper00,
                            ifelse(df2$trac_asiatest=="01_04", df2$Upper01, NA))
df2$post_tract_UEMS<- ifelse(df2$trac_asiatest=="00_01", df2$Upper01,
                            ifelse(df2$trac_asiatest=="01_04", df2$Upper04, NA))
df2$pre_tract_TMS<- ifelse(df2$trac_asiatest=="00_01", df2$asiatot00,
                            ifelse(df2$trac_asiatest=="01_04", df2$asiatot01, NA))
df2$post_tract_TMS<- ifelse(df2$trac_asiatest=="00_01", df2$asiatot01,
                             ifelse(df2$trac_asiatest=="01_04", df2$asiatot04, NA))

library(ggplot2)
lemspre<- ggplot(df2, aes(x=df2$pre_tract_LEMS))+
  geom_histogram() + theme_bw() +facet_grid(~df2$tract_EL)
lemspost<- ggplot(df2, aes(x=df2$post_tract_LEMS))+
  geom_histogram() + theme_bw() +facet_grid(~df2$tract_EL)
uemspre<- ggplot(df2, aes(x=df2$pre_tract_UEMS))+
  geom_histogram() + theme_bw()+facet_grid(~df2$tract_EL)
uemspost<- ggplot(df2, aes(x=df2$post_tract_UEMS))+
  geom_histogram() + theme_bw()+facet_grid(~df2$tract_EL)
totmspre<- ggplot(df2, aes(x=df2$pre_tract_TMS))+
  geom_histogram() + theme_bw()+facet_grid(~df2$tract_EL)
totmspost<- ggplot(df2, aes(x=df2$post_tract_TMS))+
  geom_histogram() + theme_bw()+facet_grid(~df2$tract_EL)

library(devtools)
library(ggpubr)
ggarrange(lemspre, lemspost, 
          uemspre, uemspost,
          totmspre, totmspost,
          ncol=2, nrow=3)

kruskal.test(pre_tract_LEMS ~tract_EL, data=df2)
kruskal.test(post_tract_LEMS ~tract_EL, data=df2)
kruskal.test(pre_tract_UEMS ~tract_EL, data=df2)
kruskal.test(post_tract_UEMS ~tract_EL, data=df2)
kruskal.test(pre_tract_TMS ~tract_EL, data=df2)
kruskal.test(post_tract_TMS ~tract_EL, data=df2)

# Analysis 3: Calculate the change scores from time 00 (pre surgery)
# to 52 weeks and test if there is a difference between early 
# surgery (<24hrs) vs late (>24hrs)
df2$LEMSchange_pre52_tract<- ifelse(df2$trac_asiatest=="00_01", df2$Lower52-df2$Lower00,
                                      ifelse(df2$trac_asiatest=="01_04", df2$Lower52-df2$Lower01, NA))
df2$UEMSchange_pre52_tract<- ifelse(df2$trac_asiatest=="00_01", df2$Upper52-df2$Upper00,
                                    ifelse(df2$trac_asiatest=="01_04", df2$Upper52-df2$Upper01, NA))
df2$TMSchange_pre52_tract<- ifelse(df2$trac_asiatest=="00_01", df2$asiatot52-df2$asiatot00,
                                    ifelse(df2$trac_asiatest=="01_04", df2$asiatot52-df2$asiatot01, NA))

lm(LEMSchange_pre52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="A"))
lm(LEMSchange_pre52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="B"))
lm(LEMSchange_pre52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="C"))
lm(LEMSchange_pre52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="D"))

lm(UEMSchange_pre52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="A"))
lm(UEMSchange_pre52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="B"))
lm(UEMSchange_pre52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="C"))
lm(UEMSchange_pre52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="D"))

lm(TMSchange_pre52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="A"))
lm(TMSchange_pre52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="B"))
lm(TMSchange_pre52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="C"))
lm(TMSchange_pre52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="D"))

# Analysis 4:Calculate the change scores from time 01 (post surgery) 
# to 52 weeks and test if there is a difference between early 
# surgery (<24hrs) vs late (>24hrs)
df2$LEMSchange_post52_tract<- ifelse(df2$trac_asiatest=="00_01", df2$Lower52-df2$Lower01,
                                    ifelse(df2$trac_asiatest=="01_04", df2$Lower52-df2$Lower04, NA))
df2$UEMSchange_post52_tract<- ifelse(df2$trac_asiatest=="00_01", df2$Upper52-df2$Upper01,
                                    ifelse(df2$trac_asiatest=="01_04", df2$Upper52-df2$Upper04, NA))
df2$TMSchange_post52_tract<- ifelse(df2$trac_asiatest=="00_01", df2$asiatot52-df2$asiatot01,
                                   ifelse(df2$trac_asiatest=="01_04", df2$asiatot52-df2$asiatot04, NA))

lm(LEMSchange_post52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="A"))
lm(LEMSchange_post52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="B"))
lm(LEMSchange_post52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="C"))
lm(LEMSchange_post52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="D"))

lm(UEMSchange_post52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="A"))
lm(UEMSchange_post52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="B"))
lm(UEMSchange_post52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="C"))
lm(UEMSchange_post52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="D"))

lm(TMSchange_post52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="A"))
lm(TMSchange_post52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="B"))
lm(TMSchange_post52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="C"))
lm(TMSchange_post52_tract~tract_EL+ as.factor(SEXCD) + AGE + as.factor(SPLVL1_A), 
   data=subset(df2, df2$ASIMPC01_A=="D"))


# Analysis 5: Run lmers over the from 00 (pre surgery) to 52 weeks 
# and test if there is a difference between early surgery (<24hrs) 
# vs late (>24hrs)
library(lme4)
table(df2$tract_EL, df2$ASIMPC01_A)

#subset for ASIA A
df2_A<- subset(df2, ASIMPC01_A =="A")
df2_A$Lower01_dup<- df2_A$Lower01
df2_A$Upper01_dup<- df2_A$Upper01
df2_A$asiatot01_dup<- df2_A$asiatot01

#turn data long
library(tidyr)
df2_A_long<- reshape(df2_A,
                     direction="long",
                     varying = c("Lower00", "Lower01", "Lower04", "Lower08", "Lower16", "Lower26", "Lower52",
                                 "Upper00", "Upper01", "Upper04", "Upper08", "Upper16", "Upper26", "Upper52",
                                 "asiatot00", "asiatot01", "asiatot04", "asiatot08", "asiatot16", "asiatot26", "asiatot52"),
                     timevar="time",
                     times=c("00", "01", "04", "08", "16", "26", "52"),
                     v.names=c("Lower", "Upper", "Total"),
                     idvar="new_PTID")


lmer(Lower~ AGE+as.factor(SEXCD)+as.numeric(time)+ as.numeric(Lower01_dup) +tract_EL+ as.factor(SPLVL1_A)+(1|new_PTID), data=df2_A_long)
lmer(Upper~ AGE+as.factor(SEXCD)+as.numeric(time)+ as.numeric(Upper01_dup) +tract_EL+ as.factor(SPLVL1_A)+(1|new_PTID), data=df2_A_long)
lmer(Total~ AGE+as.factor(SEXCD)+as.numeric(time)+ as.numeric(asiatot01_dup) +tract_EL+ as.factor(SPLVL1_A)+(1|new_PTID), data=df2_A_long)

#subset for ASIA B
df2_B<- subset(df2, ASIMPC01_A =="B")
df2_B$Lower01_dup<- df2_B$Lower01
df2_B$Upper01_dup<- df2_B$Upper01
df2_B$asiatot01_dup<- df2_B$asiatot01

#turn data long
df2_B_long<- reshape(df2_B,
                     direction="long",
                     varying = c("Lower00", "Lower01", "Lower04", "Lower08", "Lower16", "Lower26", "Lower52",
                                 "Upper00", "Upper01", "Upper04", "Upper08", "Upper16", "Upper26", "Upper52",
                                 "asiatot00", "asiatot01", "asiatot04", "asiatot08", "asiatot16", "asiatot26", "asiatot52"),
                     timevar="time",
                     times=c("00", "01", "04", "08", "16", "26", "52"),
                     v.names=c("Lower", "Upper", "Total"),
                     idvar="new_PTID")

lmer(Lower~ AGE+as.factor(SEXCD)+as.numeric(time)+ as.numeric(Lower01_dup) +tract_EL+ as.factor(SPLVL1_A)+(1|new_PTID), data=df2_B_long)
lmer(Upper~ AGE+as.factor(SEXCD)+as.numeric(time)+ as.numeric(Upper01_dup) +tract_EL+ as.factor(SPLVL1_A)+(1|new_PTID), data=df2_B_long)
lmer(Total~ AGE+as.factor(SEXCD)+as.numeric(time)+ as.numeric(asiatot01_dup) +tract_EL+ as.factor(SPLVL1_A)+(1|new_PTID), data=df2_B_long)

#subset for ASIA C
df2_C<- subset(df2, ASIMPC01_A =="C")
df2_C$Lower01_dup<- df2_C$Lower01
df2_C$Upper01_dup<- df2_C$Upper01
df2_C$asiatot01_dup<- df2_C$asiatot01

#turn data long
df2_C_long<- reshape(df2_C,
                     direction="long",
                     varying = c("Lower00", "Lower01", "Lower04", "Lower08", "Lower16", "Lower26", "Lower52",
                                 "Upper00", "Upper01", "Upper04", "Upper08", "Upper16", "Upper26", "Upper52",
                                 "asiatot00", "asiatot01", "asiatot04", "asiatot08", "asiatot16", "asiatot26", "asiatot52"),
                     timevar="time",
                     times=c("00", "01", "04", "08", "16", "26", "52"),
                     v.names=c("Lower", "Upper", "Total"),
                     idvar="new_PTID")

lmer(Lower~ AGE+as.factor(SEXCD)+as.numeric(time)+ as.numeric(Lower01_dup) +tract_EL+ as.factor(SPLVL1_A)+(1|new_PTID), data=df2_C_long)
lmer(Upper~ AGE+as.factor(SEXCD)+as.numeric(time)+ as.numeric(Upper01_dup) +tract_EL+ as.factor(SPLVL1_A)+(1|new_PTID), data=df2_C_long)
lmer(Total~ AGE+as.factor(SEXCD)+as.numeric(time)+ as.numeric(asiatot01_dup) +tract_EL+ as.factor(SPLVL1_A)+(1|new_PTID), data=df2_C_long)

#subset for ASIA D
df2_D<- subset(df2, ASIMPC01_A =="D")
df2_D$Lower01_dup<- df2_D$Lower01
df2_D$Upper01_dup<- df2_D$Upper01
df2_D$asiatot01_dup<- df2_D$asiatot01

#turn data long
df2_D_long<- reshape(df2_D,
                     direction="long",
                     varying = c("Lower00", "Lower01", "Lower04", "Lower08", "Lower16", "Lower26", "Lower52",
                                 "Upper00", "Upper01", "Upper04", "Upper08", "Upper16", "Upper26", "Upper52",
                                 "asiatot00", "asiatot01", "asiatot04", "asiatot08", "asiatot16", "asiatot26", "asiatot52"),
                     timevar="time",
                     times=c("00", "01", "04", "08", "16", "26", "52"),
                     v.names=c("Lower", "Upper", "Total"),
                     idvar="new_PTID")

##**ERROR AS THIS COHORT OF PTS ARE ALL CERVICAL INJURIES**##
lmer(Lower~ AGE+as.factor(SEXCD)+as.numeric(time)+ as.numeric(Lower01_dup) +tract_EL+ as.factor(SPLVL1_A)+(1|new_PTID), data=df2_D_long)
lmer(Upper~ AGE+as.factor(SEXCD)+as.numeric(time)+ as.numeric(Upper01_dup) +tract_EL+ as.factor(SPLVL1_A)+(1|new_PTID), data=df2_D_long)
lmer(Total~ AGE+as.factor(SEXCD)+as.numeric(time)+ as.numeric(asiatot01_dup) +tract_EL+ as.factor(SPLVL1_A)+(1|new_PTID), data=df2_D_long)


