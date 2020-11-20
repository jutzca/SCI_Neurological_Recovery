# ------------------------------------------------------------------------------------------------------------------
# Format EMSCI dataframe extracted with Python
#
# July 2020
# L. Bourguignon
# ------------------------------------------------------------------------------------------------------------------

data_emsci_original = read.csv('/Volumes/borgwardt/Projects/SCI_Neurological_Outcome/App/data/df_emsci.csv')

data_emsci_formatted <- data_emsci_original
data_emsci_formatted$ExamStage <- relevel(data_emsci_formatted$ExamStage, "very acute")

data_emsci_formatted$UEMS <- as.numeric(as.character(data_emsci_formatted$UEMS))
data_emsci_formatted$RUEMS <- as.numeric(as.character(data_emsci_formatted$RUEMS))
data_emsci_formatted$LUEMS <- as.numeric(as.character(data_emsci_formatted$LUEMS))
data_emsci_formatted$LEMS <- as.numeric(as.character(data_emsci_formatted$LEMS))
data_emsci_formatted$RLEMS <- as.numeric(as.character(data_emsci_formatted$RLEMS))
data_emsci_formatted$LLEMS <- as.numeric(as.character(data_emsci_formatted$LLEMS))
data_emsci_formatted$RMS <- as.numeric(as.character(data_emsci_formatted$RMS))
data_emsci_formatted$LMS <- as.numeric(as.character(data_emsci_formatted$LMS))
data_emsci_formatted$TMS <- as.numeric(as.character(data_emsci_formatted$TMS))
data_emsci_formatted$RPP <- as.numeric(as.character(data_emsci_formatted$RPP))
data_emsci_formatted$LPP <- as.numeric(as.character(data_emsci_formatted$LPP))
data_emsci_formatted$TPP <- as.numeric(as.character(data_emsci_formatted$TPP))
data_emsci_formatted$RLT <- as.numeric(as.character(data_emsci_formatted$RLT))
data_emsci_formatted$LLT <- as.numeric(as.character(data_emsci_formatted$LLT))
data_emsci_formatted$TLT <- as.numeric(as.character(data_emsci_formatted$TLT))
#data_emsci_formatted$RSL <- as.numeric(as.character(data_emsci_formatted$RSL))
#data_emsci_formatted$LSL <- as.numeric(as.character(data_emsci_formatted$LSL))
#data_emsci_formatted$RML <- as.numeric(as.character(data_emsci_formatted$RML))
#data_emsci_formatted$LML <- as.numeric(as.character(data_emsci_formatted$LML))
data_emsci_formatted$WISCI <- as.numeric(as.character(data_emsci_formatted$WISCI))
data_emsci_formatted$X6min <- as.numeric(as.character(data_emsci_formatted$X6min))
data_emsci_formatted$X10m <- as.numeric(as.character(data_emsci_formatted$X10m))
data_emsci_formatted$TUG <- as.numeric(as.character(data_emsci_formatted$TUG))
data_emsci_formatted$SCIM2_TotalScore <- as.numeric(as.character(data_emsci_formatted$SCIM2_TotalScore))
data_emsci_formatted$SCIM3_TotalScore <- as.numeric(as.character(data_emsci_formatted$SCIM3_TotalScore))

for (i in 1:dim(data_emsci_formatted)[[1]]){
  data_emsci_formatted[i, 'nb_stage'] = dim(subset(data_emsci_formatted, ExamStage == levels(data_emsci_formatted$ExamStage)[grep(data_emsci_formatted[i, 'ExamStage'], levels(data_emsci_formatted$ExamStage))]))[[1]]
}

data_emsci_formatted$YEARDOI_cat<-cut(data_emsci_formatted$YEARDOI, seq(2000,2020,5), labels=c('2000-2004', '2005-2009', '2010-2014', '2015-2019'))

data_emsci_formatted <- data_emsci_formatted %>% rename(test_6min = `6min`, test_10m = `10m`)
data_emsci_formatted <- data_emsci_formatted %>% rename(SCIM3 = SCIM3_TotalScore, SCIM2 = SCIM2_TotalScore)

write.csv(data_emsci_formatted,'/Volumes/borgwardt/Projects/SCI_Neurological_Outcome/App/data/df_emsci_formatted.csv')

# --------------------------------------------------------------------------------------

cat_age <- function (age){
  if (age < 20){
    cat = '0-19'
  } else if (age < 40 && age >= 20){
    cat = '20-39'
  } else if (age < 60 && age >= 40){
    cat = '40-59'
  } else if (age < 80 && age >= 60){
    cat = '60-79'
  } else if (age < 100 && age >= 80){
    cat = '80-99'
  } else if (age >= 20){
    cat = '100+'
  }
  return (cat)
}

cat_sex <- function (sex){
  if (sex == 2){
    cat = 'Male'
  } else if (sex == 1){
    cat = 'Female'
  }
  return(cat)
}

cat_cause <- function (cause){
  if (cause == 1){
    cat = 'automobile'
  } else if (cause == 2){
    cat = 'motorcycle'
  } else if (cause == 3){
    cat = 'pedestrian'
  } else if (cause == 4){
    cat = 'fall'
  } else if (cause == 5){
    cat = 'water related'
  } else if (cause == 6){
    cat = 'other sports'
  } else if (cause == 7){
    cat = 'blunt trauma'
  } else if (cause == 8){
    cat = 'gun shot wound'
  } else if (cause == 9){
    cat = 'others'
  } else {
    cat <- NA
  }
  return(cat)
}

cat_level <- function (level){
  if (grepl("C", level)){
    cat <- "cervical"
  } else if (grepl("T", level)){
    cat <- "thoracic"
  } else if (grepl("L", level)){
    cat <- "lumbar"
  } else if (grepl("S", level)){
    cat <- "sacral"
  } else {
    cat <- NA
  }
  return (cat)
}

ms_fct <- function(side, j, i){
  end <- paste(side, j, sep='')
  ankdo <- df_original_sygen[paste('ankdo', end, sep='')][[1]][i]
  ankpl <- df_original_sygen[paste('ankpl', end, sep='')][[1]][i]
  elbex <- df_original_sygen[paste('elbex', end, sep='')][[1]][i]
  elbfl <- df_original_sygen[paste('elbfl', end, sep='')][[1]][i]
  finab <- df_original_sygen[paste('finab', end, sep='')][[1]][i]
  finfl <- df_original_sygen[paste('finfl', end, sep='')][[1]][i]
  greto <- df_original_sygen[paste('greto', end, sep='')][[1]][i]
  hipfl <- df_original_sygen[paste('hipfl', end, sep='')][[1]][i]
  if (side == 'l'){
    knee <- df_original_sygen[paste('kneex', end, sep='')][[1]][i]
  } else if (side == 'r'){
    knee <- df_original_sygen[paste('kneet', end, sep='')][[1]][i]
  }
  wrext <- df_original_sygen[paste('wrext', end, sep='')][[1]][i]

  vec <- c(ankdo, ankpl, elbex, elbfl, finab, finfl, greto, hipfl, knee, wrext)
  if (all(is.na(vec))){
    score = NA
  } else {
    def = 0
    for (k in 1:length(vec)[1]){
      if (is.na(vec[k])){
        next
      }
      if (vec[k] == 9){
        def = def-9
      }
    }
    score = sum(c(ankdo, ankpl, elbex, elbfl, finab, finfl, greto, hipfl, knee, wrext, def), na.rm = T)
  }

  return(score)
}


df_original_sygen <- read.csv('/Volumes/borgwardt/Projects/SCI_Neurological_Outcome/Data/Sygen/JohnKramersProject_DATA_2019-10-07_0111.csv', stringsAsFactors = T)

df_sygen_formatted <- data.frame(matrix(ncol = 20, nrow = 0))
col <- c("ID", "Sex", "Age", "AIS", "Cause", "NLI", "Time", "UEMS", "LEMS", "TEMS", "RPP", "LPP", "TPP", "RLT", "LLT", "TLT", "Benzel", "RMS", "LMS", "TMS")
colnames(df_sygen_formatted) <- col

for (i in 1:dim(df_original_sygen)[1]){
  temp <- c(toString(df_original_sygen$ptid[i]),
            cat_sex(df_original_sygen$sexcd[i]),
            cat_age(df_original_sygen$age[i]),
            toString(df_original_sygen$ais1[i]),
            cat_cause(df_original_sygen$injcd[i]),
            cat_level(df_original_sygen$splvl[i]))
  for (j in c('00', '01', '04', '08', '16', '26', '52', '54')){
    time <- paste("Week", j, sep='')
    if (j == '54'){
      UEMS <- NA
      LEMS <- NA
      TEMS <- NA
    } else {
      UEMS <- df_original_sygen[paste('upper', j, sep='')][[1]][i]
      LEMS <- df_original_sygen[paste('lower', j, sep='')][[1]][i]
      TEMS <- df_original_sygen[paste('asiatot', j, sep='')][[1]][i]
    }
    if  (j == '00'){
      RPP <- NA
      LPP <- NA
      TPP <- NA
      RLT <- NA
      LLT <- NA
      TLT <- NA
    } else {
      RPP <- df_original_sygen[paste('totppr', j, sep='')][[1]][i]
      LPP <- df_original_sygen[paste('totppl', j, sep='')][[1]][i]
      TPP <- df_original_sygen[paste('totppr', j, sep='')][[1]][i] + df_original_sygen[paste('totppl', j, sep='')][[1]][i]
      RLT <- df_original_sygen[paste('totltr', j, sep='')][[1]][i]
      LLT <- df_original_sygen[paste('totltl', j, sep='')][[1]][i]
      TLT <- df_original_sygen[paste('totltr', j, sep='')][[1]][i] + df_original_sygen[paste('totltl', j, sep='')][[1]][i]
    }
    
    RMS <- ms_fct('r', j, i)
    LMS <- ms_fct('l', j, i)
    TMS <- RMS+LMS
    
    if (j == '00' || j == '01'){
      Benzel <- NA
    } else {
      Benzel <- df_original_sygen[paste('modben', j, sep='')][[1]][i]
    }
    
    temp2 <- c(UEMS,LEMS,TEMS,RPP, LPP, TPP,RLT, LLT, TLT,Benzel, RMS, LMS, TMS)
    if (all(is.na(temp2))){
      next
    } else {
      df_sygen_formatted[nrow(df_sygen_formatted) + 1, 1:20] <- c(temp, time, temp2)
    }
  }
}

df_sygen_formatted$ID <- as.factor(df_sygen_formatted$ID)
df_sygen_formatted$Time <- as.factor(df_sygen_formatted$Time)
df_sygen_formatted$Sex <- as.factor(df_sygen_formatted$Sex)
df_sygen_formatted$Age <- as.factor(df_sygen_formatted$Age)
df_sygen_formatted$AIS <- as.factor(df_sygen_formatted$AIS)
df_sygen_formatted$Cause <- as.factor(df_sygen_formatted$Cause)
df_sygen_formatted$NLI <- as.factor(df_sygen_formatted$NLI)

for (i in 1:dim(df_sygen_formatted)[[1]]){
  df_sygen_formatted[i, 'nb_stage'] = dim(subset(df_sygen_formatted, Time == levels(df_sygen_formatted$Time)[grep(df_sygen_formatted[i, 'Time'],levels(df_sygen_formatted$Time))]))[[1]]
}


write.csv(df_sygen_formatted,'/Volumes/borgwardt/Projects/SCI_Neurological_Outcome/App/data/df_sygen_formatted_2.csv')

# --------------------------------------------------------------------------------------

cat_age2 <- function (age){
  if (age == 1){
    cat = '12-19'
  } else if (age == 2){
    cat = '20-29'
  } else if (age == 3){
    cat = '30-39'
  } else if (age == 4){
    cat = '40-49'
  } else if (age == 5){
    cat = '50-59'
  } else if (age == 6){
    cat = '60-69'
  } else if (age == 7){
    cat = '70-79'
  } else if (age == 8){
    cat = '80+'
  }
  return (cat)
}

cat_sex2 <- function (sex){
  if (sex == 1){
    cat = 'Male'
  } else if (sex == 2){
    cat = 'Female'
  } else if (sex == 3){
    cat = 'Other, transgender'
  } else if (sex == 9){
    cat = NA
  }
  return(cat)
}

cat_cause2 <- function (cause){
  if (cause == 1){
    cat = 'automobile'
  } else if (cause == 2){
    cat = 'motorcycle'
  } else if (cause == 40){
    cat = 'pedestrian'
  } else if (cause == 30){
    cat = 'fall'
  } else if (cause == 20 || cause == 24 || cause == 29){
    cat = 'water related'
  } else if (cause %in% c(21, 22, 23, 26, 27, 28, 70, 71, 72, 73, 74, 75, 76, 77, 78, 25)){
    cat = 'other sports'
  } else if (cause == 12){
    cat = 'person-to-person contact'
  } else if (cause == 10){
    cat = 'gun shot wound'
  } else if (cause == 99){
    cat = NA
  } else {
    cat <- 'others'
  }
  return(cat)
}

cat_level2 <- function (level){
  if (grepl("C", level)){
    cat <- "cervical"
  } else if (grepl("T", level)){
    cat <- "thoracic"
  } else if (grepl("L", level)){
    cat <- "lumbar"
  } else if (grepl("S", level)){
    cat <- "sacral"
  } else {
    cat <- NA
  }
  return (cat)
}

cat_ais <- function (ais){
  if (ais == 'A'){
    cat = 'AIS A'
  } else if (ais == 'B'){
    cat = 'AIS B'
  } else if (ais == 'C'){
    cat = 'AIS C'
  } else if (ais == 'D'){
    cat = 'AIS D'
  } else if (ais == 'E'){
    cat = 'AIS E'
  } else if (ais == 'U'){
    cat = NA
  } else {
    cat = NA
  }
  return(cat)
}

df_original_rehab <- as.data.frame(fread("/Volumes/borgwardt/Projects/SCI_Neurological_Outcome/Data/SCIRehab/ICPSR_36724 3/DS0001/36724-0001-Data.tsv"))

df_rehab_formatted <- data.frame(matrix(ncol = 14, nrow = 0))
col_rehab <- c("ID", "Sex", "Age", "AIS", "Cause", "NLI", "Time", "RMS", "LMS", "TMS", "PHYIND", "MOBILITY", "OCCUPATION", "SOCIAL")
colnames(df_rehab_formatted) <- col_rehab

# "AASASTRR" right ASIA motor index score at rehab admission
# "AASASTRL" left ASIA motor index score at rehab admission
# "AASASTDR" right ASIA motor index score at discharge
# "AASASTDL" left ASIA motor index score at discharge


for (i in 1:dim(df_original_rehab)[1]){
  temp <- c(toString(df_original_rehab$NEWID[i]),
            cat_sex2(df_original_rehab$ASEX[i]),
            cat_age2(df_original_rehab$AINJAGE[i]),
            cat_ais(df_original_rehab$AASAIMRB[i]),
            cat_cause2(df_original_rehab$ATRMETIO[i]),
            cat_level2(df_original_rehab$ANURLVLA[i]))
  for (j in c('admission', 'discharge')){
    time <- j
    if (j == 'admission'){
      RMS <- df_original_rehab$AASASTRR[i]
      LMS <-df_original_rehab$AASASTRL[i]
      PHYIND <- NA
      MOBILITY <- NA
      OCCUPATION <- NA
      SOCIAL <- NA
    } else if (j == 'discharge'){
      RMS <- df_original_rehab$AASASTDR[i]
      LMS <- df_original_rehab$AASASTDL[i]
      PHYIND <- df_original_rehab$BCHPITOT[i]
      MOBILITY <- df_original_rehab$BCHMBTOT[i]
      OCCUPATION <- df_original_rehab$BCHOPTOT[i]
      SOCIAL <- df_original_rehab$BCHSOCIN[i]
    }
    TMS <- RMS+LMS
    temp2 <- c(RMS, LMS, TMS, PHYIND, MOBILITY, OCCUPATION, SOCIAL)
    if (all(is.na(temp2))){
      next
    } else {
      df_rehab_formatted[nrow(df_rehab_formatted) + 1, 1:14] <- c(temp, time, temp2)
    }
  }
}

df_rehab_formatted$ID <- as.factor(df_rehab_formatted$ID)
df_rehab_formatted$Time <- as.factor(df_rehab_formatted$Time)
df_rehab_formatted$Sex <- as.factor(df_rehab_formatted$Sex)
df_rehab_formatted$Age <- as.factor(df_rehab_formatted$Age)
df_rehab_formatted$AIS <- as.factor(df_rehab_formatted$AIS)
df_rehab_formatted$Cause <- as.factor(df_rehab_formatted$Cause)
df_rehab_formatted$NLI <- as.factor(df_rehab_formatted$NLI)

for (i in 1:dim(df_rehab_formatted)[[1]]){
  df_rehab_formatted[i, 'nb_stage'] = dim(subset(df_rehab_formatted, Time == levels(df_rehab_formatted$Time)[grep(df_rehab_formatted[i, 'Time'],levels(df_rehab_formatted$Time))]))[[1]]
}

write.csv(df_rehab_formatted,'/Volumes/borgwardt/Projects/SCI_Neurological_Outcome/App/data/df_rehab_formatted.csv')


# --------------------------------------------------------------------------------------



cat_age3 <- function (age){
  if (age == '12-19'){
    cat = '0-19'
  } else if (age == '20-29' || age == '30-39'){
    cat = '20-39'
  } else if (age == '40-49' || age == '50-59'){
    cat = '40-59'
  } else if (age == '60-69' || age == '70-79'){
    cat = '60-79'
  } else if (age == '80+'){
    cat = '80+'
  } 
  return (cat)
}

cat_age4 <- function (age){
  if (is.na(age)){
    cat = NA
  } else if (age == 0){
    cat = '0-19'
  } else if (age == 1){
    cat = '20-39'
  } else if (age == 2){
    cat = '40-59'
  } else if (age == 3){
    cat = '60-79'
  } else if (age == 4){
    cat = '80+'
  }
  return (cat)
}

df_all_formatted <- data.frame(matrix(ncol = 9, nrow = 0))
col_all <- c("ID", "Sex", "Age", "AIS", "NLI", "RMS", "LMS", "TMS", "Dataset")
colnames(df_all_formatted) <- col_all

vars_emsci <- names(data_emsci_formatted) %in% c('Patientennummer', 'Sex', 'age_category', 'AIS', 'NLI_level', 'RMS', 'LMS', 'TMS')
sub_emsci <- data_emsci_formatted[vars_emsci]
vars_sygen <- names(df_sygen_formatted) %in% c('ID', 'Sex', 'Age', 'AIS', 'NLI', 'RMS', 'LMS', 'TMS')
sub_sygen <- df_sygen_formatted[vars_sygen]
vars_rehab <- names(df_rehab_formatted) %in% c('ID', 'Sex', 'Age', 'AIS', 'NLI', 'RMS', 'LMS', 'TMS')
sub_rehab <- df_rehab_formatted[vars_rehab]

for (i in 1:dim(sub_rehab)[1]){
  sub_rehab[i, 'Age2'] <- cat_age3(sub_rehab[i, 'Age'])
}

for (i in 1:dim(sub_emsci)[1]){
  sub_emsci[i, 'Age'] <- cat_age4(sub_emsci[i, 'age_category'])
}

sub_emsci_2 <- sub_emsci[, c(1, 2, 9, 3, 4, 5, 6, 7)]
sub_rehab_2 <- sub_rehab[, c(1, 2, 9, 4, 5, 6, 7, 8)]


for (i in 1:dim(sub_emsci_2)[1]){
  sub_emsci_2[i, 'Dataset'] <- 'EMSCI'
}
for (i in 1:dim(sub_rehab_2)[1]){
  sub_rehab_2[i, 'Dataset'] <- 'SCI rehab'
}
for (i in 1:dim(sub_sygen)[1]){
  sub_sygen[i, 'Dataset'] <- 'Sygen'
}

sub_emsci_2 <- sub_emsci_2 %>% rename(ID = Patientennummer, NLI = NLI_level)
sub_rehab_2 <- sub_rehab_2 %>% rename(Age = Age2)

df_all_formatted <- rbind(df_all_formatted, sub_emsci_2)
df_all_formatted <- rbind(df_all_formatted, sub_sygen)
df_all_formatted <- rbind(df_all_formatted, sub_rehab_2)

df_all_formatted$Dataset <- as.factor(df_all_formatted$Dataset)

for (i in 1:dim(df_all_formatted)[[1]]){
  df_all_formatted[i, 'nb_stage'] = dim(subset(df_all_formatted, Dataset == levels(df_all_formatted$Dataset)[grep(df_all_formatted[i, 'Dataset'],levels(df_all_formatted$Dataset))]))[[1]]
}

df_all_formatted$RMS <- as.numeric(as.character(df_all_formatted$RMS))
df_all_formatted$LMS <- as.numeric(as.character(df_all_formatted$LMS))
df_all_formatted$TMS <- as.numeric(as.character(df_all_formatted$TMS))

write.csv(df_all_formatted,'/Volumes/borgwardt/Projects/SCI_Neurological_Outcome/App/data/df_all_formatted.csv')

# --------------------------------------------------------------------------------------

cat_level3 <- function (level){
  if (grepl("X", level)){
    cat <- NA
  } else if (level == ''){
    cat <- NA
  } else {
    cat <- level
  }
  return (cat)
}

cat_plegia <- function (level){
  if (grepl("C", level)){
    cat <- "tetra"
  } else if (level == 'T01'){
    cat <- "tetra"
  } else if (grepl("X", level)){
    cat <- NA
  } else if (level == ''){
    cat <- NA
  } else {
    cat <- "para"
  }
  return (cat)
}

data_rehab_formatted = read.csv('/Volumes/bs-dfs/Projects/SCI_Neurological_Outcome/App/data/df_rehab_formatted.csv')
df_original_rehab <- as.data.frame(fread("/Volumes/bs-dfs/Projects/SCI_Neurological_Outcome/Data/SCIRehab/ICPSR_36724 3/DS0001/36724-0001-Data.tsv"))

df_rehab_epi <- data.frame(matrix(ncol = 7, nrow = 0))
col_rehab <- c("ID", "Sex", "Age", "AIS", 'NLI', 'plegia', 'YEARDOI')
colnames(df_rehab_epi) <- col_rehab

# "AASASTRR" right ASIA motor index score at rehab admission
# "AASASTRL" left ASIA motor index score at rehab admission
# "AASASTDR" right ASIA motor index score at discharge
# "AASASTDL" left ASIA motor index score at discharge


for (i in 1:dim(df_original_rehab)[1]){
  temp <- c(toString(df_original_rehab$NEWID[i]),
            cat_sex2(df_original_rehab$ASEX[i]),
            cat_age2(df_original_rehab$AINJAGE[i]),
            cat_ais(df_original_rehab$AASAIMRB[i]),
            cat_level3(df_original_rehab$ANURLVLA[i]),
            cat_plegia(df_original_rehab$ANURLVLA[i]),
            df_original_rehab$AINJYEARORIG[i])
    if (all(is.na(temp))){
      next
    } else {
      df_rehab_epi[nrow(df_rehab_epi) + 1, 1:7] <- c(temp)
    }
}


df_rehab_epi$ID <- as.factor(df_rehab_epi$ID)
df_rehab_epi$Sex <- as.factor(df_rehab_epi$Sex)
df_rehab_epi$Age <- as.factor(df_rehab_epi$Age)
df_rehab_epi$AIS <- as.factor(df_rehab_epi$AIS)
df_rehab_epi$NLI <- as.factor(df_rehab_epi$NLI)
df_rehab_epi$plegia <- as.factor(df_rehab_epi$plegia)
#df_rehab_epi$YEARDOI <- as.numeric(df_rehab_epi$YEARDOI)

write.csv(df_rehab_epi,'/Volumes/bs-dfs/Projects/SCI_Neurological_Outcome/App/data/df_rehab_epi.csv')
