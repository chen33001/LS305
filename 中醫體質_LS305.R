library(openxlsx)
library(tidyverse)


data <- read.csv("C:\\R\\LS305¤¤Âå\\TCM_list20220924.csv",fileEncoding = "Big5")
head(data)

#¤ÀÂ÷¸ê®Æ
age<-c(data$age_gruop)
sex<-c(data$SEX)
yin<-c(data$³±µêÅé½è)
yang<-c(data$¶§µêÅé½è)
TCM<-c(data$·ð·ïÅé½è)
peace<-c(data$¥­©MÅé½è)
con<-c(data$Åé½è)
#¨k¤kÅé½è
newdata1<-data[,c(2,48)]
newdata1$Åé½è<-factor(newdata1$Åé½è)

library(ggplot2)
#Ã¸¹Ï
par(mfrow = c(1,1),xpd=NA,oma= c(0,0,0,8))
barplot(table(con,sex),main='¨k¤kÅé½è¤H¼Æ',beside=T,col=rainbow(nlevels(newdata1$Åé½è)))
legend(par("usr")[2],par("usr")[4],legend = as.character(levels(newdata1$Åé½è)),fill=rainbow(nlevels(newdata1$Åé½è)))

table(con,sex)

#¦~ÄÖÅé½è
newdata2<-data[,c(49,48)]
newdata2$Åé½è<-factor(newdata2$Åé½è)

par(mfrow = c(1,1),xpd=NA,oma= c(0,0,0,8))
barplot(table(con,age),main='¦U¦~ÄÖÅé½è¤H¼Æ',beside=T,col=rainbow(nlevels(newdata2$Åé½è)))
legend(par("usr")[2],par("usr")[4],legend = as.character(levels(newdata2$Åé½è)),fill=rainbow(nlevels(newdata2$Åé½è)))
table(con,age)

data2<- read.csv(¨k¤kÅé½è¤H¼Æ,fiieEncoding = "Big5")
sex2<- c(data2$¤k©Ê)
sex3<- c(data2$¨k©Ê)
t.test(sex2,sex3, var.equal=true)

#¦~ÄÖ¤À¥¬
newdata3<-data[,c(2,49)]
newdata3$¦~ÄÖ°Ï¶¡<-factor(newdata3$age_gruop)

par(mfrow = c(1,1),xpd=NA,oma= c(0,0,0,8))
barplot(table(age,sex),main='¨k¤k¦~ÄÖ¤À¥¬',beside=T,col=rainbow(nlevels(newdata3$¦~ÄÖ°Ï¶¡)))
legend(par("usr")[2],par("usr")[4],legend = as.character(levels(newdata3$¦~ÄÖ°Ï¶¡)),fill=rainbow(nlevels(newdata3$¦~ÄÖ°Ï¶¡)))
table(age,sex)

#?Œ¯?‡ºè³‡æ??
aaa <- table(con,sex)
bbb <- table(age,sex)
write.csv(aaa, file = "C:\\R\\LS305ä¸­é†«\\¦UÅé½è¨k¤k¤H¼Æ.csv",fileEncoding = "Big5")
write.csv(bbb, file = "C:\\R\\LS305ä¸­é†«\\¦U¦~ÄÖÅé½è¤H.csv",fileEncoding = "Big5")

#anova
data2 <- read.csv("C:\\R\\¦UÅé½è¨k¤k¤H¼Æ.csv",fileEncoding = "Big5")
sex2 <- c(data2$¤k©Ê)
sex3 <- c(data2$¨k©Ê)
t.test(sex2,sex3, var.equal=TRUE)

data3 <- read.csv("C:\\R\\¦U¦~ÄÖÅé½è¤H¼Æ.csv",fileEncoding = "Big5")
sex2 <- c(data3$¤k©Ê)
sex3 <- c(data3$¨k©Ê)
t.test(sex2,sex3, var.equal=TRUE)



#¥d¤è
 #¦~ÄÖÅé½è¤À¥¬
chisq.test(data4[,2:8])
 #Åé½è¡B¨k¤k
chisq.test(data2[,2:3])

#release_list_measure
measure <- read.csv("C:\\R\\LS305¤¤Âå\\release_list_measure.csv",sep=",", header=TRUE,na = "NA")

#¨úÅéÀË¨ú¯S©wÄæ¦ì
measure_extract<-subset(measure,
                        select = c("Release_No","FOLLOW",
                                   "BODY_HEIGHT","BODY_WEIGHT","BMI","BODY_FAT_RATE","BODY_WAISTLINE","BODY_BUTTOCKS","WHR",
                                   "SIT_1_SYSTOLIC_PRESSURE","SIT_1_DIASTOLIC_PRESSURE",
                                   "SIT_2_SYSTOLIC_PRESSURE","SIT_2_DIASTOLIC_PRESSURE",
                                   "SIT_3_SYSTOLIC_PRESSURE","SIT_3_DIASTOLIC_PRESSURE",
                                   "SIT_1_HEARTBEAT_SPEED","SIT_2_HEARTBEAT_SPEED","SIT_3_HEARTBEAT_SPEED",
                                   "BONE_EXAM_RESULT","T_SCORE","Z_SCORE",
                                   "VC","TV","ERV","IRV","IC","VC_HT","FVC","FVC_PRED",
                                   "FEV10","FEV10_PRED","FEV10_FVC","FEV10_FVC_PRED","FEV10_SVC",
                                   "RBC","WBC","PLATELET","HB","HCT","HBA1C",
                                   "ANTI_HCV_AB_1","ANTI_HCV_AB_2","HBSAG_1","HBSAG_2","HBEAG_1","HBEAG_2",
                                   "ANTI_HBS_AB_1","ANTI_HBS_AB_2","ANTI_HBC_AB_1","ANTI_HBC_AB_2","ANTI_HDV_AB_1","ANTI_HDV_AB_2",
                                   "FASTING_GLUCOSE","T_CHO","TG","HDL_C","LDL_C",
                                   "T_BILIRUBIN","ALBUMIN","SGOT","SGPT","GAMMA_GT","AFP",
                                   "BUN","CREATININE","URIC_ACID","MICROALB","CREATININE_URINE")
                        )

#±qmeasure_extract¨ú¥X¤¤ÂåÅé½èÀÉ®×¦³ªºRelease_No
 #TCM_list20220924­n¥ý¥t¦s¦¨csv
TCMlist<- read.csv("C:\\R\\LS305¤¤Âå\\TCM_list20220924.csv",fileEncoding = "Big5")
TCMlist<-subset(TCMlist,select=c("Release_No","Åé½è","SEX","AGE","age_gruop"))

TCMmerge<-merge(TCMlist,measure_extract, by = "Release_No", all.TCMlist = T)

#TCMmerge¦X¨ÖTWB1,2§Ç¸¹
TWB12 <- read.csv("c:\\R\\LS305¤¤Âå\\lab_info.csv",fileEncoding = "Big5")
names(TWB12)[1] <- "Release_No"
TWB12 <- subset(TWB12,select=c("Release_No","TWB1_ID","TWB2_ID"))
TCMmerge2<-merge(TWB12,TCMmerge, by = "Release_No", all.TCMmerge = T)

#¥Î¦h­«ÅÜ¶µ¥h°£­«½Æ¦C
library(dplyr)

TCMmerge3 <- distinct(TCMmerge2, BMI,BODY_FAT_RATE,BODY_WAISTLINE,BODY_BUTTOCKS,
                      WHR,SIT_1_SYSTOLIC_PRESSURE,SIT_1_DIASTOLIC_PRESSURE,
                      SIT_2_SYSTOLIC_PRESSURE,VC,TV,ERV,
                      .keep_all = TRUE )

#¿é¥Xtwb§Ç¸¹+Åé½èªºªí®æ
write.csv(TCMmerge3,file='C:\\R\\LS305¤¤Âå\\TCMmerge3.csv',fileEncoding = "Big5",append = FALSE)


#­pºâtwb12ªºÅé½è¼Æ¶q---------------------
TCMcal <- read.csv("C:\\R\\LS305¤¤Âå\\TCMmerge3.csv",fileEncoding = "Big5")

 #TWB1
 #¥u¤À¥Xbaseline(¤¤ÂåÅé½èªº­Ó¼Æ)
  TCMcal1 <- subset(TCMcal,
                   select=c("Release_No","TWB1_ID","Åé½è"),
                   FOLLOW=="Baseline")
  #TWB1ªÅÅé½è¼Æ¶q
  TWBcal1 <- print(table(TCMcal1$TWB1_ID,TCMcal1$Åé½è)
                   )
  


 #TWB2
 #¥u¤À¥Xbaseline(¤¤ÂåÅé½èªº­Ó¼Æ)
  TCMcal2 <- subset(TCMcal,
                    select=c("Release_No","TWB2_ID","Åé½è"),
                    FOLLOW=="Baseline")
 #TWB2ªÅÅé½è¼Æ¶q
  TWBcal2 <- print(table(TCMcal$TWB1_ID,TCMcal$Åé½è))

 
#Åé½è¸òÀË´úµ²ªG¥d¤è--------------------
  TCMcal <- read.csv("C:\\R\\LS305¤¤Âå\\TCMmerge3.csv",fileEncoding = "Big5")
 #¥u¨úbaseline
  chiqTCM <- subset(TCMcal,
                    FOLLOW=="Baseline")
  #´À´«¤å¦r¦¨¼Æ¦r
  #¤¤ÂåÅé½è
  chiqTCM$Åé½è <- as.character(chiqTCM$Åé½è)
  
  chiqTCM$Åé½è[which(chiqTCM$Åé½è=="¥­©M")] <- "1"
  chiqTCM$Åé½è[which(chiqTCM$Åé½è=="³±µê")] <- "2"
  chiqTCM$Åé½è[which(chiqTCM$Åé½è=="³±µê+¶§µê")] <- "3"
  chiqTCM$Åé½è[which(chiqTCM$Åé½è=="³±µê+¶§µê+·ð·ï")] <- "4"
  chiqTCM$Åé½è[which(chiqTCM$Åé½è=="³±µê+·ð·ï")] <- "5"
  chiqTCM$Åé½è[which(chiqTCM$Åé½è=="¶§µê")] <- "6"
  chiqTCM$Åé½è[which(chiqTCM$Åé½è=="·ð·ï")] <- "7"
  chiqTCM$Åé½è[which(chiqTCM$Åé½è=="·ð·ï+¶§µê")] <- "8"
  chiqTCM$Åé½è <- as.numeric(chiqTCM$Åé½è)
  
  #ANTI_HCV_AB_1
  chiqTCM$ANTI_HCV_AB_1 <- as.character(chiqTCM$ANTI_HCV_AB_1)
  chiqTCM$ANTI_HCV_AB_1[which(chiqTCM$ANTI_HCV_AB_1=="Positive")] <- "1"
  chiqTCM$ANTI_HCV_AB_1[which(chiqTCM$ANTI_HCV_AB_1=="Negative")] <- "2"
  
  #HBSAG_1
  chiqTCM$HBSAG_1 <- as.character(chiqTCM$HBSAG_1)
  chiqTCM$HBSAG_1[which(chiqTCM$HBSAG_1=="Positive")] <- "1"
  chiqTCM$HBSAG_1[which(chiqTCM$HBSAG_1=="Negative")] <- "2"
  
  #HBEAG_1
  chiqTCM$HBEAG_1 <- as.character(chiqTCM$HBEAG_1)
  chiqTCM$HBEAG_1[which(chiqTCM$HBEAG_1=="Positive")] <- "1"
  chiqTCM$HBEAG_1[which(chiqTCM$HBEAG_1=="Negative")] <- "2"
  
  #ANTI_HBS_AB_1
  chiqTCM$ANTI_HBS_AB_1 <- as.character(chiqTCM$ANTI_HBS_AB_1)
  chiqTCM$ANTI_HBS_AB_1[which(chiqTCM$ANTI_HBS_AB_1=="Positive")] <- "1"
  chiqTCM$ANTI_HBS_AB_1[which(chiqTCM$ANTI_HBS_AB_1=="Negative")] <- "2"
  
  #ANTI_HBC_AB_1
  chiqTCM$ANTI_HBC_AB_1 <- as.character(chiqTCM$ANTI_HBC_AB_1)
  chiqTCM$ANTI_HBC_AB_1[which(chiqTCM$ANTI_HBC_AB_1=="Positive")] <- "1"
  chiqTCM$ANTI_HBC_AB_1[which(chiqTCM$ANTI_HBC_AB_1=="Negative")] <- "2"
  
  #ANTI_HDV_AB_1
  chiqTCM$ANTI_HDV_AB_1 <- as.character(chiqTCM$ANTI_HDV_AB_1)
  chiqTCM$ANTI_HDV_AB_1[which(chiqTCM$ANTI_HDV_AB_1=="Positive")] <- "1"
  chiqTCM$ANTI_HDV_AB_1[which(chiqTCM$ANTI_HDV_AB_1=="Negative")] <- "2"
  
  write.csv(chiqTCM,file='C:\\R\\LS305¤¤Âå\\chiqTCM.csv',fileEncoding = "Big5")
  
  #¥d¤è(¨C¬q10­Ó)


chisq.test(chiqTCM$Åé½è,chiqTCM$BODY_HEIGHT)
chisq.test(chiqTCM$Åé½è,chiqTCM$BODY_WEIGHT)
chisq.test(chiqTCM$Åé½è,chiqTCM$BMI)
chisq.test(chiqTCM$Åé½è,chiqTCM$BODY_FAT_RATE)
chisq.test(chiqTCM$Åé½è,chiqTCM$BODY_WAISTLINE)
chisq.test(chiqTCM$Åé½è,chiqTCM$BODY_BUTTOCKS)
chisq.test(chiqTCM$Åé½è,chiqTCM$WHR)
chisq.test(chiqTCM$Åé½è,chiqTCM$SIT_1_SYSTOLIC_PRESSURE)
chisq.test(chiqTCM$Åé½è,chiqTCM$SIT_1_DIASTOLIC_PRESSURE)
chisq.test(chiqTCM$Åé½è,chiqTCM$SIT_2_SYSTOLIC_PRESSURE)

chisq.test(chiqTCM$Åé½è,chiqTCM$SIT_2_DIASTOLIC_PRESSURE)
chisq.test(chiqTCM$Åé½è,chiqTCM$SIT_3_SYSTOLIC_PRESSURE)
chisq.test(chiqTCM$Åé½è,chiqTCM$SIT_3_DIASTOLIC_PRESSURE)
chisq.test(chiqTCM$Åé½è,chiqTCM$SIT_1_HEARTBEAT_SPEED)
chisq.test(chiqTCM$Åé½è,chiqTCM$SIT_2_HEARTBEAT_SPEED)
chisq.test(chiqTCM$Åé½è,chiqTCM$SIT_3_HEARTBEAT_SPEED)
chisq.test(chiqTCM$Åé½è,chiqTCM$BONE_EXAM_RESULT)
chisq.test(chiqTCM$Åé½è,chiqTCM$T_SCORE)
chisq.test(chiqTCM$Åé½è,chiqTCM$Z_SCORE)
chisq.test(chiqTCM$Åé½è,chiqTCM$VC)

chisq.test(chiqTCM$Åé½è,chiqTCM$TV)
chisq.test(chiqTCM$Åé½è,chiqTCM$ERV)
chisq.test(chiqTCM$Åé½è,chiqTCM$IRV)
chisq.test(chiqTCM$Åé½è,chiqTCM$IC)
chisq.test(chiqTCM$Åé½è,chiqTCM$VC_HT)
chisq.test(chiqTCM$Åé½è,chiqTCM$FVC)
chisq.test(chiqTCM$Åé½è,chiqTCM$FVC_PRED)
chisq.test(chiqTCM$Åé½è,chiqTCM$FEV10)
chisq.test(chiqTCM$Åé½è,chiqTCM$FEV10_PRED)
chisq.test(chiqTCM$Åé½è,chiqTCM$FEV10_FVC)

chisq.test(chiqTCM$Åé½è,chiqTCM$FEV10_FVC_PRED)
chisq.test(chiqTCM$Åé½è,chiqTCM$FEV10_SVC)
chisq.test(chiqTCM$Åé½è,chiqTCM$RBC)
chisq.test(chiqTCM$Åé½è,chiqTCM$WBC)
chisq.test(chiqTCM$Åé½è,chiqTCM$PLATELET)
chisq.test(chiqTCM$Åé½è,chiqTCM$HB)
chisq.test(chiqTCM$Åé½è,chiqTCM$HCT)
chisq.test(chiqTCM$Åé½è,chiqTCM$HBA1C)
chisq.test(chiqTCM$Åé½è,chiqTCM$ANTI_HCV_AB_1)
chisq.test(chiqTCM$Åé½è,chiqTCM$ANTI_HCV_AB_2)

chisq.test(chiqTCM$Åé½è,chiqTCM$HBSAG_1)
chisq.test(chiqTCM$Åé½è,chiqTCM$HBSAG_2)
chisq.test(chiqTCM$Åé½è,chiqTCM$HBEAG_1)
chisq.test(chiqTCM$Åé½è,chiqTCM$HBEAG_2)
chisq.test(chiqTCM$Åé½è,chiqTCM$ANTI_HBS_AB_1)
chisq.test(chiqTCM$Åé½è,chiqTCM$ANTI_HBS_AB_2)
chisq.test(chiqTCM$Åé½è,chiqTCM$ANTI_HBC_AB_1)
chisq.test(chiqTCM$Åé½è,chiqTCM$ANTI_HBC_AB_2)
chisq.test(chiqTCM$Åé½è,chiqTCM$ANTI_HDV_AB_1)
chisq.test(chiqTCM$Åé½è,chiqTCM$ANTI_HDV_AB_2)

chisq.test(chiqTCM$Åé½è,chiqTCM$FASTING_GLUCOSE)
chisq.test(chiqTCM$Åé½è,chiqTCM$T_CHO)
chisq.test(chiqTCM$Åé½è,chiqTCM$TG)
chisq.test(chiqTCM$Åé½è,chiqTCM$HDL_C)
chisq.test(chiqTCM$Åé½è,chiqTCM$LDL_C)
chisq.test(chiqTCM$Åé½è,chiqTCM$T_BILIRUBIN)
chisq.test(chiqTCM$Åé½è,chiqTCM$ALBUMIN)
chisq.test(chiqTCM$Åé½è,chiqTCM$SGOT)
chisq.test(chiqTCM$Åé½è,chiqTCM$SGPT)
chisq.test(chiqTCM$Åé½è,chiqTCM$GAMMA_GT)

chisq.test(chiqTCM$Åé½è,chiqTCM$AFP)
chisq.test(chiqTCM$Åé½è,chiqTCM$BUN)
chisq.test(chiqTCM$Åé½è,chiqTCM$CREATININE)
chisq.test(chiqTCM$Åé½è,chiqTCM$URIC_ACID)
chisq.test(chiqTCM$Åé½è,chiqTCM$MICROALB)
chisq.test(chiqTCM$Åé½è,chiqTCM$CREATININE_URINE)


zzz1 <- chiqTCM[,c("Åé½è")]
zzz2 <- chiqTCM$Åé½è
table(zzz1 == zzz2)

dput(names(chiqTCM))

names(chiqTCM)[1:3]

use_vb <- c("BODY_HEIGHT","BODY_WEIGHT","BMI")

c("X", "Release_No", "TWB1_ID", "TWB2_ID", "Åé½è", "SEX", "AGE", 
  "age_gruop", "FOLLOW", "BODY_HEIGHT", "BODY_WEIGHT", "BMI", "BODY_FAT_RATE", 
  "BODY_WAISTLINE", "BODY_BUTTOCKS", "WHR", "SIT_1_SYSTOLIC_PRESSURE", 
  "SIT_1_DIASTOLIC_PRESSURE", "SIT_2_SYSTOLIC_PRESSURE", "SIT_2_DIASTOLIC_PRESSURE", 
  "SIT_3_SYSTOLIC_PRESSURE", "SIT_3_DIASTOLIC_PRESSURE", "SIT_1_HEARTBEAT_SPEED", 
  "SIT_2_HEARTBEAT_SPEED", "SIT_3_HEARTBEAT_SPEED", "BONE_EXAM_RESULT", 
  "T_SCORE", "Z_SCORE", "VC", "TV", "ERV", "IRV", "IC", "VC_HT", 
  "FVC", "FVC_PRED", "FEV10", "FEV10_PRED", "FEV10_FVC", "FEV10_FVC_PRED", 
  "FEV10_SVC", "RBC", "WBC", "PLATELET", "HB", "HCT", "HBA1C", 
  "ANTI_HCV_AB_1", "ANTI_HCV_AB_2", "HBSAG_1", "HBSAG_2", "HBEAG_1", 
  "HBEAG_2", "ANTI_HBS_AB_1", "ANTI_HBS_AB_2", "ANTI_HBC_AB_1", 
  "ANTI_HBC_AB_2", "ANTI_HDV_AB_1", "ANTI_HDV_AB_2", "FASTING_GLUCOSE", 
  "T_CHO", "TG", "HDL_C", "LDL_C", "T_BILIRUBIN", "ALBUMIN", "SGOT", 
  "SGPT", "GAMMA_GT", "AFP", "BUN", "CREATININE", "URIC_ACID", 
  "MICROALB", "CREATININE_URINE")

# zzz3 <- chisq.test(chiqTCM[,c("Åé½è")],chiqTCM[,c("BODY_WEIGHT")])
# zzz3$p.value


xxx1 <- chiqTCM

result_set <- data.frame(vb1 = NA, vb2 = NA, p_value = NA)

for (i in 1:length(use_vb)) {
  result_set[i,1] <- "Åé½è"
  result_set[i,2] <- use_vb[i]
  result_set[i,3] <- chisq.test(xxx1[,c("Åé½è")],xxx1[,use_vb[i]])$p.value
}