#release_list_measure
measure <- read.csv("C:\\R\\release_list_measure.csv",sep="\t", header=TRUE,na = "NA")

#取體檢取特定欄位
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

#從measure_extract取出中醫體質檔案有的Release_No
 #TCM_list20220924要先另存成csv
TCMlist<- read.csv("C:\\R\\TCM_list20220924.csv",fileEncoding = "Big5")
TCMlist<-subset(TCMlist,select=c("Release_No","體質","SEX","AGE","age_gruop"))

TCMmerge<-merge(TCMlist,measure_extract, by = "Release_No", all.TCMlist = T)

#TCMmerge合併TWB1,2序號
TWB12 <- read.csv("c:\\R\\lab_info.csv", header=TRUE,na = "NA",fileEncoding = "Big5")
names(TWB12)[1] <- "Release_No"
TWB12 <- subset(TWB12,select=c("Release_No","TWB1_ID","TWB2_ID"))
TCMmerge2<-merge(TWB12,TCMmerge, by = "Release_No", all.TCMmerge = T)

#用多重變項去除重複列
library(dplyr)

TCMmerge3 <- distinct(TCMmerge2, BMI,BODY_FAT_RATE,BODY_WAISTLINE,BODY_BUTTOCKS,
                      WHR,SIT_1_SYSTOLIC_PRESSURE,SIT_1_DIASTOLIC_PRESSURE,
                      SIT_2_SYSTOLIC_PRESSURE,VC,TV,ERV,
                      .keep_all = TRUE )

#輸出twb序號+體質的表格
write.csv(TCMmerge3,file='C:\\R\\TCMmerge3.csv',fileEncoding = "Big5",append = FALSE)


#計算twb12的體質數量---------------------
TCMcal <- read.csv("C:\\R\\TCMmerge3.csv",fileEncoding = "Big5")

 #TWB1
 #只分出baseline(中醫體質的個數)
  TCMcal1 <- subset(TCMcal,
                   select=c("Release_No","TWB1_ID","體質"),
                   FOLLOW=="Baseline")
  #TWB1空體質數量
  TWBcal1 <- print(table(TCMcal1$TWB1_ID,TCMcal1$體質)
                   )
  


 #TWB2
 #只分出baseline(中醫體質的個數)
  TCMcal2 <- subset(TCMcal,
                    select=c("Release_No","TWB2_ID","體質"),
                    FOLLOW=="Baseline")
 #TWB2空體質數量
  TWBcal2 <- print(table(TCMcal$TWB1_ID,TCMcal$體質))

 
#體質跟檢測結果卡方--------------------
  TCMcal <- read.csv("C:\\R\\TCMmerge3.csv",fileEncoding = "Big5")
 #只取baseline
  chiqTCM <- subset(TCMcal,
                    FOLLOW=="Baseline")
  #替換文字成數字
  #中醫體質
  chiqTCM$體質 <- as.character(chiqTCM$體質)
  
  chiqTCM$體質[which(chiqTCM$體質=="平和")] <- "1"
  chiqTCM$體質[which(chiqTCM$體質=="陰虛")] <- "2"
  chiqTCM$體質[which(chiqTCM$體質=="陰虛+陽虛")] <- "3"
  chiqTCM$體質[which(chiqTCM$體質=="陰虛+陽虛+痰瘀")] <- "4"
  chiqTCM$體質[which(chiqTCM$體質=="陰虛+痰瘀")] <- "5"
  chiqTCM$體質[which(chiqTCM$體質=="陽虛")] <- "6"
  chiqTCM$體質[which(chiqTCM$體質=="痰瘀")] <- "7"
  chiqTCM$體質[which(chiqTCM$體質=="痰瘀+陽虛")] <- "8"
  chiqTCM$體質 <- as.numeric(chiqTCM$體質)
  
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
  
  write.csv(chiqTCM,file='C:\\R\\chiqTCM.csv',fileEncoding = "Big5",append = FALSE)
  
  #卡方(每段10個)


chisq.test(chiqTCM$體質,chiqTCM$BODY_HEIGHT)
chisq.test(chiqTCM$體質,chiqTCM$BODY_WEIGHT)
chisq.test(chiqTCM$體質,chiqTCM$BMI)
chisq.test(chiqTCM$體質,chiqTCM$BODY_FAT_RATE)
chisq.test(chiqTCM$體質,chiqTCM$BODY_WAISTLINE)
chisq.test(chiqTCM$體質,chiqTCM$BODY_BUTTOCKS)
chisq.test(chiqTCM$體質,chiqTCM$WHR)
chisq.test(chiqTCM$體質,chiqTCM$SIT_1_SYSTOLIC_PRESSURE)
chisq.test(chiqTCM$體質,chiqTCM$SIT_1_DIASTOLIC_PRESSURE)
chisq.test(chiqTCM$體質,chiqTCM$SIT_2_SYSTOLIC_PRESSURE)

chisq.test(chiqTCM$體質,chiqTCM$SIT_2_DIASTOLIC_PRESSURE)
chisq.test(chiqTCM$體質,chiqTCM$SIT_3_SYSTOLIC_PRESSURE)
chisq.test(chiqTCM$體質,chiqTCM$SIT_3_DIASTOLIC_PRESSURE)
chisq.test(chiqTCM$體質,chiqTCM$SIT_1_HEARTBEAT_SPEED)
chisq.test(chiqTCM$體質,chiqTCM$SIT_2_HEARTBEAT_SPEED)
chisq.test(chiqTCM$體質,chiqTCM$SIT_3_HEARTBEAT_SPEED)
chisq.test(chiqTCM$體質,chiqTCM$BONE_EXAM_RESULT)
chisq.test(chiqTCM$體質,chiqTCM$T_SCORE)
chisq.test(chiqTCM$體質,chiqTCM$Z_SCORE)
chisq.test(chiqTCM$體質,chiqTCM$VC)

chisq.test(chiqTCM$體質,chiqTCM$TV)
chisq.test(chiqTCM$體質,chiqTCM$ERV)
chisq.test(chiqTCM$體質,chiqTCM$IRV)
chisq.test(chiqTCM$體質,chiqTCM$IC)
chisq.test(chiqTCM$體質,chiqTCM$VC_HT)
chisq.test(chiqTCM$體質,chiqTCM$FVC)
chisq.test(chiqTCM$體質,chiqTCM$FVC_PRED)
chisq.test(chiqTCM$體質,chiqTCM$FEV10)
chisq.test(chiqTCM$體質,chiqTCM$FEV10_PRED)
chisq.test(chiqTCM$體質,chiqTCM$FEV10_FVC)

chisq.test(chiqTCM$體質,chiqTCM$FEV10_FVC_PRED)
chisq.test(chiqTCM$體質,chiqTCM$FEV10_SVC)
chisq.test(chiqTCM$體質,chiqTCM$RBC)
chisq.test(chiqTCM$體質,chiqTCM$WBC)
chisq.test(chiqTCM$體質,chiqTCM$PLATELET)
chisq.test(chiqTCM$體質,chiqTCM$HB)
chisq.test(chiqTCM$體質,chiqTCM$HCT)
chisq.test(chiqTCM$體質,chiqTCM$HBA1C)
chisq.test(chiqTCM$體質,chiqTCM$ANTI_HCV_AB_1)
chisq.test(chiqTCM$體質,chiqTCM$ANTI_HCV_AB_2)

chisq.test(chiqTCM$體質,chiqTCM$HBSAG_1)
chisq.test(chiqTCM$體質,chiqTCM$HBSAG_2)
chisq.test(chiqTCM$體質,chiqTCM$HBEAG_1)
chisq.test(chiqTCM$體質,chiqTCM$HBEAG_2)
chisq.test(chiqTCM$體質,chiqTCM$ANTI_HBS_AB_1)
chisq.test(chiqTCM$體質,chiqTCM$ANTI_HBS_AB_2)
chisq.test(chiqTCM$體質,chiqTCM$ANTI_HBC_AB_1)
chisq.test(chiqTCM$體質,chiqTCM$ANTI_HBC_AB_2)
chisq.test(chiqTCM$體質,chiqTCM$ANTI_HDV_AB_1)
chisq.test(chiqTCM$體質,chiqTCM$ANTI_HDV_AB_2)

chisq.test(chiqTCM$體質,chiqTCM$FASTING_GLUCOSE)
chisq.test(chiqTCM$體質,chiqTCM$T_CHO)
chisq.test(chiqTCM$體質,chiqTCM$TG)
chisq.test(chiqTCM$體質,chiqTCM$HDL_C)
chisq.test(chiqTCM$體質,chiqTCM$LDL_C)
chisq.test(chiqTCM$體質,chiqTCM$T_BILIRUBIN)
chisq.test(chiqTCM$體質,chiqTCM$ALBUMIN)
chisq.test(chiqTCM$體質,chiqTCM$SGOT)
chisq.test(chiqTCM$體質,chiqTCM$SGPT)
chisq.test(chiqTCM$體質,chiqTCM$GAMMA_GT)

chisq.test(chiqTCM$體質,chiqTCM$AFP)
chisq.test(chiqTCM$體質,chiqTCM$BUN)
chisq.test(chiqTCM$體質,chiqTCM$CREATININE)
chisq.test(chiqTCM$體質,chiqTCM$URIC_ACID)
chisq.test(chiqTCM$體質,chiqTCM$MICROALB)
chisq.test(chiqTCM$體質,chiqTCM$CREATININE_URINE)

