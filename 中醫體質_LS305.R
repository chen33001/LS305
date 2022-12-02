library(tidyverse)

#抽取資料---------------------------------------------------------------------------------------------------------------------------------
#生菌讀取路徑:C:\\R\\      威甫讀取路徑:C:\\R\\LS305中醫
#各項體檢資料
measure <- read.csv("C:\\R\\LS305中醫\\release_list_measure.csv",sep=",", header=TRUE,na = "NA")
#各種體質資料
TCMlist<- read.csv("C:\\R\\LS305中醫\\TCM_list20220924.csv",fileEncoding = "Big5")
##體質跟各項體檢資料
TCMcal <- read.csv("C:\\R\\LS305中醫\\TCMmerge3.csv",fileEncoding = "Big5")


#分離資料---------------------------------------------------------------------------------------------------------------------------------
age<-c(TCMlist$age_gruop)
sex<-c(TCMlist$SEX)
con<-c(TCMlist$體質)
#男女體質
newTCMlist1<-TCMlist[,c(2,48)]
newTCMlist1$體質<-factor(newTCMlist1$體質)

#繪圖------------------------------------------------------------------------------------------------------------------------------

par(mfrow = c(1,1),xpd=NA,oma= c(0,0,0,8))
barplot(table(con,sex),main='男女體質人數',beside=T,col=rainbow(nlevels(newTCMlist1$體質)))
legend(par("usr")[2],par("usr")[4],legend = as.character(levels(newTCMlist1$體質)),fill=rainbow(nlevels(newTCMlist1$體質)))
table(con,sex)

#年齡體質
newTCMlist2<-TCMlist[,c(49,48)]
newTCMlist2$體質<-factor(newTCMlist2$體質)

par(mfrow = c(1,1),xpd=NA,oma= c(0,0,0,8))
barplot(table(con,age),main='各年齡體質人數',beside=T,col=rainbow(nlevels(newTCMlist2$體質)))
legend(par("usr")[2],par("usr")[4],legend = as.character(levels(newTCMlist2$體質)),fill=rainbow(nlevels(newTCMlist2$體質)))
table(con,age)


#年齡分布
newTCMlist3<-TCMlist[,c(2,49)]
newTCMlist3$年齡區間<-factor(newTCMlist3$age_gruop)

par(mfrow = c(1,1),xpd=NA,oma= c(0,0,0,8))
barplot(table(age,sex),main='男女年齡分布',beside=T,col=rainbow(nlevels(newTCMlist3$年齡區間)))
legend(par("usr")[2],par("usr")[4],legend = as.character(levels(newTCMlist3$年齡區間)),fill=rainbow(nlevels(newTCMlist3$年齡區間)))
table(age,sex)

#匯出各體質男女人數&各年齡體質人數 csv檔
aaa <- table(con,sex)
bbb <- table(age,sex)
write.csv(aaa, file = "C:\\R\\LS305中醫\\各體質男女人數.csv",fileEncoding = "Big5")
write.csv(bbb, file = "C:\\R\\LS305中醫\\各年齡體質人數.csv",fileEncoding = "Big5")

#取體檢取特定欄位---------------------------------------------------------------------------------------------------------------------------------------

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

#抽取中醫體質的"Release_No","體質","SEX","AGE","age_gruop"和release_list_measure.csv依照 "Release_No"合併------------------------------------------------
TCMlist<-subset(TCMlist,select=c("Release_No","體質","SEX","AGE","age_gruop"))
TCMmerge<-merge(TCMlist,measure_extract, by = "Release_No", all.TCMlist = T)



#TCMmerge合併TWB1,2序號
TWB12 <- read.csv("c:\\R\\LS305中醫\\lab_info.csv",fileEncoding = "Big5")
names(TWB12)[1] <- "Release_No"
TWB12 <- subset(TWB12,select=c("Release_No","TWB1_ID","TWB2_ID"))
TCMmerge2<-merge(TWB12,TCMmerge, by = "Release_No", all.TCMmerge = T)

#用多重變項去除重複列
TCMmerge3 <- distinct(TCMmerge2, BMI,BODY_FAT_RATE,BODY_WAISTLINE,BODY_BUTTOCKS,
                      WHR,SIT_1_SYSTOLIC_PRESSURE,SIT_1_DIASTOLIC_PRESSURE,
                      SIT_2_SYSTOLIC_PRESSURE,VC,TV,ERV,
                      .keep_all = TRUE )



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
  
write.csv(chiqTCM,file='C:\\R\\LS305中醫\\chiqTCM.csv',fileEncoding = "Big5")
  
#做卡方檢定---------------------------------------------------------------------------------------------------------------------------

use_vb <- c("SEX", "AGE", 
            "age_gruop", "BODY_HEIGHT", "BODY_WEIGHT", "BMI", "BODY_FAT_RATE", 
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

result_set <- data.frame(vb1 = NA, vb2 = NA, p_value = NA)
xxx1 <- chiqTCM
for (i in 1:length(use_vb)) {
  result_set[i,1] <- "體質"
  result_set[i,2] <- use_vb[i]
  result_set[i,3] <- chisq.test(xxx1[,c("體質")],xxx1[,use_vb[i]])$p.value
}
#匯出檔案---------------------------------------------------------------------------------------------------------------------------------
write.csv(result_set,file='C:\\R\\LS305中醫\\卡方結果.csv',fileEncoding = "Big5")

