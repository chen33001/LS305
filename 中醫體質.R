#讀入資料
library(tidyverse)
data <- read.csv("C:\\R\\LS305中醫\\TCM_list20220924.csv",fileEncoding = "Big5")
head(data)

#分離資料
sex<-c(data$SEX)
age<-c(data$age_gruop)
yin<-c(data$陰虛體質)
yang<-c(data$陽虛體質)
TCM<-c(data$痰瘀體質)
peace<-c(data$平和體質)
con<-c(data$體質)
#體質
newdata1<-data[,c(2,48)]
newdata1$體質<-factor(newdata1$體質)
#年齡層
newdata2<-data[,c(2,49)]
newdata2$age_gruop<-factor(newdata2$age_gruop)
#各年齡層體質人數

#繪圖
library(ggplot2)
#比較男女各體質的人數
par(mfrow = c(1,1),xpd=NA,oma=c(0,0,0,8))
barplot(table(con,sex),main='男女各體質人數',beside=T,col=rainbow(nlevels(newdata1$體質)))
legend(par("usr")[2],par("usr")[2],legend=as.character(levels(newdata1$體質)),fill=rainbow(nlevels(newdata1$體質)),col="black",xjust=0, yjust=-0.2)

#比較年齡層人數
par(mfrow = c(1,1),xpd=NA,oma=c(0,0,0,8))
barplot(table(age,sex),main='各年齡層體質人數',beside=T,col=rainbow(nlevels(newdata2$age_gruop)))
legend(par("usr")[2],par("usr")[2],legend=as.character(levels(newdata2$age_gruop)),fill=rainbow(nlevels(newdata2$age_gruop)),col="black",xjust=0, yjust=-0.2)


#匯出資料
aaa <- table(con,sex)
bbb <- table(age,sex)
write.csv(aaa, file = "C:\\R\\LS305中醫\\男女各體質人數.csv",fileEncoding = "Big5")
write.csv(bbb, file = "C:\\R\\LS305中醫\\男女各年齡人數.csv",fileEncoding = "Big5")

#anova(性別和體質)
data2 <- read.csv("C:\\R\\LS305中醫\\男女各體質人數.csv",fileEncoding = "Big5")
sex2 <- c(data2$女性)
sex3 <- c(data2$男性)
t.test(sex2,sex3, var.equal=TRUE)

#anova(年齡和體質)
data3 <- read.csv("C:\\R\\LS305中醫\\test2.csv",fileEncoding = "Big5")
age2 <- c(data3$女性)
age3 <- c(data3$男性)
t.test(age2,age3, var.equal=TRUE)

#BMI和體質的卡方檢定
data4 <- read.csv("C:\\R\\LS305中醫\\TCMmerge3.csv",fileEncoding = "Big5")
Baseline_data <- subset(data4, FOLLOW=="Baseline")
BMI.table <- table(data5$體質,data5$BMI)
chisq.test(BMI.table)



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
