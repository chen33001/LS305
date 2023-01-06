library(tidyverse)
library(Hmisc) # install.packages("Hmisc") straight from CRAN. version 	4.7-2
library(grid) # version 4.3.0
library(gtable) # version 0.3.1
library(gridExtra)


#抽取資料----------------------------------------------------------------------------------------------------------------------------
#生菌讀取路徑:C:\\R\\      威甫讀取路徑:C:\\R\\LS305中醫
#各項體檢資料
measure <- read.csv("C:\\R\\LS305中醫\\release_list_measure.csv",sep=",", header=TRUE,na = "NA")
#各種體質資料
TCMlist<- read.csv("C:\\R\\LS305中醫\\TCM_list20220924.csv",fileEncoding = "Big5")
#體質跟各項體檢資料
TCMcal <- read.csv("C:\\R\\LS305中醫\\TCMmerge3.csv",fileEncoding = "Big5")
#做anova所需之資料
TCM_group <- read.csv("C:\\R\\LS305中醫\\TCM_group.csv",fileEncoding = "Big5")


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
#卡方檢定之資料清洗----------------------------------------------------------------------------------------
chiqTCM$體質 <- as.character(chiqTCM$體質)

chiqTCM$體質[which(chiqTCM$體質=="平和")] <- 1
chiqTCM$體質[which(chiqTCM$體質=="陰虛")] <- 2
chiqTCM$體質[which(chiqTCM$體質=="陰虛+陽虛")] <- 3
chiqTCM$體質[which(chiqTCM$體質=="陰虛+陽虛+痰瘀")] <- 4
chiqTCM$體質[which(chiqTCM$體質=="陰虛+痰瘀")] <- 5
chiqTCM$體質[which(chiqTCM$體質=="陽虛")] <- 6
chiqTCM$體質[which(chiqTCM$體質=="痰瘀")] <- 7
chiqTCM$體質[which(chiqTCM$體質=="痰瘀+陽虛")] <- 8
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

#T-test資料清洗-----------------------------------------------------------------
chiqTCM[is.na(chiqTCM)] <- 0
chiqTCM$HBA1C <- as.numeric(chiqTCM$HBA1C)
chiqTCM$ANTI_HBS_AB_2 <- as.numeric(chiqTCM$ANTI_HBS_AB_2)
chiqTCM$SGPT <- as.numeric(chiqTCM$SGPT)
chiqTCM$GAMMA_GT <- as.numeric(chiqTCM$GAMMA_GT)
chiqTCM$AFP <- as.numeric(chiqTCM$AFP)
chiqTCM$MICROALB <- as.numeric(chiqTCM$MICROALB)
for (i in length(use_T_test)){
  chiqTCM$use_T_test[i] <- as.numeric(chiqTCM$use_T_test[i])
  
}

  
#Anova 資料清洗---------------------------------------------------------------------------------------------
source("C:\\\\LS305Anova_table_export.R")

TCM_group <- TCM_group[,-grep("AGE|age_gruop",colnames(TCM_group))] #去除sex AGE age_group
TCM_Anova_mergerdata<- merge(TCM_group, chiqTCM, by = "Release_No",all.x = TRUE  )
TCM_Anova <- TCM_Anova_mergerdata[,-grep("X|TWB1_ID|TWB2_ID|SEX.y|體質",colnames(TCM_Anova_mergerdata))]
TCM_Anova <- cbind(TCM_Anova[,1:24], TCM_Anova_mergerdata$BONE_EXAM_RESULT, TCM_Anova[,25:ncol(TCM_Anova)])
TCM_Anova <- TCM_Anova[-6244,]

#隔離無法ANOVA的資料
TCM_Anova_failed <- subset(TCM_Anova,
                           select = c("MVV","ANTI_HCV_AB_1","HBSAG_1","HBEAG_1",
                                      "ANTI_HBS_AB_1","ANTI_HBC_AB_1","ANTI_HDV_AB_1")
)
TCM_Anova <- TCM_Anova[,-grep("MVV|ANTI_HCV_AB_1|HBSAG_1|HBEAG_1|ANTI_HBS_AB_1|ANTI_HBC_AB_1|ANTI_HDV_AB_1",colnames(TCM_Anova))]


#做Anova +製圖輸出 (Anova_table_export.R取自Github上別人提供的程式碼)-------------------------------------------------
source("Anova_table_export.R")
#open PDF
pdf(file = "C:\\R\\LS305中醫\\TCM_anova_result.pdf",
    width = 10,
    height = 5)

#running plots
for (i in c(1:79)){
  TCM_Anova[,i+8] <- as.numeric(as.character(TCM_Anova[,i+8]))
  TCM_Anova[,i+8][is.na(TCM_Anova[,i+8]) | TCM_Anova[,i+8]=="Inf"] = NA
  model1 <- summary(aov(TCM_Anova[,i+8] ~ TCM_Anova$Yin_def*TCM_Anova$Yang_def*TCM_Anova$Phlegm_stasis ))
  DisplayAnovaSummary(model_summary_object = model1, title = names(TCM_Anova[i+8]), title_font_size = 16,footnote = "")
}


#turn off PDF plotting
dev.off() 
#ERROR:Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
#NA/NaN/Inf 出現於 'y'
#TCM_ANOVA:c30=VC_PRED,變數是chr

#Error in if ((model_summary$Pr..F.)[x] < 0.001) { : 
#missing value where TRUE/FALSE needed   
# TCM_ANOVA$MVV，值皆為NA

# Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
#   NA/NaN/Inf 出現於 'y'
# In addition: Warning message:
#   In storage.mode(v) <- "double" : NAs introduced by coercion
# 變數是chr，且只有1,2兩種數字
#取出anova圖中的p value製成表格--------------------------------------------------------------------
TCM_Anova_p_value <- data.frame(c(1,1,1,1,1,1,1,1))

for (i in c(1:79)){
  model1 <- summary(aov(TCM_Anova[,i+8] ~ TCM_Anova$Yin_def*TCM_Anova$Yang_def*TCM_Anova$Phlegm_stasis ))
  TCM_Anova_p_value[,i] <- model1[[1]][["Pr(>F)"]]
}

#從TCM_Anova抓欄名(加欄名)
TCM_Anova_name1 <- colnames(subset(TCM_Anova,
                                   select = c(9:87)))
colnames(TCM_Anova_p_value) <- TCM_Anova_name1

#加列名
TCM_Anova_name2 <- c("TCM_Anova$Yin_def","TCM_Anova$Yang_def","TCM_Anova$Phlegm_stasis","TCM_Anova$Yin_def:TCM_Anova$Yang_def",
                     "TCM_Anova$Yin_def:TCM_Anova$Phlegm_stasis","TCM_Anova$Yang_def:TCM_Anova$Phlegm_stasis",
                     "CM_Anova$Yin_def:TCM_Anova$Yang_def:TCM_Anova$Phlegm_stasis","Residuals")
rownames(TCM_Anova_p_value) <- TCM_Anova_name2

#t()矩陣欄列互換
TCM_Anova_p_value <- data.frame(t(TCM_Anova_p_value))

#加變相中文解釋
TCM_Anova_p_value_meaning1 <- c("身高","體重","身體質量指數","體脂肪率","腰圍","臀圍","腰臀比",
                                "靜坐時第一次收縮壓","靜坐時第一次舒張壓",
                                "靜坐時第二次收縮壓","靜坐時第二次舒張壓",
                                "靜坐時第三次收縮壓","靜坐時第三次舒張壓",
                                "靜坐時第一次心跳","靜坐時第二次心跳","靜坐時第三次心跳",
                                "STIFFNESS INDEX骨硬度指數","T參數值","Z參數值","超音波速度",
                                "寬頻超音波遞減值","肺活量","肺活量基準值","一回換氣量","預備呼氣量",
                                "預備吸氣量","預備吸氣量+一回換氣量","肺活量/身高","努力性肺活量",
                                "努力性肺活量基準值"," Zeor點至1秒經過時的呼出量（一秒量）",
                                "一秒量基準值"," 1秒率，(一秒量/努力性肺活量)*100 "," 1秒率基準值",
                                " 1秒率，(一秒量/肺活量)*100 "," FEV10_SVC基準值"," FEV10_SVC基準值",
                                " (FEV1.0/VC_PRED)*100 ","最大中間呼氣量(努力性肺活量的75％到25％的平均量)",
                                "最大中間呼氣量基準值","最大呼氣流量","最大呼氣流量基準值",
                                "努力性肺活量75％時流量","努力性肺活量75％時流量的基準值",
                                "努力性肺活量50％時流量","努力性肺活量50％時流量的基準值",
                                "努力性肺活量25％時流量","努力性肺活量25％時流量的基準值",
                                "努力性肺活量25％時流量/身高","努力性肺活量25％時流量/身高的基準值",
                                " 1秒率，(一秒量/肺活量)*100 ",
                                "紅血球","白血球","血小板","血紅素","血球比容","醣化血色素值",
                                " C型肝炎抗體值"," B型肝炎表面抗原值"," B型肝炎e抗原值",
                                " B型肝炎表面抗體值"," B型肝炎核心抗體值"," D型肝炎抗體值",
                                "飯前血糖","總膽固醇","三酸甘油酯",
                                "高密度酯蛋白膽固醇","低密度酯蛋白膽固醇","總膽紅素","白蛋白",
                                "血清麩胺酸苯醋酸轉氨基酵素","血清麩胺酸丙酮酸轉氨基酵素",
                                "γ －麩胺醯轉移酵素","甲型胎兒血清蛋白","血中尿素氮",
                                "肌酸酐","尿酸","尿中微蛋白","尿中肌酸酐"
)
TCM_Anova_p_value <- cbind(TCM_Anova_p_value_meaning1, TCM_Anova_p_value)

TCM_Anova_p_value_meaning2 <- c("中文解釋","陰虛","陽虛","痰瘀","陰虛+陽虛",
                                "陰虛+痰瘀","陽虛+痰瘀",
                                "陰虛+陽虛+痰瘀","殘餘")
TCM_Anova_p_value <- rbind(TCM_Anova_p_value_meaning2, TCM_Anova_p_value)

#做卡方檢定---------------------------------------------------------------------------------------------------------------------------

use_vb <- c("SEX", 
            "age_gruop", 
            "ANTI_HCV_AB_1", "HBSAG_1", "HBEAG_1" 
            , "ANTI_HBS_AB_1", "ANTI_HBC_AB_1" 
            , "ANTI_HDV_AB_1")
use_meaning<- c("性別",
                "年齡層",
                "C型肝炎抗體","B型肝炎表面抗原","B型肝炎e抗原",
                "B型肝炎表面抗體","B型肝炎核心抗體",
                "D型肝炎抗體")
use_T_test <- c("AGE","BODY_HEIGHT","BODY_WEIGHT","BMI","BODY_FAT_RATE","BODY_WAISTLINE", "BODY_BUTTOCKS", "WHR", "SIT_1_SYSTOLIC_PRESSURE", 
                "SIT_1_DIASTOLIC_PRESSURE", "SIT_2_SYSTOLIC_PRESSURE", "SIT_2_DIASTOLIC_PRESSURE", 
                "SIT_3_SYSTOLIC_PRESSURE", "SIT_3_DIASTOLIC_PRESSURE", "SIT_1_HEARTBEAT_SPEED", 
                "SIT_2_HEARTBEAT_SPEED", "SIT_3_HEARTBEAT_SPEED", "BONE_EXAM_RESULT", 
                "T_SCORE", "Z_SCORE","VC", "TV", "ERV", "IRV", "IC", "VC_HT", 
                "FVC", "FEV10", "RBC", "WBC", "PLATELET", "HB", "HCT", "HBA1C",
                "ANTI_HCV_AB_2", "HBSAG_2","HBEAG_2", "ANTI_HBS_AB_2","ANTI_HBC_AB_2", "ANTI_HDV_AB_2", "FASTING_GLUCOSE", 
                "T_CHO", "TG", "HDL_C", "LDL_C", "T_BILIRUBIN", "ALBUMIN", "SGOT", 
                "SGPT", "GAMMA_GT", "AFP", "BUN", "CREATININE", "URIC_ACID", 
                "MICROALB", "CREATININE_URINE")
use_T_test_meaning <- c("年齡","身高","體重","身體質量指數","體脂肪率","腰圍", "臀圍", "腰臀比", "靜坐時第一次收縮壓", 
                        "靜坐時第一次舒張壓", "靜坐時第二次收縮壓", "靜坐時第二次舒張壓", 
                        "靜坐時第三次收縮壓", "靜坐時第三次舒張壓", "靜坐時第一次心跳", 
                        "靜坐時第二次心跳", "靜坐時第三次心跳", "STIFFNESS INDEX骨硬度指數", 
                        "T參數值", "Z參數值","肺活量", "一回換氣量", "預備呼氣量", "預備吸氣量", "預備呼氣量+預備吸氣量", "肺活量/身長", 
                        "強制呼出時肺活量（努力性肺活量）", "Zeor點至1秒經過時的呼出量（一秒量）", "紅血球", "白血球", "血小板", "血紅素", "血球比容", "醣化血色素值",
                        "AC型肝炎抗體（值）", "B型肝炎表面抗原（值）","B型肝炎e抗原(值)", "B型肝炎表面抗體（值）","B型肝炎核心抗體（值）", "D型肝炎抗體（值）", "飯前血糖", 
                        "總膽固醇", "三酸甘油酯", "高密度酯蛋白膽固醇", "低密度酯蛋白膽固醇", "總膽紅素", "白蛋白", "血清麩胺酸苯醋酸轉氨基酵素", 
                        "血清麩胺酸丙酮酸轉氨基酵素", "γ －麩胺醯轉移酵素", "甲型胎兒血清蛋白", "血中尿素氮", "肌酸酐", "尿酸", 
                        "尿中微蛋白", "尿中肌酸酐")

result_set <- data.frame(vb1 = NA, vb2 = NA, vb2_釋義 = NA, p_value = NA)
xxx1 <- chiqTCM
for (i in 1:length(use_vb)) {
  result_set[i,1] <- "體質"
  result_set[i,2] <- use_vb[i]
  result_set[i,3] <- use_meaning[i]
  result_set[i,4] <- chisq.test(xxx1[,c("體質")],xxx1[,use_vb[i]])$p.value
}

xxx2 <- chiqTCM
result_set_T_test <- data.frame(vb1 = NA, vb2 = NA, vb2_釋義 = NA , p_value = NA)
for (i in 1:length(use_T_test)) {
  result_set_T_test[i,1] <- "體質"
  result_set_T_test[i,2] <- use_T_test[i]
  result_set_T_test[i,3] <- use_T_test_meaning[i]
  result_set_T_test[i,4] <- t.test(xxx2[,5],xxx2[,use_T_test[i]])$p.value
}
#--------------作質方圖
for (i in 1:length(use_T_test)){
  hist(x=xxx2[,use_T_test[i]], main=use_T_test[i],xlab=use_T_test[i], ylab="數量")
}


#做Anova +輸出-------------------------------------------------

#匯出檔案---------------------------------------------------------------------------------------------------------------------------------
write.csv(result_set,file='C:\\R\\LS305中醫\\卡方結果.csv',fileEncoding = "Big5")
write.csv(result_set_T_test, file='C:\\R\\LS305中醫\\T-test.csv',fileEncoding = "Big5")
write.csv(chiqTCM, file='C:\\R\\LS305中醫\\中醫體質清理完成之資料.csv',fileEncoding = "Big5")
write.csv(TCM_Anova, file = 'C:\\R\\LS305中醫\\TCM_Anova.csv',fileEncoding = "Big5")
#輸出表格
write.csv(TCM_Anova_p_value, file='C:\\R\\TCM_Anova_p_value.xlsx',fileEncoding = "Big5")
write.csv(TCM_Anova_p_value, file='C:\\R\\LS305中醫\\TCM_Anova_p_value.xlsx',fileEncoding = "Big5")
