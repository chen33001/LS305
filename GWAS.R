library(stringr)
library(dplyr)
#生菌讀取路徑:C:\\R\\      威甫讀取路徑:C:\\R\\LS305中醫\\
#讀取資料---------------------------------------------------------------
TCM_Anova <- read.csv("C:\\R\\LS305中醫\\TCM_Anova.csv",fileEncoding = "big5")
TCMmerge3 <- read.csv("C:\\R\\LS305中醫\\TCMmerge3.csv",fileEncoding = "big5")
TCM_group <- read.csv("C:\\R\\LS305中醫\\TCM_group.csv",fileEncoding = "big5")
#資料清洗---------------------------------------------------------------
TCM_Anova <- subset(TCM_Anova, select = c("Release_No","Yin_def","Yang_def","Phlegm_stasis"))
TCMmerge3 <- subset(TCMmerge3, select = c("Release_No","TWB1_ID"))
rm.TCMmerge3 <- TCMmerge3 %>% filter(TWB1_ID != "") %>% distinct()
covar_cons <- merge(rm.TCMmerge3, TCM_group)


#資料合併更改column名稱(FID跟IID相同的情況)------------------------------------------------------------------------
rm.col_same <- cbind(rm.TCMmerge3$TWB1_ID, rm.TCMmerge3$TWB1_ID)
rm.col_same <- data.frame(rm.col_same)
GWAS_cons <- cbind(rm.col_same[,1], rm.col_same[,2],TCM_Anova[,2], TCM_Anova[,3], TCM_Anova[,4])
GWAS_cons <- data.frame(GWAS_cons)
names(GWAS_cons) <- c("FID" ,"IID","Yin_def","Yang_def","Phlegm_stasis")
#去除GWAS重複列-----------------------------------------------------------------------
GWAS_cons <- GWAS_cons %>% distinct()
#修改Yin, Yang, phlegm(0 --> 1 1 --> 2)-------------------------------------------------------------------------------------
#Yin------------------------
GWAS_cons$Yin_def[which(GWAS_cons$Yin_def=="1")] <- 2
GWAS_cons$Yin_def[which(GWAS_cons$Yin_def=="0")] <- 1
#Yang----------------------------------------------
GWAS_cons$Yang_def[which(GWAS_cons$Yang_def=="1")] <- 2
GWAS_cons$Yang_def[which(GWAS_cons$Yang_def=="0")] <- 1
#phlegm----------------------------------------------
GWAS_cons$Phlegm_stasis[which(GWAS_cons$Phlegm_stasis=="1")] <- 2
GWAS_cons$Phlegm_stasis[which(GWAS_cons$Phlegm_stasis=="0")] <- 1

#covar_cons--------------------------------------------------------------------------------------------------
covar_cons <- cbind(rm.TCMmerge3$Release_No, rm.TCMmerge3$TWB1_ID)
covar_cons <- data.frame(covar_cons)
names(covar_cons) <- c("Release_No", "TWB1_ID")
covar_cons_sex_age <- merge(covar_cons, TCM_group, by = "Release_No")
covar_cons_sex_age <- subset(covar_cons_sex_age[,2:4])
covar_cons_sex_age <- cbind.data.frame(rm.TCMmerge3$TWB1_ID, covar_cons_sex_age)
names(covar_cons_sex_age) <- c("FID" ,"IID","Sex","Age" )



#資料匯出--------------------------------------------------------------------------
write.table(GWAS, file = "C:\\R\\LS305中醫\\GWAS.txt",sep = "\t",row.names = F,quote = F)
write.table(GWAS_cons, file = "C:\\R\\LS305中醫\\GWAS_cons.txt",sep = "\t",row.names = F,quote = F)
write.table(rm.col_same,file = "C:\\R\\LS305中醫\\list.txt",sep = "\t",row.names = F,quote = F)
write.table(covar_cons_sex_age,file = "C:\\R\\LS305中醫\\covar_cons.txt",sep = "\t",row.names = F,quote = F)
