library(stringr)
library(dplyr)
#生菌讀取路徑:C:\\R\\      威甫讀取路徑:C:\\R\\LS305中醫\\
#讀取資料---------------------------------------------------------------
TCM_Anova <- read.csv("C:\\R\\LS305中醫\\TCM_Anova.csv",fileEncoding = "big5")
TCMmerge3 <- read.csv("C:\\R\\LS305中醫\\TCMmerge3.csv",fileEncoding = "big5")

#資料清洗---------------------------------------------------------------
TCM_Anova <- subset(TCM_Anova, select = c("Release_No","Yin_def","Yang_def","Phlegm_stasis"))
TCMmerge3 <- subset(TCMmerge3, select = c("Release_No","TWB1_ID"))
rm.TCMmerge3 <- TCMmerge3 %>% filter(TWB1_ID != "") %>% distinct()

#資料合併更改column名稱------------------------------------------------------------------------
names(rm.TCMmerge3) <- c("FID","IID")
GWAS_Yin <- cbind(rm.TCMmerge3[,1], rm.TCMmerge3[,2],TCM_Anova[,2])
GWAS_Yin <- data.frame(GWAS_Yin)
names(GWAS_Yin) <- c("FID","IID","Yin_def")

GWAS_Yang <- cbind(rm.TCMmerge3[,1], rm.TCMmerge3[,2], TCM_Anova[,3])
GWAS_Yang <- data.frame(GWAS_Yang)
names(GWAS_Yang) <- c("FID","IID","Yang_def")

GWAS_Phlegm_stasis <- cbind(rm.TCMmerge3[,1], rm.TCMmerge3[,2], TCM_Anova[,4])
GWAS_Phlegm_stasis <- data.frame(GWAS_Phlegm_stasis)
names(GWAS_Phlegm_stasis) <- c("FID","IID","Phlegm_stasis")
#去除GWAS重複列-----------------------------------------------------------------------
GWAS_Yin <- GWAS_Yin %>% distinct()
GWAS_Yang <- GWAS_Yang %>% distinct()
GWAS_Phlegm_stasis <- GWAS_Phlegm_stasis %>% distinct()



#FID跟IID相同的情況------------------------------------------------------------
rm.col_same <- cbind(rm.TCMmerge3$TWB1_ID, rm.TCMmerge3$TWB1_ID)
rm.col_same <- data.frame(rm.col_same)
  

GWAS_Yin <- cbind(rm.col_same[,1], rm.col_same[,2],TCM_Anova[,2])
GWAS_Yin <- data.frame(GWAS_Yin)
names(GWAS_Yin) <- c("FID","IID","Yin_def")

GWAS_Yang <- cbind(rm.col_same[,1], rm.col_same[,2], TCM_Anova[,3])
GWAS_Yang <- data.frame(GWAS_Yang)
names(GWAS_Yang) <- c("FID","IID","Yang_def")

GWAS_Phlegm_stasis <- cbind(rm.col_same[,1], rm.col_same[,2], TCM_Anova[,4])
GWAS_Phlegm_stasis <- data.frame(GWAS_Phlegm_stasis)
names(GWAS_Phlegm_stasis) <- c("FID","IID","Phlegm_stasis")

GWAS_Yin <- GWAS_Yin %>% distinct()
GWAS_Yang <- GWAS_Yang %>% distinct()
GWAS_Phlegm_stasis <- GWAS_Phlegm_stasis %>% distinct()



#資料匯出--------------------------------------------------------------------------
write.table(GWAS, file = "C:\\R\\LS305中醫\\GWAS.txt",sep = "\t",row.names = F,quote = F)
write.table(GWAS_Yin, file = "C:\\R\\LS305中醫\\GWAS_Yin.txt",sep = "\t",row.names = F,quote = F)
write.table(GWAS_Yang, file = "C:\\R\\LS305中醫\\GWAS_Yang.txt",sep = "\t",row.names = F,quote = F)
write.table(GWAS_Phlegm_stasis, file = "C:\\R\\LS305中醫\\GWAS_Phlegm_stasis.txt",sep = "\t",row.names = F,quote = F)
