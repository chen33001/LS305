library(stringr)
library(dplyr)

#讀取資料---------------------------------------------------------------
TCM_Anova <- read.csv("C:\\R\\LS305中醫\\TCM_Anova.csv",fileEncoding = "big5")
TCMmerge3 <- read.csv("C:\\R\\LS305中醫\\TCMmerge3.csv",fileEncoding = "big5")

#資料清洗---------------------------------------------------------------
TCM_Anova <- subset(TCM_Anova, select = c("Release_No","Yin_def","Yang_def","Phlegm_stasis"))
TCMmerge3 <- subset(TCMmerge3, select = c("Release_No","TWB1_ID"))
rm.TCMmerge3 <- TCMmerge3 %>% filter(TWB1_ID != "")

#資料合併------------------------------------------------------------------------
GWAS <- merge(TCMmerge3,TCM_Anova , by = "Release_No")
#資料匯出--------------------------------------------------------------------------
write.table(GWAS, file = "C:\\R\\LS305中醫\\GWAS.txt",sep = "\t",row.names = F,quote = F)
