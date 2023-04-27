library(stringr)
library(dplyr)
library(CMplot)

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

#covar_cons---log-----------------------------------------------------------------------------------------------
covar_cons <- cbind(rm.TCMmerge3$Release_No, rm.TCMmerge3$TWB1_ID)
covar_cons <- data.frame(covar_cons)
names(covar_cons) <- c("Release_No", "TWB1_ID")
covar_cons_sex_age <- merge(covar_cons, TCM_group, by = "Release_No")
covar_cons_sex_age <- subset(covar_cons_sex_age[,2:4])
covar_cons_sex_age <- cbind.data.frame(rm.TCMmerge3$TWB1_ID, covar_cons_sex_age)
names(covar_cons_sex_age) <- c("FID" ,"IID","Sex","Age" )

#作圖-----------------------------------------------------------------------------------
setwd("C:\\R\\GWAS")
result_1 <- read.table("Yin_def_result.assoc.logistic", header = TRUE)
result_2<- read.table("Phlegm_stasis_result.assoc.logistic", header = TRUE)
result_3 <- read.table("Yang_def_result.assoc.logistic", header = TRUE)


pvalue_1 <- result_1[, c("SNP", "CHR", "BP", "P")]
pvalue_2 <- result_2[, c("SNP", "CHR", "BP", "P")]
pvalue_3 <- result_3[, c("SNP", "CHR", "BP", "P")]

#manhattan plot
for (i in 1:3){
  CMplot(str_c("pvalue_",i), plot.type = "m", LOG10 = TRUE, threshold = 1e-5, chr.den.col = NULL, 
         file = "jpg", dpi = 300, file.output = TRUE, verbose = FALSE)
  
  #QQ plot
  CMplot(str_c("pvalue_",i), plot.type = "q", conf.int.col = NULL, box = TRUE, 
         file = "jpg", dpi = 300, file.output = TRUE, verbose = FALSE)
  median((str_c("result_",i)$STAT)^2)/0.455
  
  
  
  #subset significant region for LocusZoom----------------------------------------
  # find min p-value
  subset(str_c("pvalue_",i), P == min(P))
}


#作圖迴圈失效-----------------
CMplot(pvalue_1, plot.type = "m", LOG10 = TRUE, threshold = 1e-5, chr.den.col = NULL, 
       file = "jpg", file.name="yin_Manhtn.P", dpi = 300, file.output = TRUE, verbose = FALSE)
CMplot(pvalue_1, plot.type = "q", conf.int.col = NULL, box = TRUE, 
       file = "jpg", file.name="_yin", dpi = 300, file.output = TRUE, verbose = FALSE)

CMplot(pvalue_2, plot.type = "m", LOG10 = TRUE, threshold = 1e-5, chr.den.col = NULL, 
       file = "jpg", file.name="Phlegm_Manhtn.P", dpi = 300, file.output = TRUE, verbose = FALSE)
CMplot(pvalue_2, plot.type = "q", conf.int.col = NULL, box = TRUE, 
       file = "jpg", file.name="_Phlegm", dpi = 300, file.output = TRUE, verbose = FALSE)

CMplot(pvalue_3, plot.type = "m", LOG10 = TRUE, threshold = 1e-5, chr.den.col = NULL, 
       file = "jpg", file.name="Yang_Manhtn.P", dpi = 300, file.output = TRUE, verbose = FALSE)
CMplot(pvalue_3, plot.type = "q", conf.int.col = NULL, box = TRUE, 
       file = "jpg", file.name="_Yang", dpi = 300, file.output = TRUE, verbose = FALSE)

# Yin_def:find another site---------------------------------------
Yin_def_locus <- subset(pvalue_1, P < 1E-5)

#Yin_def subset region
#迴圈

for(i in 1:length(Yin_def_locus$BP)){
  locus <- subset(pvalue_1, CHR == Yin_def_locus[i,2] & BP < Yin_def_locus[i,3] + 400000 & BP > Yin_def_locus[i,3] - 400000)
  write.table(locus, sprintf("locus_%d_%s.txt",Yin_def_locus[i,2] ,Yin_def_locus[i,1]), append = FALSE, quote = FALSE, sep = "\t", 
              row.names = FALSE, col.names = TRUE)
  
}

# Yang_def:find another site--------------------------------------
Yang_def_locus <- subset(pvalue_3, P < 1E-5)
# Yang_def subset region


#迴圈
for(i in 1:length(Yang_def_locus$BP)){
  locus <- subset(pvalue_3, CHR == Yang_def_locus[i,2] & BP < Yang_def_locus[i,3] + 400000 & BP > Yang_def_locus[i,3] - 400000)
  write.table(locus, sprintf("locus_%d_%s.txt",Yang_def_locus[i,2] ,Yang_def_locus[i,1]), append = FALSE, quote = FALSE, sep = "\t", 
              row.names = FALSE, col.names = TRUE)
  
}


# Phlegm_stasis:find another site------------------------------------
Phlegm_stasis_locus <- subset(pvalue_2, P < 1E-5)

# Phlegm_stasis subset region

#迴圈
for(i in 1:length(Phlegm_stasis_locus$BP)){
  locus <- subset(pvalue_2, CHR == Phlegm_stasis_locus[i,2] & BP < Phlegm_stasis_locus[i,3] + 400000 & BP > Phlegm_stasis_locus[i,3] - 400000)
  write.table(locus, sprintf("locus_%d_%s.txt",Phlegm_stasis_locus[i,2] ,Phlegm_stasis_locus[i,1]), append = FALSE, quote = FALSE, sep = "\t", 
              row.names = FALSE, col.names = TRUE)
  
}


#資料匯出--------------------------------------------------------------------------
write.table(GWAS, file = "C:\\R\\GWAS\\GWAS.txt",sep = "\t",row.names = F,quote = F)
write.table(GWAS_cons, file = "C:\\R\\GWAS\\GWAS_cons.txt",sep = "\t",row.names = F,quote = F)
write.table(rm.col_same,file = "C:\\R\\GWAS\\list.txt",sep = "\t",row.names = F,quote = F)
write.table(covar_cons_sex_age,file = "C:\\R\\GWAS\\covar_cons.txt",sep = "\t",row.names = F,quote = F)
