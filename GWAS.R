library(stringr)
library(dplyr)
library(CMplot)
library(openxlsx)
#生菌讀取路徑:C:\\R\\      威甫讀取路徑:C:\\R\\LS305中醫\\
#讀取資料---------------------------------------------------------------
TCM_Anova <- read.csv("C:\\R\\TCM_Anova.csv",fileEncoding = "big5")
TCMmerge3 <- read.csv("C:\\R\\TCMmerge3.csv",fileEncoding = "big5")
TCM_group <- read.csv("C:\\R\\TCM_group.csv",fileEncoding = "big5")
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
setwd("C:\\GWAS")
result <- read.table("result.assoc.logistic", header = TRUE)
pvalue <- result[, c("SNP", "CHR", "BP", "P")]
#manhattan plot 
CMplot(pvalue, plot.type = "m", LOG10 = TRUE, threshold = 1e-5, chr.den.col = NULL, 
       file = "jpg", memo = "", dpi = 300, file.output = TRUE, verbose = FALSE)
#QQ plot
CMplot(pvalue, plot.type = "q", conf.int.col = NULL, box = TRUE, 
       file = "jpg", memo = "", dpi = 300, file.output = TRUE, verbose = FALSE)
median((result$STAT)^2)/0.455

#subset significant region for LocusZoom----------------------------------------
# find min p-value
subset(pvalue, P == min(P))

# Yang_def:find another site--------------------------------------
Yang_def_locus <- subset(pvalue, P < 1E-5)

# Yang_def subset region
locus <- subset(pvalue, CHR ==  & BP <  + 400000 & BP >  - 400000)
write.table(locus, "locus__.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)
# Yin_def:find another site---------------------------------------
Yin_def_locus <- subset(pvalue, P < 1E-5)

#Yin_def subset region
#Yin_def chr 1
locus <- subset(pvalue, CHR == 1 & BP < 88843951 + 400000 & BP > 88843951 - 400000)
write.table(locus, "locus_1_rs994452.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 1 & BP < 88847231 + 400000 & BP > 88847231 - 400000)
write.table(locus, "locus_1_rs449198.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 1 & BP < 88853812 + 400000 & BP > 88853812 - 400000)
write.table(locus, "locus_1_rs448483.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 1 & BP < 88864082 + 400000 & BP > 88864082 - 400000)
write.table(locus, "locus_1_rs4656043.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)
#Yin_def chr 2

locus <- subset(pvalue, CHR == 2 & BP < 121306440 + 400000 & BP > 121306440 - 400000)
write.table(locus, "locus_2_rs17050272.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

#Yin_def chr 8
locus <- subset(pvalue, CHR == 8 & BP < 3103110 + 400000 & BP > 3103110 - 400000)
write.table(locus, "locus_8_rs4875411.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

#Error: SNP affx-32045388 not recognized as SNP
locus <- subset(pvalue, CHR == 8 & BP < 3117884 + 400000 & BP > 3117884 - 400000)
write.table(locus, "locus_8_Affx-32045388.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 8 & BP < 5250660 + 400000 & BP > 5250660 - 400000)
write.table(locus, "locus_8_rs10101849.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

#Yin_def chr 17
locus <- subset(pvalue, CHR == 17 & BP < 6168235 + 400000 & BP > 6168235 - 400000)
write.table(locus, "locus_17_rs1811229.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 17 & BP < 6169183 + 400000 & BP > 6169183 - 400000)
write.table(locus, "locus_17_rs56987007.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

#Yin_def chr 18
locus <- subset(pvalue, CHR == 18 & BP < 20188276 + 400000 & BP > 20188276 - 400000)
write.table(locus, "locus_18_rs4378711.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 18 & BP < 20205437 + 400000 & BP > 20205437 - 400000)
write.table(locus, "locus_18_rs12326842.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 18 & BP < 20219787 + 400000 & BP > 20219787 - 400000)
write.table(locus, "locus_18_rs34241323.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 18 & BP < 20223695 + 400000 & BP > 20223695 - 400000)
write.table(locus, "locus_18_rs11661542.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

#Yin_def chr 21
locus <- subset(pvalue, CHR == 21 & BP < 33203899 + 400000 & BP > 33203899 - 400000)
write.table(locus, "locus_21_rs56269846.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 21 & BP < 33223437 + 400000 & BP > 33223437 - 400000)
write.table(locus, "locus_21_rs80147001.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)


# Phlegm_stasis:find another site------------------------------------
Phlegm_stasis_locus <- subset(pvalue, P < 1E-5)

# Phlegm_stasis subset region
locus <- subset(pvalue, CHR == 2 & BP < 4354172 + 400000 & BP > 4354172 - 400000)
write.table(locus, "locus_2_rs58835236.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 4 & BP < 20895013 + 400000 & BP > 20895013 - 400000)
write.table(locus, "locus_4_rs7670834.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)
#資料匯出-----------------------------------------------------------------------
write.table(GWAS_cons, file = "C:\\GWAS\\GWAS_cons.txt",sep = "\t",row.names = F,quote = F)
write.table(rm.col_same,file = "C:\\GWAS\\list.txt",sep = "\t",row.names = F,quote = F)
write.table(covar_cons_sex_age,file = "C:\\GWAS\\covar_cons.txt",sep = "\t",row.names = F,quote = F)

write.xlsx(Yang_def_locus, file = "C:\\GWAS\\Yang_def_locus.xlsx")
write.xlsx(Yin_def_locus, file = "C:\\GWAS\\Yin_def_locus.xlsx")
write.xlsx(Phlegm_stasis_locus, file = "C:\\GWAS\\Phlegm_stasis_locus.xlsx")