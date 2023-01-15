library(stringr)
library(dplyr)
library(CMplot)
library(openxlsx)
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

# Yin_def:find another site---------------------------------------
Yin_def_locus <- subset(pvalue, P < 1E-5)

#Yin_def subset region

locus <- subset(pvalue, CHR == 1 & BP < 181508326 + 400000 & BP > 181508326 - 400000)
write.table(locus, "locus_1_rs6681017.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)


locus <- subset(pvalue, CHR == 1 & BP < 181509821 + 400000 & BP > 181509821 - 400000)
write.table(locus, "locus_1_rs6685078.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)


locus <- subset(pvalue, CHR == 2 & BP < 121306440 + 400000 & BP > 121306440 - 400000)
write.table(locus, "locus_2_rs17050272.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)


locus <- subset(pvalue, CHR == 5 & BP < 168212096 + 400000 & BP > 168212096 - 400000)
write.table(locus, "locus_5_rs10475891.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)


locus <- subset(pvalue, CHR == 10 & BP < 60793767 + 400000 & BP > 60793767 - 400000)
write.table(locus, "locus_10_rs10826267.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)


locus <- subset(pvalue, CHR == 10 & BP < 132345848 + 400000 & BP > 132345848 - 400000)
write.table(locus, "locus_10_rs1334910.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)


locus <- subset(pvalue, CHR == 14 & BP < 70839635 + 400000 & BP > 70839635 - 400000)
write.table(locus, "locus_14_rs3742916.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)


locus <- subset(pvalue, CHR == 17 & BP < 2859272 + 400000 & BP > 2859272 - 400000)
write.table(locus, "locus_17_rs8077217.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)


# Yang_def:find another site--------------------------------------
Yang_def_locus <- subset(pvalue, P < 1E-5)
# Yang_def subset region

locus <- subset(pvalue, CHR == 2 & BP < 237554988 + 400000 & BP > 237554988 - 400000)
write.table(locus, "locus_2_rs1796444.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 8 & BP < 96221214 + 400000 & BP > 96221214 - 400000)
write.table(locus, "locus_8_rs880642.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 11 & BP < 92605599 + 400000 & BP > 92605599 - 400000)
write.table(locus, "locus_11_rs77906267.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 13 & BP < 94870765 + 400000 & BP > 94870765 - 400000)
write.table(locus, "locus_13_rs1572050.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 15 & BP < 38847023 + 400000 & BP > 38847023 - 400000)
write.table(locus, "locus_15_rs117840856.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 17 & BP < 10143039 + 400000 & BP > 10143039 - 400000)
write.table(locus, "locus_17_rs6503300.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)


locus <- subset(pvalue, CHR == 22 & BP < 26416570 + 400000 & BP > 26416570 - 400000)
write.table(locus, "locus_22_rs1033589.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)


# Phlegm_stasis:find another site------------------------------------
Phlegm_stasis_locus <- subset(pvalue, P < 1E-5)

# Phlegm_stasis subset region
locus <- subset(pvalue, CHR == 5 & BP < 168212096 + 400000 & BP > 168212096 - 400000)
write.table(locus, "locus_5_rs10475891.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 8 & BP < 94168269 + 400000 & BP > 94168269 - 400000)
write.table(locus, "locus_8_rs11994583.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 8 & BP < 94169379 + 400000 & BP > 94169379 - 400000)
write.table(locus, "locus_8_rs278568.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 10 & BP < 115381747 + 400000 & BP > 115381747 - 400000)
write.table(locus, "locus_10_rs868738.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 14 & BP < 60209188 + 400000 & BP > 60209188 - 400000)
write.table(locus, "locus_14_rs10138254.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

locus <- subset(pvalue, CHR == 22 & BP < 26416570 + 400000 & BP > 26416570 - 400000)
write.table(locus, "locus_22_rs1033589.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

#資料匯出-----------------------------------------------------------------------
write.table(GWAS_cons, file = "C:\\R\\LS305中醫\\GWAS_cons.txt",sep = "\t",row.names = F,quote = F)
write.table(rm.col_same,file = "C:\\R\\LS305中醫\\list.txt",sep = "\t",row.names = F,quote = F)
write.table(covar_cons_sex_age,file = "C:\\R\\LS305中醫\\covar_cons.txt",sep = "\t",row.names = F,quote = F)

write.xlsx(Yin_def_locus, file = "C:\\R\\GWAS\\Yin_def_locus.xlsx")
write.xlsx(Yang_def_locus, file = "C:\\R\\GWAS\\Yang_def_locus.xlsx")
write.xlsx(Phlegm_stasis_locus, file = "C:\\R\\GWAS\\Phlegm_stasis_locus.xlsx")