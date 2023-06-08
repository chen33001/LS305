library(stringr)
library(dplyr)
library(CMplot)
library(magrittr)

#生菌讀取路徑:C:\\R\\      威甫讀取路徑:C:\\R\\LS305中醫\\
#讀取做GWAS需要的資料----------------------------------------------------------- 
lab_info_input <- read.csv("C:\\R\\LS305中醫\\lab_info.csv",fileEncoding = "big5")
#陰虛
yin_stasis <- read.csv("C:\\Users\\User\\Desktop\\傾向分數估計\\陰虛傾向分數估計.csv",fileEncoding = "big5")
#陽虛
yang_stasis <- read.csv("C:\\Users\\User\\Desktop\\傾向分數估計\\陽虛傾向分數估計.csv",fileEncoding = "big5")
#痰盂
Phlegm_stasis <- read.csv("C:\\Users\\User\\Desktop\\傾向分數估計\\痰盂傾向分數估計.csv",fileEncoding = "big5")

#清洗做GWAS需要的資料，把序號對應成TWB編號-----------------------------------------------------------
lab_info_wash <- lab_info_input %>% 
                  rename(Release_No = 嚜燎elease_No ) %>% 
                  subset(select=c("Release_No","TWB1_ID","FOLLOW")) %>% 
                  subset(FOLLOW=="Baseline") %>%
                  data.table::data.table()
lab_info_input <- lab_info_wash[grepl('TWB',TWB1_ID)]

#製作陰虛的list.txt資料---------------------------------------------------------
#陰虛
yin_stasis_list <- yin_stasis %<>%
                  merge(lab_info_input, by = "Release_No", all.x = T) 


yin_stasis_list  <-  subset(yin_stasis_list,select=c("TWB1_ID","TWB1_ID"))  
write.table(yin_stasis_list,file='C:\\Users\\User\\Desktop\\傾向分數估計\\list_yin.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

#製作YIN_GWAS_cons.txt
YIN_GWAS_cons <- yin_stasis %>%
                    subset(select=c("TWB1_ID","TWB1_ID","Yin_def"))
#改欄位名稱
colnames(YIN_GWAS_cons) <- c("FID","IID","Yin_def") 
#改Yin_def的數值0->1, 1->2
YIN_GWAS_cons$Yin_def[which(YIN_GWAS_cons$Yin_def=="1")] <- 2 
YIN_GWAS_cons$Yin_def[which(YIN_GWAS_cons$Yin_def=="0")] <- 1 
write.table(YIN_GWAS_cons,file='C:\\Users\\User\\Desktop\\傾向分數估計\\YIN_GWAS_cons.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

#製作covar_yin.txt資料
covar_yin  <- yin_stasis %>%
              subset(select=c("TWB1_ID","TWB1_ID","BODY_WEIGHT", 
                              "BMI","BODY_FAT_RATE","BODY_WAISTLINE", 
                              "BODY_BUTTOCKS","WHR","CREATININE","URIC_ACID"))
#改欄位名稱
colnames(covar_yin) <- c("FID","IID","BODY_WEIGHT", 
                         "BMI","BODY_FAT_RATE","BODY_WAISTLINE", 
                         "BODY_BUTTOCKS","WHR","CREATININE","URIC_ACID") 
write.table(covar_yin,file='C:\\Users\\user\\Desktop\\傾向分數估計\\covar_yin.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

#製作陽虛的list.txt資料---------------------------------------------------------------------
#陽虛
yang_stasis_list <- yang_stasis %<>%
                    merge(lab_info_input, by = "Release_No", all.x = T) 


yang_stasis_list  <-  subset(yang_stasis_list,select=c("TWB1_ID","TWB1_ID"))  
write.table(yang_stasis_list,file='C:\\Users\\User\\Desktop\\傾向分數估計\\list_yang.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

#製作YANG_GWAS_cons.txt
YANG_GWAS_cons <- yang_stasis %>%
  subset(select=c("TWB1_ID","TWB1_ID","Yang_def"))
#改欄位名稱
colnames(YANG_GWAS_cons) <- c("FID","IID","Yang_def") 
#改YANG_def的數值0->1, 1->2
YANG_GWAS_cons$Yang_def[which(YANG_GWAS_cons$Yang_def=="1")] <- 2 
YANG_GWAS_cons$Yang_def[which(YANG_GWAS_cons$Yang_def=="0")] <- 1 
write.table(YANG_GWAS_cons,file='C:\\Users\\User\\Desktop\\傾向分數估計\\YANG_GWAS_cons.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

#製作covar_yang.txt資料
covar_yang  <- yang_stasis %>%
  subset(select=c("TWB1_ID","TWB1_ID","BODY_WEIGHT", 
                  "BMI","BODY_FAT_RATE","BODY_WAISTLINE", 
                  "BODY_BUTTOCKS","WHR","CREATININE","URIC_ACID"))
#改欄位名稱
colnames(covar_yang) <- c("FID","IID","BODY_WEIGHT", 
                         "BMI","BODY_FAT_RATE","BODY_WAISTLINE", 
                         "BODY_BUTTOCKS","WHR","CREATININE","URIC_ACID") 
write.table(covar_yang,file='C:\\Users\\user\\Desktop\\傾向分數估計\\covar_yang.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 


#製作痰盂的list.txt資料--------------------------------------------------------------
Phlegm_stasis_list <- Phlegm_stasis %<>%
                      merge(lab_info_input, by = "Release_No", all.x = T) 


Phlegm_stasis_list  <-  subset(Phlegm_stasis_list,select=c("TWB1_ID","TWB1_ID"))  
write.table(Phlegm_stasis_list,file='C:\\Users\\User\\Desktop\\傾向分數估計\\list_Phlegm.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

#製作Phlegm_GWAS_cons.txt
Phlegm_GWAS_cons <- Phlegm_stasis %>%
                    subset(select=c("TWB1_ID","TWB1_ID","Phlegm_stasis"))
#改欄位名稱
colnames(Phlegm_GWAS_cons) <- c("FID","IID","Phlegm_def") 
#改Phlegm_def的數值0->1, 1->2
Phlegm_GWAS_cons$Phlegm_def[which(Phlegm_GWAS_cons$Phlegm_def=="1")] <- 2 
Phlegm_GWAS_cons$Phlegm_def[which(Phlegm_GWAS_cons$Phlegm_def=="0")] <- 1 
write.table(Phlegm_GWAS_cons,file='C:\\Users\\User\\Desktop\\傾向分數估計\\Phlegm_GWAS_cons.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

#製作covar_Phlegm.txt資料
covar_Phlegm  <- Phlegm_stasis %>%
  subset(select=c("TWB1_ID","TWB1_ID","BODY_WEIGHT", 
                  "BMI","BODY_FAT_RATE","BODY_WAISTLINE", 
                  "BODY_BUTTOCKS","WHR","CREATININE","URIC_ACID"))
#改欄位名稱
colnames(covar_Phlegm) <- c("FID","IID","BODY_WEIGHT", 
                          "BMI","BODY_FAT_RATE","BODY_WAISTLINE", 
                          "BODY_BUTTOCKS","WHR","CREATININE","URIC_ACID") 
write.table(covar_Phlegm,file='C:\\Users\\user\\Desktop\\傾向分數估計\\covar_Phlegm.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

#先去做plink，跑出het檔案後，再回來R跑heterozygosity--------------------------------------------
#做heterozygosity
#陰虛-----------------------------------------------------------------
setwd("C:\\R\\中醫體質_GWAS\\陰虛_GWAS")
het <- read.table("het.het", header = TRUE)
het$meanHet <- (het$N.NM. - het$O.HOM.) / het$N.NM.
upplimit <- mean(het$meanHet) + (3 * sd(het$meanHet))
lowlimit <- mean(het$meanHet) - (3 * sd(het$meanHet))
remove <- het[which(het$meanHet < lowlimit | het$meanHet > upplimit), c("FID", "IID")]
write.table(remove, "fail-het-qc.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = FALSE)

#陽虛----------------------------------------------------
setwd("C:\\R\\中醫體質_GWAS\\陽虛_GWAS")
het <- read.table("het.het", header = TRUE)
het$meanHet <- (het$N.NM. - het$O.HOM.) / het$N.NM.
upplimit <- mean(het$meanHet) + (3 * sd(het$meanHet))
lowlimit <- mean(het$meanHet) - (3 * sd(het$meanHet))
remove <- het[which(het$meanHet < lowlimit | het$meanHet > upplimit), c("FID", "IID")]
write.table(remove, "fail-het-qc.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = FALSE)

#痰盂----------------------------------------------------
setwd("C:\\R\\中醫體質_GWAS\\痰盂_GWAS")
het <- read.table("het.het", header = TRUE)
het$meanHet <- (het$N.NM. - het$O.HOM.) / het$N.NM.
upplimit <- mean(het$meanHet) + (3 * sd(het$meanHet))
lowlimit <- mean(het$meanHet) - (3 * sd(het$meanHet))
remove <- het[which(het$meanHet < lowlimit | het$meanHet > upplimit), c("FID", "IID")]
write.table(remove, "fail-het-qc.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = FALSE)
#做完heterozygosity後，跑plink，做出pca.eigenvec檔案後，再回來做combin_PCA_變數.R---------------------------------------------------

#跑combin_PCA_變數.R------------------------------------
#陰虛------------------------------------------------------------------------------------
setwd ("C:\\R\\中醫體質_GWAS\\陰虛_GWAS")
pc  <- read.table("pca.eigenvec", header = TRUE)
covar <-read.table("covar_yin.txt", header = TRUE)
covar_1 <- merge(covar, pc, by = c("FID", "IID"))
covar_2 <- covar_1[, c("FID", "IID", "FID","IID","BODY_WEIGHT", 
                       "BMI","BODY_FAT_RATE","BODY_WAISTLINE", 
                       "BODY_BUTTOCKS","WHR","CREATININE","URIC_ACID", paste0("PC", 1:10, sep = ""))]
write.table(covar_2, "Yin_covariate.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

#陽虛---------------------------------------------------------------------------------
setwd ("C:\\R\\中醫體質_GWAS\\陽虛_GWAS")
pc  <- read.table("pca.eigenvec", header = TRUE)
covar <-read.table("covar_yang.txt", header = TRUE)
covar_1 <- merge(covar, pc, by = c("FID", "IID"))
covar_2 <- covar_1[, c("FID", "IID","FID","IID","BODY_WEIGHT", 
                       "BMI","BODY_FAT_RATE","BODY_WAISTLINE", 
                       "BODY_BUTTOCKS","WHR","CREATININE","URIC_ACID", paste0("PC", 1:10, sep = ""))]
write.table(covar_2, "Yang_covariate.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

#痰盂----------------------------------------------------------------------------------
setwd ("C:\\R\\中醫體質_GWAS\\痰盂_GWAS")
pc  <- read.table("pca.eigenvec", header = TRUE)
covar <-read.table("covar_Phlegm.txt", header = TRUE)
covar_1 <- merge(covar, pc, by = c("FID", "IID"))
covar_2 <- covar_1[, c("FID", "IID", "FID","IID","BODY_WEIGHT", 
                       "BMI","BODY_FAT_RATE","BODY_WAISTLINE", 
                       "BODY_BUTTOCKS","WHR","CREATININE","URIC_ACID", paste0("PC", 1:10, sep = ""))]
write.table(covar_2, "Phlegm_covariate.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)

#跑完combin_PCA_變數.R--------------------------------------------------------------------------------
#先去做Plink，跑出.logistic檔案後，再回來R作圖-----------------------------------------------------------------------------------
#先去做Plink，跑出.logistic檔案後，再回來R作圖-----------------------------------------------------------------------
#先去做Plink，跑出.logistic檔案後，再回來R作圖-----------------------------------------------------------------------
#很重要說3次

#陰虛作圖資料讀取------------------------------------------

setwd("C:\\R\\中醫體質_GWAS\\陰虛_GWAS")
result_Yin <- read.table("GWAS_PCA_Yin.assoc.logistic", header = TRUE)
pvalue_Yin <- result_Yin[, c("SNP", "CHR", "BP", "P")]

#陰虛作圖-----------------
#Manhattan plot
CMplot(pvalue_Yin, plot.type = "m", LOG10 = TRUE, threshold = 1e-5, chr.den.col = NULL, 
       file = "jpg", dpi = 300, file.output = TRUE, verbose = FALSE)
#QQ plot
CMplot(pvalue_Yin, plot.type = "q", conf.int.col = NULL, box = TRUE, 
       file = "jpg", dpi = 300, file.output = TRUE, verbose = FALSE)

#陽虛作圖資料讀取-------------------------------------------
setwd("C:\\R\\中醫體質_GWAS\\陽虛_GWAS")
result_Yang<- read.table("GWAS_PCA_Yang.assoc.logistic", header = TRUE)
pvalue_Yang <- result_Yang[, c("SNP", "CHR", "BP", "P")]
#陽虛作圖-----------------
#Manhattan plot
CMplot(pvalue_Yang, plot.type = "m", LOG10 = TRUE, threshold = 1e-5, chr.den.col = NULL, 
       file = "jpg", dpi = 300, file.output = TRUE, verbose = FALSE)
#QQ plot
CMplot(pvalue_Yang, plot.type = "q", conf.int.col = NULL, box = TRUE, 
       file = "jpg", dpi = 300, file.output = TRUE, verbose = FALSE)

#痰盂作圖資料讀取-------------------------------------------
setwd("C:\\R\\中醫體質_GWAS\\痰盂_GWAS")
result_Phlegm<- read.table("GWAS_PCA_Phlegm.assoc.logistic", header = TRUE)
pvalue_Phlegm <- result_Phlegm[, c("SNP", "CHR", "BP", "P")]
#痰盂作圖-----------------
#Manhattan plot
CMplot(pvalue_Phlegm, plot.type = "m", LOG10 = TRUE, threshold = 1e-5, chr.den.col = NULL, 
       file = "jpg", dpi = 300, file.output = TRUE, verbose = FALSE)
#QQ plot
CMplot(pvalue_Phlegm, plot.type = "q", conf.int.col = NULL, box = TRUE, 
       file = "jpg", dpi = 300, file.output = TRUE, verbose = FALSE)

#匯出顯著基因-------------------------------------------------------
# Yin_def:find another site---------------------------------------
Yin_def_locus <- subset(pvalue_Yin, P < 1E-5)
#Yin_def subset region
#迴圈
for(i in 1:length(Yin_def_locus$BP)){
  locus <- subset(pvalue_Yin, CHR == Yin_def_locus[i,2] & BP < Yin_def_locus[i,3] + 400000 & BP > Yin_def_locus[i,3] - 400000)
  write.table(locus, sprintf("locus_%d_%s.txt",Yin_def_locus[i,2] ,Yin_def_locus[i,1]), append = FALSE, quote = FALSE, sep = "\t", 
              row.names = FALSE, col.names = TRUE)
}

# Yang_def:find another site--------------------------------------
Yang_def_locus <- subset(pvalue_Yang, P < 1E-5)
# Yang_def subset region
#迴圈
for(i in 1:length(Yang_def_locus$BP)){
  locus <- subset(pvalue_Yang, CHR == Yang_def_locus[i,2] & BP < Yang_def_locus[i,3] + 400000 & BP > Yang_def_locus[i,3] - 400000)
  write.table(locus, sprintf("locus_%d_%s.txt",Yang_def_locus[i,2] ,Yang_def_locus[i,1]), append = FALSE, quote = FALSE, sep = "\t", 
              row.names = FALSE, col.names = TRUE)
}


# Phlegm_stasis:find another site------------------------------------
Phlegm_stasis_locus <- subset(pvalue_Phlegm, P < 1E-5)

# Phlegm_stasis subset region

#迴圈
for(i in 1:length(Phlegm_stasis_locus$BP)){
  locus <- subset(pvalue_Phlegm, CHR == Phlegm_stasis_locus[i,2] & BP < Phlegm_stasis_locus[i,3] + 400000 & BP > Phlegm_stasis_locus[i,3] - 400000)
  write.table(locus, sprintf("locus_%d_%s.txt",Phlegm_stasis_locus[i,2] ,Phlegm_stasis_locus[i,1]), append = FALSE, quote = FALSE, sep = "\t", 
              row.names = FALSE, col.names = TRUE)
}

