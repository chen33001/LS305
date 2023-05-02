setwd ("C:\\R\\中醫體質_GWAS\\陽虛_GWAS")
pc  <- read.table("pca.eigenvec", header = TRUE)
covar <-read.table("covar_yang.txt", header = TRUE)
covar_1 <- merge(covar, pc, by = c("FID", "IID"))
covar_2 <- covar_1[, c("FID", "IID","FID","IID","BODY_WEIGHT", 
                       "BMI","BODY_FAT_RATE","BODY_WAISTLINE", 
                       "BODY_BUTTOCKS","WHR","CREATININE","URIC_ACID", paste0("PC", 1:10, sep = ""))]
write.table(covar_2, "Yang_covariate.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)
