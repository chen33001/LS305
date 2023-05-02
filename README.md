# Genome-wide Association Studies

Select: ★★★★★

## R

1.lab_info ⇒ 篩出release_No 和 TWB1 都有的

1. 由1.的資料合併TCM_Anova找有中醫的release_No
2. 根據陰虛、陽虛、痰盂不同體質做傾向分數配對
- 4.做GWAS需要的資料
    
    (1)需要TWB1.hg19.bed、TWB1.hg19.bim、TWB1.hg19.fam 檔案，這是基因的資訊，[這3個檔案需要去老師的dropbox上下載]
    
    - (2)做list_yin.txt、list_yang.txt、list_Phlegm.txt，(包含兩行TWB1_ID)
        
        [list_yin.txt](Genome-wide%20Association%20Studies%203dfabf94d5ce44e4be8ab0e8d02c8ecc/list_yin.txt)
        
        [list_yang.txt](Genome-wide%20Association%20Studies%203dfabf94d5ce44e4be8ab0e8d02c8ecc/list_yang.txt)
        
        [list_Phlegm.txt](Genome-wide%20Association%20Studies%203dfabf94d5ce44e4be8ab0e8d02c8ecc/list_Phlegm.txt)
        
    - (3)做YIN_GWAS_cons.txt、YANG_GWAS_cons.txt、Phlegm_GWAS_cons.txt，(包含FID、	IID、[(yin_def)或是(yang_def)或是(Phlegm_def)]
        
        [yang_GWAS_cons.txt](Genome-wide%20Association%20Studies%203dfabf94d5ce44e4be8ab0e8d02c8ecc/yang_GWAS_cons.txt)
        
        [Phlegm_GWAS_cons.txt](Genome-wide%20Association%20Studies%203dfabf94d5ce44e4be8ab0e8d02c8ecc/Phlegm_GWAS_cons.txt)
        
        [YIN_GWAS_cons.txt](Genome-wide%20Association%20Studies%203dfabf94d5ce44e4be8ab0e8d02c8ecc/YIN_GWAS_cons.txt)
        
    - (4)做covar_yiin.txt、covar_yang.txt、covar_Phlegm.txt(包含FID、IID、以及欲觀察的性狀)
        
        [covar_yin.txt](Genome-wide%20Association%20Studies%203dfabf94d5ce44e4be8ab0e8d02c8ecc/covar_yin.txt)
        
        [covar_yang.txt](Genome-wide%20Association%20Studies%203dfabf94d5ce44e4be8ab0e8d02c8ecc/covar_yang.txt)
        
        [covar_Phlegm.txt](Genome-wide%20Association%20Studies%203dfabf94d5ce44e4be8ab0e8d02c8ecc/covar_Phlegm.txt)
        
    
    再來去CMD，跑Plink。
    

## Plink

中醫體質 Plink 流程  跑出logistic檔案

需要TWB1.hg19.bed、TWB1.hg19.bim、TWB1.hg19.fam 檔案，這是基因的資訊

**Plink 需要做3次，陰虛、陽虛、痰盂**

### 陰虛

```powershell
cd /d C:\R\中醫體質_GWAS\陰虛_GWAS
plink --bfile TWB1.hg19 --keep list_yin.txt --pheno YIN_GWAS_cons.txt --pheno-name Yin_def --make-bed --out TWB1_pheno
plink --bfile TWB1_pheno --mind 0.05 --make-bed --out qc1
plink --bfile qc1 --het --out het
##heterozygosity.r
plink --bfile qc1 --remove fail-het-qc.txt --make-bed --out qc2
plink --bfile qc2 --indep-pairwise 250 5 0.2 --exclude range long.range.LD.region.txt --out indep
plink --bfile qc2 --extract indep.prune.in --genome --min 0.1875 --out related  #take long time
plink --bfile qc2 --remove related.genome --make-bed --out qc3
plink --bfile qc3 --geno 0.05 --maf 0.05 --hwe 0.00001 --make-bed --out qc-final

#PCA --> 10PCs
#plink --bfile qc-final --indep-pairwise 250 5 0.2 --exclude range long.range.LD.region.txt --out indep
plink --bfile qc-final --extract indep.prune.in --pca header --out pca
#R code combin covariate and 10 PCs (combin_PCA.R --> cons_covariate.txt)
plink --bfile qc-final --logistic  hide-covar --covar cons_covariate.txt --covar-name BODY_WEIGHT,BMI,BODY_FAT_RATE,BODY_WAISTLINE,BODY_BUTTOCKS,WHR,CREATININE,URIC_ACID,PC1-PC10 --out GWAS_PCA_Yin

#without PCA
plink --bfile qc-final --covar covar_yin.txt --covar-name BODY_WEIGHT,BMI,BODY_FAT_RATE,BODY_WAISTLINE,BODY_BUTTOCKS,WHR,CREATININE,URIC_ACID --logistic --out GWAS_Yin

plink --bfile qc-final --logistic hide-covar --covar cons_covariate.txt --covar-name BODY_WEIGHT,BMI,BODY_FAT_RATE,BODY_WAISTLINE,BODY_BUTTOCKS,WHR,CREATININE,URIC_ACID,PC1-PC10 --out Yin_def_result
```

## 陽虛

```powershell
cd /d C:\R\中醫體質_GWAS\陽虛_GWAS
plink --bfile TWB1.hg19 --keep list_yang.txt --pheno yang_GWAS_cons.txt --pheno-name Yang_def --make-bed --out TWB1_pheno
plink --bfile TWB1_pheno --mind 0.05 --make-bed --out qc1
plink --bfile qc1 --het --out het
##heterozygosity.r
plink --bfile qc1 --remove fail-het-qc.txt --make-bed --out qc2
plink --bfile qc2 --indep-pairwise 250 5 0.2 --exclude range long.range.LD.region.txt --out indep
plink --bfile qc2 --extract indep.prune.in --genome --min 0.1875 --out related  #take long time
plink --bfile qc2 --remove related.genome --make-bed --out qc3
plink --bfile qc3 --geno 0.05 --maf 0.05 --hwe 0.00001 --make-bed --out qc-final

#PCA --> 10PCs
#plink --bfile qc-final --indep-pairwise 250 5 0.2 --exclude range long.range.LD.region.txt --out indep
plink --bfile qc-final --extract indep.prune.in --pca header --out pca
#R code combin covariate and 10 PCs (combin_PCA.R --> cons_covariate.txt)
plink --bfile qc-final --logistic  hide-covar --covar cons_covariate.txt --covar-name BODY_WEIGHT,BMI,BODY_FAT_RATE,BODY_WAISTLINE,BODY_BUTTOCKS,WHR,CREATININE,URIC_ACID,PC1-PC10 --out GWAS_PCA_Yang

#without PCA
plink --bfile qc-final --covar covar_yang.txt --covar-name BODY_WEIGHT,BMI,BODY_FAT_RATE,BODY_WAISTLINE,BODY_BUTTOCKS,WHR,CREATININE,URIC_ACID --logistic --out GWAS_Yang

plink --bfile qc-final --logistic hide-covar --covar cons_covariate.txt --covar-name BODY_WEIGHT,BMI,BODY_FAT_RATE,BODY_WAISTLINE,BODY_BUTTOCKS,WHR,CREATININE,URIC_ACID,PC1-PC10 --out Yang_def_result
```

## 痰盂

```powershell
cd /d C:\R\中醫體質_GWAS\痰盂_GWAS
plink --bfile TWB1.hg19 --keep list_Phlegm.txt --pheno Phlegm_GWAS_cons.txt --pheno-name Phlegm_def --make-bed --out TWB1_pheno
plink --bfile TWB1_pheno --mind 0.05 --make-bed --out qc1
plink --bfile qc1 --het --out het
##heterozygosity.r
plink --bfile qc1 --remove fail-het-qc.txt --make-bed --out qc2
plink --bfile qc2 --indep-pairwise 250 5 0.2 --exclude range long.range.LD.region.txt --out indep
plink --bfile qc2 --extract indep.prune.in --genome --min 0.1875 --out related  #take long time
plink --bfile qc2 --remove related.genome --make-bed --out qc3
plink --bfile qc3 --geno 0.05 --maf 0.05 --hwe 0.00001 --make-bed --out qc-final

#PCA --> 10PCs
#plink --bfile qc-final --indep-pairwise 250 5 0.2 --exclude range long.range.LD.region.txt --out indep
plink --bfile qc-final --extract indep.prune.in --pca header --out pca
#R code combin covariate and 10 PCs (combin_PCA.R --> cons_covariate.txt)
plink --bfile qc-final --logistic  hide-covar --covar cons_covariate.txt --covar-name BODY_WEIGHT,BMI,BODY_FAT_RATE,BODY_WAISTLINE,BODY_BUTTOCKS,WHR,CREATININE,URIC_ACID,PC1-PC10 --out GWAS_PCA_Phlegm

#without PCA
plink --bfile qc-final --covar covar_yin.txt --covar-name BODY_WEIGHT,BMI,BODY_FAT_RATE,BODY_WAISTLINE,BODY_BUTTOCKS,WHR,CREATININE,URIC_ACID --logistic --out GWAS_Phlegm

plink --bfile qc-final --logistic hide-covar --covar cons_covariate.txt --covar-name BODY_WEIGHT,BMI,BODY_FAT_RATE,BODY_WAISTLINE,BODY_BUTTOCKS,WHR,CREATININE,URIC_ACID,PC1-PC10 --out Phlegm_def_result
```
