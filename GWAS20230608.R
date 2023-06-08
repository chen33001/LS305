library(stringr)
library(dplyr)
library(CMplot)
library(magrittr)

#生菌讀取路徑:C:\\R\\      威甫讀取路徑:C:\\R\\LS305中醫\\
#讀取做GWAS需要的資料----------------------------------------------------------- 
lab_info_input <- read.csv("C:\\R\\LS305中醫\\lab_info.csv",fileEncoding = "big5")
#陰虛
yin_stasis <- read.csv("C:\\Users\\User\\Desktop\\傾向分數估計\\20230601_PSM\\20230601_twb1_陰虛傾向分數估計.csv",fileEncoding = "big5")
#陽虛
yang_stasis <- read.csv("C:\\Users\\User\\Desktop\\傾向分數估計\\20230601_PSM\\20230601_twb1_陽虛傾向分數估計.csv",fileEncoding = "big5")
#痰盂
Phlegm_stasis <- read.csv("C:\\Users\\User\\Desktop\\傾向分數估計\\20230601_PSM\\20230601_twb1_痰盂傾向分數估計.csv",fileEncoding = "big5")

#清洗做GWAS需要的資料，把序號對應成TWB編號-----------------------------------------------------------
lab_info_wash <- lab_info_input %>% 
                  rename(Release_No = 嚜燎elease_No ) %>% 
                  subset(select=c("Release_No","TWB1_ID","FOLLOW")) %>% 
                  subset(FOLLOW=="Baseline") %>%
                  data.table::data.table() 
  
lab_info_input <- lab_info_wash[grepl('TWB',TWB1_ID)]

#製作陰虛的list.txt資料---------------------------------------------------------
#陰虛
yin_stasis_list <- yin_stasis %>%
                    merge(lab_info_input, by = "Release_No", all.x = T) %>%
                    subset(.,select=c("TWB1_ID","TWB1_ID"))

write.table(yin_stasis_list,file='C:\\Users\\User\\Desktop\\傾向分數估計\\20230601_PSM\\list_yin.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

#製作YIN_GWAS_cons.txt
YIN_GWAS_cons <- yin_stasis %>%
                merge(lab_info_input, by = "Release_No", all.x = T) %>%
                subset(select=c("TWB1_ID","TWB1_ID","Yin_def")) %>%
                rename(FID = TWB1_ID, IID = TWB1_ID.1)
#改Yin_def的數值0->1, 1->2
YIN_GWAS_cons$Yin_def[which(YIN_GWAS_cons$Yin_def=="1")] <- 2 
YIN_GWAS_cons$Yin_def[which(YIN_GWAS_cons$Yin_def=="0")] <- 1 
write.table(YIN_GWAS_cons,file='C:\\Users\\User\\Desktop\\傾向分數估計\\20230601_PSM\\YIN_GWAS_cons.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5")

#製作covar_yin.txt資料
covar_yin  <- yin_stasis %>%
              merge(lab_info_input, by = "Release_No", all.x = T) %>%
              subset(select=c("TWB1_ID","TWB1_ID","Yin_def","AGE", "SEX")) %>%
              rename(FID = TWB1_ID, IID = TWB1_ID.1)

write.table(covar_yin,file='C:\\Users\\user\\Desktop\\傾向分數估計\\20230601_PSM\\covar_yin.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 



