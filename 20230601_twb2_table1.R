library(dplyr)

#讀取資料
PSM_yin <- read.csv("C:\\Github\\LS305\\Table1資料\\20230601Table1\\twb2\\20230601_twb2_陰虛傾向分數估計.csv")
PSM_yang <- read.csv("C:\\Github\\LS305\\Table1資料\\20230601Table1\\twb2\\20230601_twb2_陽虛傾向分數估計.csv")
PSM_Phlegm <- read.csv("C:\\Github\\LS305\\Table1資料\\20230601Table1\\twb2\\20230601_twb2_痰盂傾向分數估計.csv")
survey_tcm <- read.csv("C:\\Github\\LS305\\Table1資料\\survey_tcm.csv")

#資料配對&清洗
#陰虛
Table1_yin <- merge(PSM_yin ,survey_tcm, by = "Release_No", all.survey_tcm = T) %>%
  subset(., select = c(-X, -Yin_def.y, -distance, -weights, -subclass, -AGE.y, -SEX.y  )) %>%
  rename(Yin_def = Yin_def.x, AGE = AGE.x, SEX = SEX.x) 

#陰虛修改欄位順序
yin_cols <- colnames(Table1_yin)
yin_new_cols <- c(yin_cols[1], yin_cols[2], yin_cols[5], yin_cols[6], yin_cols[3:4], yin_cols[7:17])
Table1_yin <- Table1_yin[,yin_new_cols]


#陽虛
Table1_yang <- merge(PSM_yang ,survey_tcm, by = "Release_No", all.survey_tcm = T) %>%
  subset(., select = c(-X, -Yang_def.y, -distance, -weights, -subclass, -AGE.y, -SEX.y   )) %>%
  rename(Yang_def = Yang_def.x, AGE = AGE.x, SEX = SEX.x)

#陽虛修改欄位順序
yang_cols <- colnames(Table1_yang)
yang_new_cols <- c(yang_cols[1], yang_cols[2], yang_cols[5], yang_cols[6], yang_cols[3:4],yang_cols[7:17])
Table1_yang <- Table1_yang[, yang_new_cols]

#痰盂
Table1_Phlegm <- merge(PSM_Phlegm ,survey_tcm, by = "Release_No", all.survey_tcm = T) %>%
  subset(., select = c(-X, -Phlegm_stasis.y, -distance, -weights, -subclass, -AGE.y, -SEX.y  )) %>%
  rename(Phlegm_stasis = Phlegm_stasis.x, AGE = AGE.x, SEX = SEX.x)

#痰盂修改欄位順序
Phlegm_cols <- colnames(Table1_Phlegm)
Phlegm_new_cols <- c(Phlegm_cols[1], Phlegm_cols[2], Phlegm_cols[5], Phlegm_cols[6], Phlegm_cols[3:4], Phlegm_cols[7:17])
Table1_Phlegm <- Table1_Phlegm[, Phlegm_new_cols]


#匯出Table1 excel
write.csv(Table1_yin,file='C:\\Github\\LS305\\Table1資料\\20230601Table1\\twb2\\20230601_twb2_Table1need_yin.csv',fileEncoding = "Big5")
write.csv(Table1_yang,file='C:\\Github\\LS305\\Table1資料\\20230601Table1\\twb2\\20230601_twb2_Table1need_yang.csv',fileEncoding = "Big5")
write.csv(Table1_Phlegm,file='C:\\Github\\LS305\\Table1資料\\20230601Table1\\twb2\\20230601_twb2_Table1need_Phlegm.csv',fileEncoding = "Big5")


#上面跑完後Global Environment可以先清空了--------------------------------------------------------------------------------------

#survey
setwd("C:/Github/LS305/Table1資料/20230601Table1/twb2")
#用github上別人的程式碼
source("C:\\Github\\LS305\\statisticaltests.R")
Table1need_yin <- read.csv("C:\\Github\\LS305\\Table1資料\\20230601Table1\\twb2\\20230601_twb2_Table1need_yin.csv")
Table1need_yang <- read.csv("C:\\Github\\LS305\\Table1資料\\20230601Table1\\twb2\\20230601_twb2_Table1need_yang.csv")
Table1need_Phlegm <- read.csv("C:\\Github\\LS305\\Table1資料\\20230601Table1\\twb2\\20230601_twb2_Table1need_Phlegm.csv")


yin_statistics <- statisticaltests(Table1need_yin,'yin',T)
yang_statistics <- statisticaltests(Table1need_yang,'yang',T)
Phlegm_statistics <- statisticaltests(Table1need_Phlegm,'Phlegm',T)






