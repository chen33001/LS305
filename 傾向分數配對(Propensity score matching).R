library(MatchIt)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(data.table)

#資料讀取
TCM_Anova <- read.csv("C:\\R\\LS305中醫\\TCM_Anova.csv",fileEncoding = "big5")
lab_info_input <- read.csv("C:\\R\\LS305中醫\\lab_info.csv",fileEncoding = "big5")

#資料清洗
names(lab_info_input)[1] <- "Release_No"
lab_info_wash <- lab_info_input %>% 
                      subset(select=c("Release_No","TWB1_ID","FOLLOW")) %>% 
                      subset(FOLLOW=="Baseline") %>%
                      data.table()
lab_info <- lab_info_wash[grepl('TWB',TWB1_ID)]
PSM <- merge(lab_info,TCM_Anova)
PSM <-subset(PSM, select = c(-X))
PSM <- as.data.frame(PSM)



#欲觀察的變數
PSM_COV <- c("BODY_WEIGHT","BMI","BODY_FAT_RATE","BODY_WAISTLINE","BODY_BUTTOCKS","WHR",
             "CREATININE","URIC_ACID")

#第1步Difference-in-means: pre-treatment covariates
#陰虛
PSM %>%
  group_by(Yin_def) %>%
  select(one_of(PSM_COV)) %>%
  summarise_all(list(~mean(., na.rm = T)))

lapply(PSM_COV, function(v) {
  t.test(PSM[ ,v] ~ PSM[ ,'Yin_def'])
})
#陽虛
PSM %>%
  group_by(Yang_def) %>%
  select(one_of(PSM_COV)) %>%
  summarise_all(list(~mean(., na.rm = T)))

lapply(PSM_COV, function(v) {
  t.test(PSM[, v] ~ PSM[, 'Yang_def'])
})

#痰盂

PSM %>%
  group_by(Phlegm_stasis) %>%
  select(one_of(PSM_COV)) %>%
  summarise_all(list(~mean(., na.rm = T)))

lapply(PSM_COV, function(v) {
  t.test(PSM[, v] ~ PSM[, 'Phlegm_stasis'])
})

#第2步傾向得分估計
#陰虛
m_ps1 <- glm(Yin_def ~ BODY_WEIGHT + BMI + BODY_FAT_RATE + BODY_WAISTLINE + BODY_BUTTOCKS, data = PSM)
summary(m_ps1)

prs_df1 <- data.frame(pr_score = predict(m_ps1, type = "response"),
                      Yin_def = m_ps1$model$Yin_def)
head(prs_df1)

#陽虛
m_ps2 <- glm(Yang_def ~ BODY_WEIGHT + BMI + BODY_FAT_RATE + BODY_WAISTLINE + BODY_BUTTOCKS, data = PSM)
summary(m_ps2)

prs_df2 <- data.frame(pr_score = predict(m_ps2, type = "response"),
                      Yang_def = m_ps2$model$Yang_def)
head(prs_df2)

#痰盂
m_ps3 <- glm(Phlegm_stasis ~ BODY_WEIGHT + BMI + BODY_FAT_RATE + BODY_WAISTLINE + BODY_BUTTOCKS, data = PSM)
summary(m_ps3)

prs_df3 <- data.frame(pr_score = predict(m_ps3, type = "response"),
                      Phlegm_stasis = m_ps3$model$Phlegm_stasis)
head(prs_df3)


#Examining the region of common support
#陰虛
labs <- paste(c("Yin_def", "Not_Yin_def"))
prs_df1 %>%
  mutate(Yin_def = ifelse(Yin_def == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~Yin_def) +
  xlab("Probability of Yin_def") +
  theme_bw()

#陽虛
labs <- paste(c("Yang_def", "Not_Yang_def"))
prs_df2 %>%
  mutate(Yang_def = ifelse(Yang_def == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~Yang_def) +
  xlab("Probability of Yang_def") +
  theme_bw()


#痰盂
labs <- paste(c("Phlegm_stasis", "Not_Phlegm_stasis"))
prs_df3 %>%
  mutate(Phlegm_stasis = ifelse(Phlegm_stasis == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~Phlegm_stasis) +
  xlab("Probability of Phlegm_stasis_def") +
  theme_bw()

#第3步Executing a matching algorithm
#陰虛
PSM_nomiss <- PSM %>%  # MatchIt does not allow missing values
  select(Release_No, Yin_def, one_of(PSM_COV)) %>%
  na.omit()

mod_match1 <- matchit(Yin_def ~ BODY_WEIGHT + BMI + BODY_FAT_RATE + BODY_WAISTLINE + BODY_BUTTOCKS,
                      method = "nearest", data = PSM_nomiss)
dta_m1 <- match.data(mod_match1)
dim(dta_m1)

#陽虛

PSM_nomiss <- PSM %>%  # MatchIt does not allow missing values
  select(Release_No, Yang_def, one_of(PSM_COV)) %>%
  na.omit()

mod_match2 <- matchit(Yang_def ~ BODY_WEIGHT + BMI + BODY_FAT_RATE + BODY_WAISTLINE + BODY_BUTTOCKS,
                      method = "nearest", data = PSM_nomiss)
dta_m2 <- match.data(mod_match2)
dim(dta_m2)

#痰盂

PSM_nomiss <- PSM %>%  # MatchIt does not allow missing values
  select(Release_No, Phlegm_stasis, one_of(PSM_COV)) %>%
  na.omit()

mod_match3 <- matchit(Phlegm_stasis ~ BODY_WEIGHT + BMI + BODY_FAT_RATE + BODY_WAISTLINE + BODY_BUTTOCKS,
                      method = "nearest", data = PSM_nomiss)
dta_m3 <- match.data(mod_match3)
dim(dta_m3)

#第4步Examining covariate balance in the matched sample
#陰虛
fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  if (variable == 'BMI') dta$variable <- dta$variable / 10^3
  dta$Yin_def <- as.factor(dta$Yin_def)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = Yin_def)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

grid.arrange(
  fn_bal(dta_m1, "BODY_WEIGHT"),
  fn_bal(dta_m1, "BMI") + theme(legend.position = "none"),
  fn_bal(dta_m1, "BODY_FAT_RATE"),
  fn_bal(dta_m1, "BODY_WAISTLINE") + theme(legend.position = "none"),
  fn_bal(dta_m1, "BODY_BUTTOCKS"),
  nrow = 3, widths = c(1, 0.8)
)

#陽虛
fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  if (variable == 'BMI') dta$variable <- dta$variable / 10^3
  dta$Yang_def <- as.factor(dta$Yang_def)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = Yang_def)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

grid.arrange(
  fn_bal(dta_m2, "BODY_WEIGHT"),
  fn_bal(dta_m2, "BMI") + theme(legend.position = "none"),
  fn_bal(dta_m2, "BODY_FAT_RATE"),
  fn_bal(dta_m2, "BODY_WAISTLINE") + theme(legend.position = "none"),
  fn_bal(dta_m2, "BODY_BUTTOCKS"),
  nrow = 3, widths = c(1, 0.8)
)

#痰盂
fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  if (variable == 'BMI') dta$variable <- dta$variable / 10^3
  dta$Phlegm_stasis <- as.factor(dta$Phlegm_stasis)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = Phlegm_stasis)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

grid.arrange(
  fn_bal(dta_m3, "BODY_WEIGHT"),
  fn_bal(dta_m3, "BMI") + theme(legend.position = "none"),
  fn_bal(dta_m3, "BODY_FAT_RATE"),
  fn_bal(dta_m3, "BODY_WAISTLINE") + theme(legend.position = "none"),
  fn_bal(dta_m3, "BODY_BUTTOCKS"),
  nrow = 3, widths = c(1, 0.8)
)
#第4.2 Difference-in-means
#陰虛
dta_m1 %>%
  group_by(Yin_def) %>%
  select(one_of(PSM_COV)) %>%
  summarise_all(list(mean))

lapply(PSM_COV, function(v) {
  t.test(dta_m1[, v] ~ dta_m1$Yin_def)
})
#陽虛
dta_m2 %>%
  group_by(Yang_def) %>%
  select(one_of(PSM_COV)) %>%
  summarise_all(list(mean))

lapply(PSM_COV, function(v) {
  t.test(dta_m2[, v] ~ dta_m2$Yang_def)
})
#痰盂
dta_m3 %>%
  group_by(Phlegm_stasis) %>%
  select(one_of(PSM_COV)) %>%
  summarise_all(list(mean))

lapply(PSM_COV, function(v) {
  t.test(dta_m3[, v] ~ dta_m3$Phlegm_stasis)
})
#第5步Estimating treatment effects
#陰虛
with(dta_m1, t.test(BMI ~ Yin_def))

lm_treat1 <- lm(BMI ~ Yin_def, data = dta_m1)
summary(lm_treat1)

lm_treat2 <- lm(BMI ~ Yin_def + BODY_WEIGHT  + BODY_FAT_RATE + BODY_WAISTLINE + BODY_BUTTOCKS, data = dta_m1)
summary(lm_treat2)

#陽虛
with(dta_m2, t.test(BMI ~ Yang_def))

lm_treat1 <- lm(BMI ~ Yang_def, data = dta_m2)
summary(lm_treat1)

lm_treat2 <- lm(BMI ~ Yang_def + BODY_WEIGHT  + BODY_FAT_RATE + BODY_WAISTLINE + BODY_BUTTOCKS, data = dta_m2)
summary(lm_treat2)

#痰盂
with(dta_m3, t.test(BMI ~ Phlegm_stasis))

lm_treat1 <- lm(BMI ~ Phlegm_stasis, data = dta_m3)
summary(lm_treat1)

lm_treat2 <- lm(BMI ~ Phlegm_stasis + BODY_WEIGHT  + BODY_FAT_RATE + BODY_WAISTLINE + BODY_BUTTOCKS, data = dta_m3)
summary(lm_treat2)


write.csv(dta_m1,file='C:\\Users\\user\\Desktop\\傾向分數估計\\陰虛傾向分數估計.csv',fileEncoding = "Big5")
write.csv(dta_m2,file='C:\\Users\\user\\Desktop\\傾向分數估計\\陽虛傾向分數估計.csv',fileEncoding = "Big5")
write.csv(dta_m3,file='C:\\Users\\user\\Desktop\\傾向分數估計\\痰盂傾向分數估計.csv',fileEncoding = "Big5")

#轉換做GWAS需要的資料----------------------------------------------------------- 
lab_info_input <- read.csv("C:\\R\\LS305中醫\\lab_info.csv",fileEncoding = "big5") 
lab_info_input <- subset(lab_info_input, 
                         FOLLOW=="Baseline") 
names(lab_info_input)[1] <- "Release_No" 
#陰虛 
yin_stasis <- read.csv("C:\\Users\\User\\Desktop\\傾向分數估計\\陰虛傾向分數估計.csv",fileEncoding = "big5") 
yin_stasis <- merge(yin_stasis,lab_info_input, by = "Release_No", all.x = T) 

yin_stasis_list  <-  subset(yin_stasis,select=c("TWB1_ID","TWB1_ID"))  
write.table(yin_stasis_list,file='C:\\Users\\User\\Desktop\\傾向分數估計\\list_yin.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

yin_stasis_list <- subset(yin_stasis,select=c("TWB1_ID","TWB1_ID","Yin_def")) 
colnames(yin_stasis_list) <- c("FID","IID","Yin_def") 
yin_stasis_list$Yin_def[which(yin_stasis_list$Yin_def=="1")] <- 2 
yin_stasis_list$Yin_def[which(yin_stasis_list$Yin_def=="0")] <- 1 
write.table(yin_stasis_list,file='C:\\Users\\User\\Desktop\\傾向分數估計\\YIN_GWAS_cons.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

covar_yin  <-   subset(yin_stasis,select=c("TWB1_ID","TWB1_ID","BODY_WEIGHT", 
                                           "BMI","BODY_FAT_RATE","BODY_WAISTLINE", 
                                           "BODY_BUTTOCKS","WHR","CREATININE","URIC_ACID")) 
colnames(covar_yin) <- c("FID","IID","BODY_WEIGHT", 
                         "BMI","BODY_FAT_RATE","BODY_WAISTLINE", 
                         "BODY_BUTTOCKS","WHR","CREATININE","URIC_ACID") 
write.table(covar_yin,file='C:\\GWAS\\covar_yin.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 
#陽虛 
yang_stasis <- read.csv("C:\\Users\\User\\Desktop\\傾向分數估計\\陽虛傾向分數估計.csv",fileEncoding = "big5") 

yang_stasis <- merge(yang_stasis,lab_info_input, by = "Release_No", all.x = T) 

yang_stasis_list  <-  subset(yang_stasis,select=c("TWB1_ID","TWB1_ID"))  
write.table(yang_stasis_list,file='C:\\Users\\User\\Desktop\\傾向分數估計\\list_yang.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

yang_stasis_list <- subset(yang_stasis,select=c("TWB1_ID","TWB1_ID","Yang_def")) 
colnames(yang_stasis_list) <- c("FID","IID","yang_def") 
yang_stasis_list$yang_def[which(yang_stasis_list$yang_def=="1")] <- 2 
yang_stasis_list$yang_def[which(yang_stasis_list$yang_def=="0")] <- 1 
write.table(yang_stasis_list,file='C:\\Users\\User\\Desktop\\傾向分數估計\\yang_GWAS_cons.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

covar_yang  <-   subset(yang_stasis,select=c("TWB1_ID","TWB1_ID","BODY_WEIGHT", 
                                             "BMI","BODY_FAT_RATE","BODY_WAISTLINE", 
                                             "BODY_BUTTOCKS","WHR","CREATININE","URIC_ACID")) 
colnames(covar_yang) <- c("FID","IID","BODY_WEIGHT", 
                          "BMI","BODY_FAT_RATE","BODY_WAISTLINE", 
                          "BODY_BUTTOCKS","WHR","CREATININE","URIC_ACID") 
write.table(covar_yang,file='C:\\Users\\User\\Desktop\\傾向分數估計\\covar_yang.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

#痰瘀 
Phlegm_stasis <- read.csv("C:\\Users\\User\\Desktop\\傾向分數估計\\痰盂傾向分數估計.csv",fileEncoding = "big5") 
Phlegm_stasis <- merge(Phlegm_stasis,lab_info_input, by = "Release_No", all.x = T) 

Phlegm_stasis_list  <-  subset(Phlegm_stasis,select=c("TWB1_ID","TWB1_ID"))  
write.table(Phlegm_stasis_list,file='C:\\Users\\User\\Desktop\\傾向分數估計\\list_Phlegm.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

Phlegm_stasis_list <- subset(Phlegm_stasis,select=c("TWB1_ID","TWB1_ID","Phlegm_stasis")) 
colnames(Phlegm_stasis_list) <- c("FID","IID","Phlegm_stasis") 
Phlegm_stasis_list$Phlegm_stasis[which(Phlegm_stasis_list$Phlegm_stasis=="1")] <- 2 
Phlegm_stasis_list$Phlegm_stasis[which(Phlegm_stasis_list$Phlegm_stasis=="0")] <- 1 
write.table(Phlegm_stasis_list,file='C:\\Users\\User\\Desktop\\傾向分數估計\\Phlegm_GWAS_cons.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

covar_yang  <-   subset(yang_stasis,select=c("TWB1_ID","TWB1_ID","BODY_WEIGHT", 
                                             "BMI","BODY_FAT_RATE","BODY_WAISTLINE", 
                                             "BODY_BUTTOCKS","WHR","CREATININE","URIC_ACID")) 
colnames(covar_yang) <- c("FID","IID","BODY_WEIGHT", 
                          "BMI","BODY_FAT_RATE","BODY_WAISTLINE", 
                          "BODY_BUTTOCKS","WHR","CREATININE","URIC_ACID") 
write.table(covar_yang,file='C:\\Users\\User\\Desktop\\傾向分數估計\\covar_Phlegm.txt',sep = "\t",row.names = F, 
            quote = F,fileEncoding = "Big5") 

#把序號對應成TWB編號------------------------------------------------------------ 

