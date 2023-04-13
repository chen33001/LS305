library(MatchIt)
library(dplyr)
library(ggplot2)
library(gridExtra)


#資料讀取
PSM <- read.csv("C:\\R\\LS305中醫\\TCM_Anova.csv",fileEncoding = "big5")

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
  t.test(PSM[, v] ~ PSM[, 'Yin_def'])
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
  select(BMI, Yin_def, one_of(PSM_COV)) %>%
  na.omit()

mod_match1 <- matchit(Yin_def ~ BODY_WEIGHT + BMI + BODY_FAT_RATE + BODY_WAISTLINE + BODY_BUTTOCKS,
                     method = "nearest", data = PSM_nomiss)
dta_m1 <- match.data(mod_match1)
dim(dta_m1)

#陽虛

PSM_nomiss <- PSM %>%  # MatchIt does not allow missing values
  select(BMI, Yang_def, one_of(PSM_COV)) %>%
  na.omit()

mod_match2 <- matchit(Yang_def ~ BODY_WEIGHT + BMI + BODY_FAT_RATE + BODY_WAISTLINE + BODY_BUTTOCKS,
                      method = "nearest", data = PSM_nomiss)
dta_m2 <- match.data(mod_match2)
dim(dta_m2)

#痰盂

PSM_nomiss <- PSM %>%  # MatchIt does not allow missing values
  select(BMI, Phlegm_stasis, one_of(PSM_COV)) %>%
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
write.csv(dta_m1,file='C:\\Users\\user\\Desktop\\傾向分數估計\\陰虛傾向分數估計.csv',fileEncoding = "Big5")
write.csv(dta_m2,file='C:\\Users\\user\\Desktop\\傾向分數估計\\陽虛傾向分數估計.csv',fileEncoding = "Big5")
write.csv(dta_m3,file='C:\\Users\\user\\Desktop\\傾向分數估計\\痰盂傾向分數估計.csv',fileEncoding = "Big5")



