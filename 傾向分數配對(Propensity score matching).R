library(MatchIt)
library(dplyr)
library(ggplot2)
library(gridExtra)

#資料讀取
PSM <- read.csv("C:\\R\\LS305中醫\\TCM_Anova.csv",fileEncoding = "big5")

PSM_COV <- c("BODY_WEIGHT","BMI","BODY_FAT_RATE","BODY_WAISTLINE","BODY_BUTTOCKS","WHR",
             "CREATININE","URIC_ACID")


#第1步Difference-in-means: pre-treatment covariates
PSM %>%
  group_by(Yin_def) %>%
  select(one_of(PSM_COV)) %>%
  summarise_all(list(~mean(., na.rm = T)))

lapply(PSM_COV, function(v) {
  t.test(PSM[, v] ~ PSM[, 'Yin_def'])
})

#第2步傾向得分估計
m_ps <- glm(Yin_def ~ BODY_WEIGHT + BMI + BODY_FAT_RATE + BODY_WAISTLINE + BODY_BUTTOCKS, data = PSM)
summary(m_ps)

prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     Yin_def = m_ps$model$Yin_def)
head(prs_df)

#Examining the region of common support
labs <- paste("Yin_def:", c("Yin_def", "Not_Yin_def"))
prs_df %>%
  mutate(Yin_def = ifelse(Yin_def == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~Yin_def) +
  xlab("Probability of Yin_def") +
  theme_bw()
#第3步Executing a matching algorithm
PSM_nomiss <- PSM %>%  # MatchIt does not allow missing values
  select(BMI, Yin_def, one_of(PSM_COV)) %>%
  na.omit()

mod_match <- matchit(Yin_def ~ BODY_WEIGHT + BMI + BODY_FAT_RATE + BODY_WAISTLINE + BODY_BUTTOCKS,
                     method = "nearest", data = PSM_nomiss)
dta_m <- match.data(mod_match)
dim(dta_m)

#第4步Examining covariate balance in the matched sample
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
  fn_bal(dta_m, "BODY_WEIGHT"),
  fn_bal(dta_m, "BMI") + theme(legend.position = "none"),
  fn_bal(dta_m, "BODY_FAT_RATE"),
  fn_bal(dta_m, "BODY_WAISTLINE") + theme(legend.position = "none"),
  fn_bal(dta_m, "BODY_BUTTOCKS"),
  nrow = 3, widths = c(1, 0.8)
)
#第4.2 Difference-in-means
dta_m %>%
  group_by(Yin_def) %>%
  select(one_of(PSM_COV)) %>%
  summarise_all(list(mean))

lapply(PSM_COV, function(v) {
  t.test(dta_m[, v] ~ dta_m$Yin_def)
})
#第5步Estimating treatment effects
with(dta_m, t.test(BMI ~ Yin_def))

lm_treat1 <- lm(BMI ~ Yin_def, data = dta_m)
summary(lm_treat1)

lm_treat2 <- lm(BMI ~ Yin_def + BODY_WEIGHT  + BODY_FAT_RATE + BODY_WAISTLINE + BODY_BUTTOCKS, data = dta_m)
summary(lm_treat2)
