#讀入資料
library(tidyverse)
data <- read.csv("C:\\R\\LS305中醫\\TCM_list20220924.csv",fileEncoding = "Big5")
head(data)

#分離資料
sex<-c(data$SEX)
age<-c(data$age_gruop)
yin<-c(data$陰虛體質)
yang<-c(data$陽虛體質)
TCM<-c(data$痰瘀體質)
peace<-c(data$平和體質)
con<-c(data$體質)
#體質
newdata1<-data[,c(2,48)]
newdata1$體質<-factor(newdata1$體質)
#年齡層
newdata2<-data[,c(2,49)]
newdata2$age_gruop<-factor(newdata2$age_gruop)
#各年齡層體質人數

#繪圖
library(ggplot2)
#比較男女各體質的人數
par(mfrow = c(1,1),xpd=NA,oma=c(0,0,0,8))
barplot(table(con,sex),main='男女各體質人數',beside=T,col=rainbow(nlevels(newdata1$體質)))
legend(par("usr")[2],par("usr")[2],legend=as.character(levels(newdata1$體質)),fill=rainbow(nlevels(newdata1$體質)),col="black",xjust=0, yjust=-0.2)

#比較年齡層人數
par(mfrow = c(1,1),xpd=NA,oma=c(0,0,0,8))
barplot(table(age,sex),main='各年齡層體質人數',beside=T,col=rainbow(nlevels(newdata2$age_gruop)))
legend(par("usr")[2],par("usr")[2],legend=as.character(levels(newdata2$age_gruop)),fill=rainbow(nlevels(newdata2$age_gruop)),col="black",xjust=0, yjust=-0.2)


#匯出資料
aaa <- table(con,sex)
bbb <- table(age,sex)
write.csv(aaa, file = "C:\\R\\LS305中醫\\男女各體質人數.csv",fileEncoding = "Big5")
write.csv(bbb, file = "C:\\R\\LS305中醫\\男女各年齡人數.csv",fileEncoding = "Big5")

#anova(性別和體質)
data2 <- read.csv("C:\\R\\LS305中醫\\男女各體質人數.csv",fileEncoding = "Big5")
sex2 <- c(data2$女性)
sex3 <- c(data2$男性)
t.test(sex2,sex3, var.equal=TRUE)

#anova(年齡和體質)
data3 <- read.csv("C:\\R\\LS305中醫\\test2.csv",fileEncoding = "Big5")
age2 <- c(data3$女性)
age3 <- c(data3$男性)
t.test(age2,age3, var.equal=TRUE)

#BMI和體質的卡方檢定
data4 <- read.csv("C:\\R\\LS305中醫\\TCMmerge3.csv",fileEncoding = "Big5")
Baseline_data <- subset(data4, FOLLOW=="Baseline")
BMI.table <- table(data5$體質,data5$BMI)
chisq.test(BMI.table)
