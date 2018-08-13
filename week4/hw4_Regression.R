
  # p  質越小差異越顯著


library(tidyr)
library(stringr)
path =  "C:\\Users\\B06408042\\Desktop\\GitHub\\CSX_RProject_summer_2018\\week4\\UCI_Credit_Card.csv"
TB = read.csv(path)
TB$EDUCATION = str_replace_all(TB$EDUCATION, c("5" = "0", "6" = "0", "4" = "0"))


TB$ID = factor(TB$ID)
TB$SEX = factor(TB$SEX, labels = c("male", "female"))
TB$EDUCATION = factor(TB$EDUCATION, labels = c("others","graduate school", "university", "high school"))
TB$MARRIAGE = factor(TB$MARRIAGE, labels = c("others","married", "single","divorce"))
TB$AGE = factor(TB$AGE)
TB$default.payment.next.month = as.numeric(TB$default.payment.next.month = factor(TB$default.payment.next.month))

TB$PAY_0 = factor(TB$PAY_0, labels = c("No consumption", "Paid in full", "use revolving credit", "delay 1 month", "delay 2 month","delay 3 month","delay 4 month","delay 5 month","delay 6 month","delay 7 month","delay 8 month"))
TB$PAY_2 = factor(TB$PAY_2, labels = c("No consumption", "Paid in full", "use revolving credit", "delay 1 month", "delay 2 month","delay 3 month","delay 4 month","delay 5 month","delay 6 month","delay 7 month","delay 8 month"))
TB$PAY_3 = factor(TB$PAY_3, labels = c("No consumption", "Paid in full", "use revolving credit", "delay 1 month", "delay 2 month","delay 3 month","delay 4 month","delay 5 month","delay 6 month","delay 7 month","delay 8 month"))
TB$PAY_4 = factor(TB$PAY_4, labels = c("No consumption", "Paid in full", "use revolving credit", "delay 1 month", "delay 2 month","delay 3 month","delay 4 month","delay 5 month","delay 6 month","delay 7 month","delay 8 month"))
TB$PAY_5 = factor(TB$PAY_5, labels = c("No consumption", "Paid in full", "use revolving credit", "delay 2 month", "delay 3 month","delay 4 month","delay 5 month","delay 6 month","delay 7 month","delay 8 month"))
TB$PAY_6 = factor(TB$PAY_6, labels = c("No consumption", "Paid in full", "use revolving credit", "delay 2 month", "delay 3 month","delay 4 month","delay 5 month","delay 6 month","delay 7 month","delay 8 month"))




summary(TB$EDUCATION)
summary(TB$SEX)
summary(TB$MARRIAGE)
summary(TB$AGE)
summary(TB$default.payment.next.month)

names(TB)[2]<-"given_credit"
names(TB)[3]<-"sex"
names(TB)[4]<-"education"
names(TB)[5]<-"marriage"
names(TB)[6]<-"age"
names(TB)[25]<-"default"
names(TB)[7]<-"Sep"
names(TB)[8]<-"Aug"
names(TB)[9]<-"Jul"
names(TB)[10]<-"Jun"
names(TB)[11]<-"May"
names(TB)[12]<-"Apr"



# 畫圖觀察
library(ggplot2)
library(dplyr)

d<-TB%>%
  group_by(sex,education,age,marriage)%>%
  summarise(defaultAvg = mean(default))
d
ggplot(d, aes(x = age, y = defaultAvg, color = sex))+ 
  geom_point()+
  facet_grid(. ~ sex)
ggplot(d, aes(x = age, y = defaultAvg, color = education))+ 
  geom_point()+
  facet_wrap(~ education)
ggplot(d, aes(x = age, y = defaultAvg, color = marriage))+
  geom_point()+
  facet_wrap(~ marriage)


ggplot(d, aes(x = marriage, y = defaultAvg))+
  geom_boxplot()
ggplot(d, aes(x = sex, y = defaultAvg))+
  geom_boxplot()
ggplot(d, aes(x = education, y = defaultAvg))+
  geom_boxplot()



# 
dr<-TB%>%
  group_by(sex,education,age,marriage)%>%
  filter(marriage =="married", sex == "male", education =="graduate school", age %in% c(40:80) )%>%
  summarise(defaultAvg = mean(default))
dr
summary(dr)

ds<-TB%>%
  filter(education == "graduate school")%>%
  group_by(sex)%>%
  summarise(defaultAvg = mean(default))
ds  
de<-TB%>%
  group_by(age,education)%>%
  summarise(defaultAvg = mean(default))
de



da<-TB%>%
  group_by(age,sex)%>%
  summarise(defaultAvg = mean(default))
da  
t.test(defaultAvg ~ sex, da)

# 以年齡分群
de<-TB%>%
  group_by(age,education)%>%
  summarise(defaultAvg = mean(default))
result = aov(defaultAvg~education, de)
summary(result)
TukeyHSD(result)


dm<-TB%>%
  group_by(age,marriage)%>%
  summarise(defaultAvg = mean(default))
resultm = aov(defaultAvg~marriage, dm)
summary(resultm)


ds<-TB%>%
  group_by(age,sex)%>%
  summarise(defaultAvg = mean(default))
result = aov(defaultAvg~sex, ds)
summary(result)

TukeyHSD(result)









library(Hmisc)


m1 <- lm(defaultAvg ~ education, de)
anova(m1)
ggplot(de, aes(group = education , 
           y =defaultAvg , x = age )) +
  geom_point() +
  stat_smooth(method = 'lm', se = F) +
  stat_smooth(aes(group = education , 
                  y =defaultAvg , x = age), 
              method = 'lm', se = F) + 
  facet_grid( . ~  education) +
  labs(title="教育程度",x ="年齡", y = "拖欠機率" )


m2 <- lm(defaultAvg ~ marriage, dm)
anova(m2)
ggplot(dm, aes(group = marriage , 
               y =defaultAvg , x = age )) +
  geom_point() +
  stat_smooth(method = 'lm', se = F) +
  stat_smooth(aes(group = marriage , 
                  y =defaultAvg , x = age), 
              method = 'lm', se = F) + 
  facet_grid( . ~  marriage) +
  labs(title="婚姻狀態",x ="年齡", y = "拖欠機率" )


m3 <- lm(defaultAvg ~ sex, ds)
anova(m3)
ggplot(ds, aes(group = sex , 
               y =defaultAvg , x = age )) +
  geom_point() +
  stat_smooth(method = 'lm', se = F) +
  stat_smooth(aes(group = sex , 
                  y =defaultAvg , x = age), 
              method = 'lm', se = F) + 
  facet_grid( . ~  sex) +
  labs(title="性別",x ="年齡", y = "拖欠機率" )
m1
m2
m3

str(m1)
res_lm <- lapply(list(m1, m2, m3), summary)

(res_lm[[2]]$r.sq - res_lm[[3]]$r.sq)/res_lm[[2]]$r.sq
anova(m3, m2)
(res_lm[[2]]$r.sq - res_lm[[1]]$r.sq)/res_lm[[1]]$r.sq
anova(m1, m2)




require(coefplot)
m2 <- lm(LIMIT_BAL ~ EDUCATION+PAY_0- 1, 
         data = ccdata)
coefplot(m2, xlab = '估計值', ylab = '迴歸變項', title = '反應變項 = LIMIT_BAL')
fit_m2 <- data.frame(ccdata[, c(2, 4, 7)], fitted = fitted(m2), resid = resid(m2),
                     infl = influence(m2)$hat )


ggplot(data = fit_m2, aes(x = LIMIT_BAL, group = EDUCATION )) +
  stat_density(geom = 'path', position = 'identity') +
  stat_density(geom = 'path', position = 'identity', aes(x = fitted)) +
  geom_vline(xintercept = c(with(ccdata, tapply(LIMIT_BAL,EDUCATION, mean))), linetype = 'dotted')+
  facet_grid(EDUCATION ~ .) +
  scale_x_continuous(breaks = seq(200, 900, by = 100))+
  labs(x = '數學分數', y = '機率密度')

De = TB%>%
  group_by(education,Sep,given_credit,age)%>%
  summarise(defaultAvg = mean(default))
  
ggplot(De,aes(x =, color = education, size = ))+
  geom_histogram()+
  facet_wrap(~ education)

         


head(De)
tail(De)
  
