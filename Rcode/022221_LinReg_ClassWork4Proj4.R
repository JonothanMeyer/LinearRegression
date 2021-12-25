#HW3

library(faraway)
head(divusa)
divusa

? divusa

pairs(divusa)
plot(divusa$femlab,divusa$divorce)


#---------Class work----------------
divm.lm<-lm(divorce~birth,data=divusa)
summary(divm.lm)
anova(divm.lm)


#Scatterplot
plot(divusa$birth,divusa$divorce) #predictor: birth, response: divorce
abline(divm.lm)

#Scatterplot using ggplot()
library(ggplot2)
ggplot(data=divusa,aes(birth, divorce)) +
  geom_point()

#Residual plot(res vs predictor) using plot()
plot(resid(divm.lm)~divusa$birth)

#Residual plot (residuals vs. predictor) using ggplot()
ggplot(data=divusa,aes(birth,resid(divm.lm))) +
  geom_point() +
  

#Residual plot (residuals vs. fitted) using plot()
plot(resid(divm.lm)~fitted(divm.lm))

#Residual plot (residuals vs. fitted) using ggplot()
ggplot(data=divusa,aes(fitted(divm.lm),resid(divm.lm)))+
  geom_point()

ggplot(data=divusa,aes(fitted(divm.lm),resid(divm.lm), colour=femlab, size=unemployed, shape=))+
  geom_point() +
  scale_color_viridis(option = "A")

install.packages("viridis")
library("viridis")

#-----------------------------------Exercise 2 - Proj 4-----------------------------------
library(faraway)
head(teengamb)

plot(gamble~income, data=teengamb)
abline()
#We want to check the constant variance assumption
tg.lm<-lm(gamble~income,data=teengamb)

library(ggplot2)

plot(teengamb$income, resid(tg.lm))

#now plot the residuals against fitted - we want to be looking at this because 
plot(fitted(tg.lm), resid(tg.lm))
#-----------------------Aside-----------------------
#looking at things with more than 1 predictor
tg2.lm<-lm(gamble~income+verbal,data=teengamb)
summary(tg2.lm)

plot(teengamb$income, resid(tg2.lm))
plot(teengamb$verbal, resid(tg2.lm))

#now plot the residuals against fitted - we want to be looking at this because it shows how the distribution changes from different data points. 
plot(fitted(tg2.lm), resid(tg2.lm))

anova(tg.lm)
anova(tg2.lm)

#to check assumption of normality of residuals (3210 method)
hist(resid(tg2.lm))
#qqnorm - quantile quantile plot helps shows normality too
#raises a dark orange flag. Prof: "seems to be normal with an outlier"
qqnorm(resid(tg2.lm))

#Then we need to check for outliers
teengamb2<-teengamb[teengamb$gamble<100,]

tg.outlier.lm<-lm(gamble~income, data=teengamb2)

plot(gamble~income,data=teengamb2)       
anova(tg.outlier.lm)
qqnorm(resid(tg.outlier.lm))
#want to look how removing the outlier changed the regression itself
summary(tg.outlier.lm) #changed the slope by more than 1

#then we can visualize the change in slope by plotting both least squares line
plot(gamble~income, data=teengamb, main="Least ^2 regression lines w/ and w/out outlier")
abline(tg.lm)
abline(tg.outlier.lm)

#check for the relationship between...
#When you check the 

#Distinguishing gender in teengamb data set------------
teengamb$sex<-as.character(teengamb$sex) #have to change 'sex' from numerical to character to avoid errors for 'shape' in ggplot
ggplot(data=teengamb,aes(income,gamble, colour=sex, shape=sex)) +
    geom_point()

ggplot(data=teengamb,aes(fitted(tg.lm),resid(tg.lm),colour=sex,shape=sex))+
    geom_point()

teengamb.M<-teengamb[teengamb$sex==0,]
teengamb.F<-teengamb[teengamb$sex==1,]

length(teengamb.M$sex)
length(teengamb.F$sex)

ggplot(data=teengamb.M,aes(income,gamble))+
  geom_point()

ggplot(data=teengamb.F,aes(income,gamble))+
  geom_point()

teengamb.F.lm<-lm(gamble~income,data=teengamb.F)
teengamb.M.lm<-lm(gamble~income,data=teengamb.M)

summary(tg.lm)
summary(teengamb.M.lm)
summary(teengamb.F.lm)

hist(resid(teengamb.F.lm))
hist(resid(teengamb.M.lm))

plot(gamble~income,data=teengamb)
abline(teengamb.F.lm)
abline(teengamb.M.lm)
