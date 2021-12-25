
#Only needs one run on new computer
install.packages("faraway")

install.packages("ggplot2")

#Calls library on computer (wherever it lives):
library(faraway)
head(divusa)


# Data simulations for building qq-plots data acumen:
samp.size=100

theory.data<-qnorm(seq(0.01,0.99,0.01))
empir.data<- rnorm(samp.size) 
qqplot(theory.data,empir.data)

empir.std.data<-quantile(empir.data,probs = seq(0.01,0.99,0.01))
qqplot(empir.std.data~theory.data)
qqnorm(empir.data)

qqplot(divusa$unemployed,divusa$divorce)

theory.data<-qnorm(seq(0.01,0.99,0.01))
empir.std.data<-quantile(rnorm(100),probs = seq(0.01,0.99,0.01))
qqplot(theory.data,empir.std.data)

qqnorm(rnorm(100))

# Linear Model Analysis:
head(divusa)
plot(divusa$unemployed~divusa$divorce)
unemployed.divorice.lm<-lm(divusa$divorce~divusa$unemployed)
summary(unemployed.divorice.lm)
plot(residuals(unemployed.divorice.lm)~divusa$unemployed)
qqnorm(residuals(unemployed.divorice.lm))


   

#Sample statistics
mean.y<-mean(divusa$divorce)
mean.x<-mean(divusa$unemployed)

sxx.divusa<-sum((divusa$unemployed-mean.x)^2)
sst.divusa<-sum((divusa$divorce-mean.y)^2)

sxy.cheddar<-sum(cheddar$H2S*cheddar$taste)-sum(cheddar$H2S)*sum(cheddar$taste)/length(cheddar$taste)
sse.cheddar<-sum(residuals(taste.H2S.lm)^2)

sxy.divusa<-sum(divusa$divorce*divusa$unemployed)-sum(divusa$divorce)*sum(divusa$unemployed)/length(divusa$divorce)
sse.divusa<-sum(residuals(unemployed.divorice.lm))

ssA.cheddar<-sst.cheddar-sse.cheddar
s.cheddar<-(sse.cheddar/(length(cheddar$taste)-2))^.5

ssA.divusa<-sst.divusa-sse.divusa
s.divusa<-(sse.divusa/(length(divusa$divorce)-2))^.5

ssA.divusa
sse.divusa

summary(unemployed.divorice.lm)

# Point estimate of coefficient of determination
(r2.divusa<-1-sse.divusa/sst.divusa)

# Point estimates of parameters
(b1.cheddar<-sxy.cheddar/sxx.cheddar)
(bo.cheddar<-mean.y-b1.cheddar*mean.x)

b1.divusa<-sxy.divusa/sxx.divusa
bo.divusa<-mean.y-b1.divusa*mean.x

# Standard error for parameter estimates:
(sb1.cheddar<-s.cheddar/(sxx.cheddar^.5))
(sb0.cheddar<-s.cheddar*(1/length(cheddar$taste)+mean.x^2/sxx.cheddar)^.5)

# Standard error for \hat{y} if x^*=xstar
xstar=6
(sxstar.cheddar<-s.cheddar*(1/length(cheddar$taste)+(xstar-mean.x)^2/sxx.cheddar)^.5)

# ANOVA table for a linear model

anova(unemployed.divorice.lm)

# Coefficients for the model
coef(taste.H2S.lm)

coef(unemployed.divorice.lm)

# Fitted values

fitted(unemployed.divorice.lm)

# Residual values

resid(unemployed.divorice.lm)



