library(faraway)
library(matlib)
head(teengamb)

tg<-teengamb[teengamb$sex==0,]
# or tg<-filter(.data=teengamb,sex==0)
head(tg)

tg.lm<-lm(gamble~status+income+verbal, data=tg)
summary(tg.lm) 

tg <- teengamb_M #Used for exercise 2
tg$gamble <- sqrt(teengamb_M$gamble) #used for exercise 2
tg.lm <- lm(gamble~status+income+verbal, data=gamble_root) #Used for exercise 2

# Calculate x* for Exercise 1(a)(b) from Project 6
summary(tg)
mean(tg$verbal)
mean(tg$status)

# Define xstar for predictions
xstar<-data.frame(status=mean(tg$status),
                  income=mean(tg$income),
                  verbal=mean(tg$verbal))

# Confidence interval for Y|x=x*
predict(tg.lm, newdata=xstar, interval="conf", level= .95)

# Prediction interval for Y|x=x*
predict(tg.lm, newdata=xstar, interval="predict", level= .95)

#______________________________________________________________________________
# Manual Calculation of prediction interval

# MSE Calculation (verify with anova(tg.lm))
(MSE<-sum(residuals(tg.lm)^2)/(28-4))

# capitol X* column vector
Xstar<-c(1, mean(tg$status), mean(tg$income), mean(tg$verbal))

# beta coefficients
b<-coefficients(tg.lm)

#predicted value
t(Xstar)%*%b

# t-value
tval<-qt(1-(1-.95)/2,28-4)

# X matrix
X<-model.matrix(tg.lm)

# Manual prediction interval (1-observation, compare with predict(..) from above)
c(t(Xstar)%*%b-tval* (699.78*(1+t(Xstar)%*%inv(t(X)%*%X)%*%Xstar))^.5,
  t(Xstar)%*%b+tval* (699.78*(1+t(Xstar)%*%inv(t(X)%*%X)%*%Xstar))^.5)

# Manual confidence interval
c(t(Xstar)%*%b-tval* (699.78*(t(Xstar)%*%inv(t(X)%*%X)%*%Xstar))^.5,
  t(Xstar)%*%b+tval* (699.78*(t(Xstar)%*%inv(t(X)%*%X)%*%Xstar))^.5)

#______________________________________________________________________________
# Prediction interval for the mean of a sample of n=25 (see Exercise 1(d) from
# Project 6 Exercises)
c(t(Xstar)%*%b-tval* (699.78*(1/25+t(Xstar)%*%inv(t(X)%*%X)%*%Xstar))^.5,
  t(Xstar)%*%b+tval* (699.78*(1/25+t(Xstar)%*%inv(t(X)%*%X)%*%Xstar))^.5)

#------------------------------------------------Exercise 2----------------------------------------------------
library(faraway)
library(matlib)
head(teengamb)

tg<-teengamb[teengamb$sex==0,]
# or tg<-filter(.data=teengamb,sex==0)
head(tg)

tg.lm<-lm(gamble~status+income+verbal, data=tg)
summary(tg.lm) 

# Calculate x* for Exercise 1(a)(b) from Project 6
summary(tg)
mean(tg$verbal)
mean(tg$status)

# Define xstar for predictions
xstar<-data.frame(status=mean(tg$status),
                  income=mean(tg$income),
                  verbal=mean(tg$verbal))

# Confidence interval for Y|x=x*
predict(tg.lm, newdata=xstar, interval="conf", level= .95)

# Prediction interval for Y|x=x*
predict(tg.lm, newdata=xstar, interval="predict", level= .95)

#______________________________________________________________________________
# Manual Calculation of prediction interval

# MSE Calculation (verify with anova(tg.lm))
(MSE<-sum(residuals(tg.lm)^2)/(28-4))

# capitol X* column vector
Xstar<-c(1, mean(tg$status), mean(tg$income), mean(tg$verbal))

# beta coefficients
b<-coefficients(tg.lm)

#predicted value
t(Xstar)%*%b

# t-value
tval<-qt(1-(1-.95)/2,28-4)

# X matrix
X<-model.matrix(tg.lm)

# Manual prediction interval (1-observation, compare with predict(..) from above)
c(t(Xstar)%*%b-tval* (699.78*(1+t(Xstar)%*%inv(t(X)%*%X)%*%Xstar))^.5,
  t(Xstar)%*%b+tval* (699.78*(1+t(Xstar)%*%inv(t(X)%*%X)%*%Xstar))^.5)

# Manual confidence interval
c(t(Xstar)%*%b-tval* (699.78*(t(Xstar)%*%inv(t(X)%*%X)%*%Xstar))^.5,
  t(Xstar)%*%b+tval* (699.78*(t(Xstar)%*%inv(t(X)%*%X)%*%Xstar))^.5)

#______________________________________________________________________________
# Prediction interval for the mean of a sample of n=25 (see Exercise 1(d) from
# Project 6 Exercises)
c(t(Xstar)%*%b-tval* (699.78*(1/25+t(Xstar)%*%inv(t(X)%*%X)%*%Xstar))^.5,
  t(Xstar)%*%b+tval* (699.78*(1/25+t(Xstar)%*%inv(t(X)%*%X)%*%Xstar))^.5)

