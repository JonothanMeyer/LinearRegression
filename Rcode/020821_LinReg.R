#Project 3

#Exercise 1:
library(faraway)
head(prostate)
? prostate
#Exercise 1
#1
plot(prostate$lcp~prostate$lcavol) #predict: lcavol, response: lcp
mv.lm<-lm(lcp~lcavol, prostate)
summary(mv.lm)
mv.lm #Y = betanot + beta1 + E
abline(mv.lm)
sigma(mv.lm)
#2)
b0<- -1.260
b1<-.80
xstar<-4
predy<-b0 + b1 * xstar
predy

newx<-seq(0,3,by=.01)
conf.int<-predict(mv.lm, newdata = data.frame(lcavol=newx), interval="confidence",level =0.95)
conf.int
newx

#Testing (Ignore)
newx<-seq(0,4,by=.01)
conf.int<-predict(mv.lm, newdata = data.frame(lcp=newx), interval="confidence",level =0.95)
conf.int
newx


lines(newx, conf.int[,2], col="red", lty=2)


mv.lm<-lm(lcp~lcavol, prostate)
summary(mv.lm)

mv.lm
sigma(mv.lm)

#CI for beta_1 with a 95% Confidence level, #3 from exersize 1
b0<- -1.2609
b1<-.801
sb1<-.09
d.f<- 95 #degress of freedom
n.samp<-97

c(b1-qt(1-.05/2,d.f)*sb1,
  b1+qt(1-.05/2,d.f)*sb1)

confint(mv.lm)

#direct calc of CI for mean of yx=x*
xframe<-as.data.frame(5)
colnames(xframe)<-"lcavol"
predict(mv.lm,xframe,interval="pred",level=.95)
predict(mv.lm,xframe,interval="conf",level=.95)


? predict

# manual calc of CI for mean of Y
xstar<- 4
mean.x<-mean(prostate$lcavol)
s.res<- sd(residuals(mv.lm)) #1.031255
sxx<-sum((prostate$lcavol-mean.x)^2)
ypred<-b0+b1*xstar
s.xstar<-s.res*(1/n.samp+(xstar-mean.x)^2/sxx)^.5

c(ypred-qt(1-.05/2,n.samp-2)*s.xstar,
  ypred+qt(1-.05/2,n.samp-2)*s.xstar)

#--------------------------------------------------------
library(ggplot2)

ggplot(data.frame(cbind(prostate$lcavol,prostate$lcp)),
       aes(prostate$lcavol,prostate$lcp))+
  geom_point(size=2.5)
 

#"handwritten" CI for prostate datay (pred:lcavol, response:lcp)
ggplot(prostate[c(1,6)],aes(lcavol,lcp))+geom_point(size=1)+
  geom_line(aes(newx,conf.int[,2]), colour='red',linetype='dashed',size=1)+
  geom_line(aes(newx,conf.int[,3]),colour='red',linetype='dashed',size=1)+
  geom_line(aes(prostate$lcp,prostate.ci[,2]),colour='black',linetype='dashed',size=1)


#"handwritten" CI for prostate datay (pred:lcavol, response:lcp)
ggplot(prostate[c(1,6)],aes(lcavol,lcp))+geom_point(size=1)+
  geom_line(aes(newx,conf.int[,2]), colour='red',linetype='dashed',size=1)+
  geom_line(aes(newx,conf.int[,3]),colour='red',linetype='dashed',size=1)+
  geom_line(aes(prostate$lcp,prostate.ci[,2]),colour='black',linetype='dashed',size=1)

aes(newx,conf.int[,1])

ggplot(prostate[c(1,6)],aes(lcavol,lcp))+geom_point(size=1)+
  geom_smooth(method="glm",level=0.95) + geom_smooth(method="lm",level=0.9999)
  
? geom_smooth

prostate[c(1,6)]

? predict

conf_interval<-predict(mv.lm, newdata=data.frame())
predict(mv.lm)

prostate.ci<-predict(mv.lm, data=prostate$lcp, interval="confidence",level =0.95)
#---------------------------------------------------------------------------------------------------------------------------------
#Exercise 3 (2/17/21)
library(faraway)
library(dplyr)
head(cheddar)
pairs(cheddar)
#______________________________________________________________________________
# filter out 'bad data' from prostate data set with respect to lbph

prst<-filter(prostate, prostate$lbph>-1)
ched<-filter(cheddar, cheddar$Acetic<6)
head(ched)
plot(ched$Acetic, ched$taste)

ched.n<-length(ched$Acetic)





# Hypothesis tests for beta_1=0 with 2-sided alternative hypothesis

beta10<-0

ched.lm<-lm(ched$taste~ched$Acetic, offset = beta10*ched$Acetic)
summary(ched.lm)

? pt
#two sided
2*(1-pt(.0007624, 21))
# where is P-value in summary?
.9993989

# 1-sided alternative hypothesis
# No further computations required. 
# How do you extrat left-sided, right sided P-values from summary(prst.lm)?

#______________________________________________________________________________
# Hypothesis test for H_0: beta_1=1  H_A: beta_1<1
beta10<-1
ched2.lm<-lm(ched$taste~ched$Acetic,offset=beta10*ched$Acetic)
summpary(ched2)

beta10<-0
prst2.lm <- lm(prst$lpsa~prst$lcavol,offset= beta10*prst$lcavol)
summary(prst2.lm)

# how to extract lower tailed (left-sided) P-value from summary?
pt(,ched.n-2)
#______________________________________________________________________________
# Hypothesis tests for beta_1=0 or beta_2=0 in multiple regression models

prst12.lm <- lm(prst$lpsa~prst$lcavol+prst$lbph)
summary(prst12.lm)

ched12.lm<- lm(ched$Acetic~ched$taste+ched$Acetic)
summary(ched12.lm)

# Scatterplot
plot(lpsa~lbph,data=prst)
plot(taste~Acetic,data=ched) #pred:lbph, response: lpsa

# 2-sided alternative hypothesis

2*(1-pt(abs(15.47),21))

beta10<-0
ched.lm<-lm(ched$taste~ched$Acetic,offset=beta10*ched$Acetic)
summary(ched.lm)
# where are P-values in summary?
2*(1-pt(abs(15.47),21))

#2sided
2*(1-pt(abs(15.47),21))

#right-sided
1-pt(15.47, 21)

#left-sided (if we have a large
pt(15.47,21) 


