library(faraway)
head(cheddar)

cheddar.lm<-lm(taste~Acetic+H2S+Lactic, cheddar)
summary(cheddar.lm)



#2)
#(b) Acetic and H2S are measured on a log scale. Fit a linear model where all three pre-
#dictors are measured on their original scale. Identify the predictors that are statistically
#significant at the 5% level for this model.

cheddar2.lm<-lm(taste~exp(Acetic)+exp(H2S)+Lactic, cheddar)
summary(cheddar2.lm)
exAcetic<-exp(cheddar$Acetic)
exAcetic[1]
# = 93.97
log(93.97229)

#(d) (c skipped)
#(d) If H2S is increased 0.01 for the model used in (a), what change in the taste would
#be expected?

cheddar3.lm<-lm(taste~Acetic+H2S+offset(H2S+.01)+Lactic, cheddar)
summary(cheddar3.lm)

plot(taste~H2S, cheddar)
plot(taste~offset(H2S+.01), cheddar)

inc_H2s<- cheddar$H2S + .01
cheddar4.lm<-lm(taste~Acetic+incH2s+Lactic, cheddar)

inc_H2S.lm<-lm(taste~Acetic+(H2S+.01)+Lactic, cheddar)
inc_H2S.lm

summary(cheddar4.lm)

exincH2S<-exp(incH2s)


otherH2s<-exp(cheddar$H2S)+.01
exincH2S / otherH2s


exH2s<-exp(cheddar$H2S)
mean.logincH2S<-mean(logincH2S)
mean.logH2S<-mean(logH2s)
difference<-mean.logincH2S/mean.logH2S
difference

#Notes for 2/22/21 Below



