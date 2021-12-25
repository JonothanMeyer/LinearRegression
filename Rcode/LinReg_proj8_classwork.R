library(MASS)
library(faraway)

head(mammalsleep)

sleep_gest.df<- mammalsleep[,c(5,7)]

plot(sleep_gest.df$gestation, sleep_gest.df$sleep)

sleep_gest.mod<-lm(sleep~gestation + lifespan, data= sleep_gest.df)

boxcox(sleep_gest.mod, plotit=FALSE)
boxcox(sleep_gest.mod, plotit=TRUE)

summary(lm(sleep~getation, data = sleep_gest.df))
plot(sleep_gest.mod)


sleep_gest.mod2<- lm(sleep^.5~gestation, data=sleep_gest.df)
summary(sleep_gest.mod2)
plot(sleep_gest.mod2)

qqnorm(sleep_gest.mod2$residuals)
qqnorm(sleep_gest.mod$residuals)

#------------------------04/26/21----------------------------

head(cheddar)
ched.lm <- lm(taste~.,data=cheddar)
summary(ched.lm)
step(ched.lm) # step(ched.lm, trace = 0)

library(faraway)
library(dplyr)
head(wcgs)
lmod <- glm(chd~height+cigs, family = binomial, data = wcgs)
summary(lmod)


# AIC: Explains which preditor has the biggest impact.
#Last Exercise on project

head(fat)

fattt <- fat %>% select(-brozek, -density)
head(fattt)
names(fattt)

? step

fat.lm <- lm(siri ~., data = fattt)
step(fat.lm, trace = 1) #each 'step' removes a predictor to build a model with lowest possible AIC after accessing the AIC
# with all combinations of predictor removal. The next 'step' repeats this process after removing the previous predictor.
#This process is continued until the model's AIC is now longer lowered from removing predictors.

best.lm.AIC <- lm(siri ~ weight + height + adipos + free + chest + 
  abdom + thigh + knee + ankle + biceps + forearm, data = fattt)

plot(fitted(best.lm.AIC), resid(best.lm.AIC))
qqnorm(resid(best.lm.AIC))
qqline(best.lm.AIC)

#part c)

library(MASS)

head(meatspec)
? meatspec

trainmeat <- meatspec[1:172,]
rgmod <- lm.ridge(fat~.,data = trainmeat, lambda = seq(0, 5e-8, len=21))
plot(lm.ridge(fat~., data=trainmeat, lambda = seq(0, 5e-8, len = 21)))

select(lm.ridge(fat~., data = trainmeat, lambda = seq(0,5e-8, len=21)))
rgmod.lm <- lm.ridge(fat~., data = trainmeat, lambda = .0000000175)
coef(rgmod.lm)

plot(fitted(rgmod.lm), resid(rgmod))
coef(rgmod.lm)

lambda = seq(0, 5e-8, len = 21)


coef(rgmod)
plot(fitted(rgmod), resid(rgmod))
lambda = seq(0, 5e-8, len=21)


fattt <- fat %>% select(-brozek, -density)
head(fattt)
names(fattt)

best.lm.AIC <- lm(siri ~ weight + height + adipos + free + chest + 
                    abdom + thigh + knee + ankle + biceps + forearm, data = fattt)

select <- MASS::select

lambda = seq(0, 10, len=21)
fat.ridge.lm <- lm.ridge(siri~.,data = fattt, lambda = lambda )
plot(lm.ridge(siri~., data=fattt, lambda = lambda))
legend(legend = fat.ridge.lm$coef, x = "topright")
cof <- fat.ridge.lm$coef
cof

select(fat.ridge.lm <- lm.ridge(siri~.,data = fattt, lambda = lambda))
coef(fat.ridge.lm)

fat.ridge.lm <- lm.ridge(siri~.,data=fattt, lambda = 0)
coef(fat.ridge.lm)

plot(lm.ridge(siri~., data=fattt, lambda = lambda))

? lm.ridge

lambda1 = 12
fat.ridge.lm1 <- lm.ridge(siri~.,data=fattt, lambda = lambda1)
coef(fat.ridge.lm1)
co1 <- coef(fat.ridge.lm1)
max(coef(fat.ridge.lm1))
# You use ridge regression when you're using a lot of predictor variables and want to account for colinearity

lambda1 = 10
fat.ridge.lm1 <- lm.ridge(siri~.,data=fattt, lambda = lambda1)
coef(fat.ridge.lm1)
co2 <- coef(fat.ridge.lm1)
max(coef(fat.ridge.lm1))

names(co1)
names(co2)

co1
co2

summary(lm(siri~., data = fattt))
lm <- lm(siri~., data = fattt)
co1 - co2

step(lm, trace = 1)

summary(lm(siri ~ weight + height + adipos + free + chest + abdom + thigh + knee + ankle + biceps + forearm, data = fattt))

#--------------------------------------------------------------------------classwork 04/28/21----------------------

head(cornnit) #There are multiple observations for x of the same value so can do goodness of fit

names(cornnit)        

plot(yield~nitrogen, data = cornnit)
ablin(corn.lm)
corn.lm <- lm(yield~nitrogen, data = cornnit)
corn.lm.lof <- lm(yield~factor(nitrogen), data= cornnit)

anova(corn.lm, corn.lm.lof) #it has a small p-value. reject the null hypothesis that a linear model fits this data

#look at transformations
plot(yield~log(1+nitrogen), data = cornnit)

corn.lm <- lm(yield~log(1+nitrogen), data = cornnit)
corn.lm.lof <- lm(yield~factor(log(1+nitrogen)), data= cornnit)

anova(corn.lm, corn.lm.lof) #failed lack of fit test.
