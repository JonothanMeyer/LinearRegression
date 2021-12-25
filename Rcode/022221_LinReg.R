#HW3

library(faraway)
head(divusa)

? divusa

pairs(divusa)
plot(divusa$femlab,divusa$divorce)


#---------Class work----------------
divm.lm<-lm(divorce~birth,data=divusa)
summary(divm.lm)


#Scatterplot
plot(divusa$birth,divusa$divorce) #predictor: birth, response: divorce
abline(divm.lm)

#Scatterplot using ggplot()
library(ggplot2)
ggplot(data=divusa,aes(birth, divorce))) +
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