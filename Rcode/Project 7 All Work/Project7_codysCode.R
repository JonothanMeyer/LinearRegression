library(faraway)
library(ggplot2)

? mammalsleep

head(mammalsleep)

#Getting Rid of NA values for whole data set
mammalsleep1 <- mammalsleep[complete.cases(mammalsleep),]

# creating a list that shows all the different combinations of predictors 
listcombo <- unlist(sapply(0:4, function(x) combn(4,x,simplify = FALSE)),recursive = FALSE)
length(listcombo)

# has placed predation in every combination of predictors in the list above
predterms <- lapply(listcombo, function(x) paste( c("predation",c("body","nondream","lifespan","gestation")[x]),collapse="+"))

# checking for matching length of listcombo
predterm.n<-length(predterms)       

#creating a 16 x 2 will all NA values
coefm<-matrix(NA,predterm.n,2)

#This loop places cooeficient values and standard errors associated with each predictor combination in predterm.n
for(i in 1:predterm.n){lmi <- lm(as.formula(paste("sleep~", predterms[[i]])), data = mammalsleep1)
coefm[i,] <- summary(lmi)$coef[2,c(1,2)]
}

#Naming the matrix headers and rounding each value to 4 decimal places
rownames(coefm) <- predterms
colnames(coefm) <- c("beta","std err")
round(coefm,4)

#sorted the standard error column in accending order to find the lowest value and the associated predictors combo
sortedCoefm <- sort(round(coefm[,2],4))


mammalsleep1 <- mammalsleep[complete.cases(mammalsleep),]
#Ryan suggestion through his pairs chart was to log all predictors from out lowest std err combo
mammalsleep1$sleep <- log(mammalsleep1$sleep)
mammalsleep1$body <- log(mammalsleep1$body)
mammalsleep1$nondream <- log(mammalsleep1$nondream)
mammalsleep1$gestation <- log(mammalsleep1$gestation)
mammalsleep1$lifespan <- log(mammalsleep1$lifespan)

#Linear model with the lowest Standard error of predation, after transformation std.err = 0.011677
#In comparison to the pre-tranformation std error of predation = 0.1281
test.lm <- lm(sleep ~ predation + body + nondream + gestation + lifespan, mammalsleep1)

test.lm <- lm(sleep ~ predation + log(body) + nondream + log(gestation), mammalsleep1)


#multiple R-squared = 0.9643
#Adjusted R-squared = 0.9604
summary(test.lm)

plot(sleep~predation, data=mammalsleep1)


#---------------------------------------------------------------------------------------------------------------------------
#Jonos Work
# log(sleep) ~ predation + log(body) + log(nondream) + log(gestation), data = mammalsleep1
mams <- mammalsleep[complete.cases(mammalsleep),]

# Looking at the distribution of variables before and after transformation. Generally it can be observed that all are more
# normally distributed after log transformation
hist(mams$body, main = "Histogram of Body", xlab = "body")
hist(log(mams$body), col= "red", main = "Histogram of log(body)", xlab = "log(body)")
text(x=5, y=12, labels = "*color added to illustrate
     excitment of transformation")

hist(mams$nondream, main = "Histogram of Dream", xlab = "dream")
hist(log(mams$nondream), col = "blue")
text(x=1, y=15, labels = "*color added to illustrate
     excitment of transformation")

hist(mams$gestation, main = "Histogram of Gestation", xlab="gestation")
hist(log(mams$gestation), col = "pink", main = "Histogram of log(gestation)", xlab="log(gestation)")
text(x=2.8, y=8, labels = "*color added to illustrate
     excitment of transformation")

hist(mams$sleep, main = "Histogram of Sleep", xlab = "sleep")
hist(log(mams$sleep), col= "black", main = "Histogram of log(sleep)", xlab = "log(sleep)")
text(x=1.5, y=6, labels = "*black added to illustrate
     sadness of transformation")
#--------------------------------------------------------------------------------------------------------------
#Looking at the general relationship between sleep and predation visualized. Shows that a general relationship
#between sleep and predation can be seen. 

summary_pred_mams <- mams %>% group_by(predation) %>% summarize(n = n(), sd = sd(sleep), mean = mean(sleep))
summary_pred_mams
names(summary_pred_mams)

ggplot(summary_pred_mams) +
  geom_bar( aes(x=predation, y=mean), stat="identity", fill="blue", alpha=0.7) +
  geom_errorbar( aes(x=predation, ymin=mean-sd, ymax=mean+sd), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  ggtitle("Average Amount of Sleep for Each Predation Level") +
  geom_text(aes(label=n, x=predation, y=1), position=position_dodge(width=0.9), size=4) +
  geom_text(aes(label="n =", x=.8, y=1), position=position_dodge(width=0.9), size=4) +
  xlab("Predation Level") + 
  ylab("Mean Hours of Sleep") + 
  labs(stat = "Standard Deviation")

cor(mammalsleep1)

#--------------------------------------------------------------------------------------------------------------

#Assumption of Normality Amoung Residuals
hist(test.lm$residuals, breaks = 10)
sd(test.lm$residuals) #.094108
length(test.lm$residuals) # n = 42

plot(density(test.lm$residuals), pch=".", main = ) #Looks normal

density(test.lm$residuals)

plot(fitted(test.lm), residuals(test.lm))
hist(test.lm$residuals, breaks = 20, main = "Histogram of Residuals", xlab = "residuals")
ggplot(data=mams, aes(fitted(test.lm), resid(test.lm))) +geom_point()
ggplot(data=test.lm, aes(fitted(test.lm), resid(test.lm))) +geom_point() +
  xlab("Fitted") + ylab("Residuals")
qqnorm(residuals(test.lm), pch = 1, frame = TRUE)
qqline(residuals(test.lm),
       col = "blue")

? mammalsleep
summary(test.lm)

# log(sleep) ~ predation + log(body) + log(nondream) + log(gestation), data = mammalsleep1
mean(mammalsleep1$gestation)
mean(mammalsleep1$body)
x_star <- data.frame(predation = c(1,2,3,4,5), gestation = rep(1, 5), body = rep(1,5), nondream = rep(1,5))
predict(test.lm, x_star)
