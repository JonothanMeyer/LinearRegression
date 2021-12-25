#Project 5 - Lin Reg

#Exercise 1
#(From book, pg 23, problem 4): 4) The dataset 'prostate' comes from a study on 97 men with prostate cancer who were due
#to  receive a rradical prostatectomy. Fit a model with 'lpsa' as the response and lcavol as the predictor. Record the
#residual standard error and the r^2. Now add 'lweight', 'svi', 'lbph', 'age, 'lcp', 'pgg45', and 'gleason' to the
#model one at a time. For each model record the residual standard error and the r^2. Plot the trends in these 2 stats.

prostate
plot(lpsa~lcavol, data=prostate)

lpsa.lcavol.lm<-lm(lpsa~lcavol, data=prostate)
summary(lpsa.lcavol.lm) #r^2= .5394, residual-stan-error=.7875

lpsa.lcavol.lwight.lm<-lm(lpsa~lcavol+lweight, data=prostate)
summary(lpsa.lcavol.lwight.lm) #r^2=.5859, rse=.7506

lpsa.lcavol.lweight.svi.lm<-lm(lpsa~lcavol+lweight+svi, data=prostate)
summary(lpsa.lcavol.lweight.svi.lm) #r^2=.6264, rse=.7168

lpsa.lcavol.lweight.svi.lpbh.lm<-lm(lpsa~lcavol+lweight+svi+lbph, data=prostate)
summary(lpsa.lcavol.lweight.svi.lpbh.lm) #r^2=.6366, rse=.7108

lpsa.lcavol.lweight.svi.lbph.age.lm<-lm(lpsa~lcavol+lweight+svi+lbph+age, data=prostate)
summary(lpsa.lcavol.lweight.svi.lbph.age.lm) #r^2=.6441, rse=.7073

lpsa.lcavol.lweight.svi.lbph.age.lcp.lm<-lm(lpsa~lcavol+lweight+svi+lbph+age+lcp, data=prostate)
summary(lpsa.lcavol.lweight.svi.lbph.age.lcp.lm)# r^2=.6451, rse=.7102

lpsa.lcavol.lweight.svi.lbph.age.lcp.pgg45.lm<-lm(lpsa~lcavol+lweight+svi+lbph+age+lcp+pgg45, data=prostate)
summary(lpsa.lcavol.lweight.svi.lbph.age.lcp.pgg45.lm) #r^2=.6544, rse=.70448

lpsa.lcavol.lweight.svi.lbph.age.lcp.pgg45.gleason.lm<-lm(lpsa~lcavol+lweight+svi+lbph+age+lcp+pgg45+gleason, data=prostate)
summary(lpsa.lcavol.lweight.svi.lbph.age.lcp.pgg45.gleason.lm) #r^2=.6548, rse=.7084

rse_r2_his<- data.frame(num_predictors = c(1:8), 
              name_pred = c("lcavol", "+lweight", "+svi", "+lbph", "+age", "+lcp", "+pgg45", "+gleason"),
              r_squared = c(.5394, .5859, .6264, .6366, .6441, .6451, .6544, .6548),
              res_stan_error = c(.7875, .7506, .7168, .7108, .7073, .7102, .7048, .7084))
rse_r2_his

plot(r_squared~res_stan_error, data=rse_r2_his, xlab = "Residual Standard Error", ylab = "R Squared") #As residual standard error increases, r^2 decreases
plot(r_squared~num_predictors, data=rse_r2_his, xlab = "Number of Predictors", ylab = "R Squared") #As number of predictors were added to the linear model r^2 increased logarithmicly
plot(res_stan_error~num_predictors, data=rse_r2_his, xlab = "Number of Predictors", ylab = "Residual Standard Error") #As the number of predictors increased rse decrease, lim@.71

rse_r2_his
#----------------------Exercise 2 Below--------------------------------------------------------------------------
#Exercise 2 (6 points): Find five different sized vessels such as a cup or a pot or a vase
#in your house. Measure the weight (or width) of each vessel, the height of each vessel, and
#the volume of water each vessel holds as accurately as possible. Organize this data in a
#data frame in R, identifying volume as the response and the other two measurements as
#predictors.

Vessels <- data.frame(Vessel = c("Rx_bottle", "Seltzer_can", "Sm_hand_sntizer", "Sgr_srip_case", "Vapur_wtr_btl"),
                      Volume_ml = c(47.1, 355, 59, 24.2, 500),
                      Weight_g = c(7.6, 14.6, 10.6, 9.1, 16.3),
                      Height_cm = c(7, 12.4, 9.7, 5, 19.3))

Vessels
plot(Volume_ml~Weight_g, data = Vessels)
plot(Volume_ml~Height_cm, data = Vessels)
abline()

Vessels.lm <- lm(Volume_ml~Weight_g+Height_cm, data = Vessels)
anova(Vessels.lm)
summary(Vessels.lm)
Vessels.lm

#(1) Identify X, Y, Yhat, and e in this context; include the, R code you use.
X <- matrix(c(1,1,1,1,1, 7.6,14.6,10.6,9.1,16.3, 7.0,12.4,9.7,5.0,19.3), nrow=5, ncol=3) 
X.another <- cbind(rep(1,5), Vessels$Weight_g, Vessels$Height_cm)
X.matrix.model <- model.matrix(Vessels.lm)
X.matrix.model
X.another
X
Y <- matrix(c(47.1,355,59,24.2,500), nrow=5, ncol=1)
Y
m.beta  <- matrix(c(-377.10, 35.50, 15.07), ncol=1)
m.beta
H<-(X%*%inv(t(X)%*%X))%*%t(X)
H
Yhat <- H%*%Y
Yhat
Yhat.diffmeth <- fitted(Vessels.lm) #another way to use an R command to get Yhat
Yhat.diffmeth
e <- Y-Yhat.diffmeth
e
#(2) Identify H, SSR, SSE, SST, MSR, MSE, s2 and R2 in this context; include the R code you use.
cor(X)

H<-(X%*%inv(t(X)%*%X))%*%t(X)
H
nsmp = 5
J<-matrix(1, nrow=nsmp, ncol=nsmp)
SST<-t(Y)%*%(diag(nsmp)-1/nsmp*J)%*%Y
SSE<-t(Y)%*%(diag(nsmp)-H)%*%Y
SSR<-t(Y)%*%(H-1/nsmp*J)%*%Y #Sum of Square Regression, add up sum of squares associated with factors
anova(Vessels.lm)
DiffSSR <- ((173207 + 4294)/2)/5323 #add Sum sq column from anova, F statistic from summary(Vessels.lm) is the same as this value
DiffSSR
summary(Vessels.lm)
dataframe <- data.frame(SSR = c(SSR), SSE = c(SSE), SST = c(SST), s = c(72.95), s_squared = c(5323), r_squared = c(.9434))
dataframe
H
SST
s_sqr_another <- 5323*(diag(5)-H) #s^2 = MSE(I-H)
s_sqr_another

s_sqr_b.matrix.Vessels <- 5323*inv(t(X)%*%X) 
s_sqr_b.matrix.Vessels

aov(Vessels.lm)
summary(Vessels.lm)
anova(Vessels.lm)

Vessels_nox2.lm <- lm(Volume_ml~Weight_g, data = Vessels) #Looking at the linear model without height
Vessels.lm
Vessels_nox2.lm

plot(residuals(Vessels.lm)~fitted(Vessels.lm)) #'fitted' shows you kind of the amagmation of all the predictors against the residuals vs just one
#(3) Interpret R2 in terms of the real world variables.




#-----Below is used for testing to better understanding things--------------------------
new_vessles <- data.frame(Volume = c(10, 20, 30, 40, 50),
                          Weight = c(2, 4, 6, 8, 100),
                          Height = c(8, 1, 7, 6, 10))
pracLM <- lm(Volume~Weight*Height, data = new_vessles)
summary(pracLM)
anova(pracLM)
#----------------------------------------------------------------------------------

#Exercise 3 (10 points): Complete Textbook Exercise 7 from Chapter 2 (page 31), stated
#as follows: An experiment was conducted to determine the effect of four factors on the
#resistivity of a semiconductor wafer. The data is found in wafer where each of the four
#factors is coded as 􀀀 or + depending on whether the low or the high setting for that factor
#was used. Fit the linear model resist ~ x1 + x2 + x3 + x4.

library(faraway)
head(wafer)
wafer.lm <- lm(resist~x1+x2+x3+x4, data =wafer)
summary(wafer.lm, correlation = FALSE)

#(1) Extract the X matrix using the model.matrix function. 
#Examine this to determine how the low and high levels have been coded in the model.

X <- model.matrix(wafer.lm)
X #low and high levels were coded as either 0/1, 0 for low, 1 for high

#(2) Compute the correlation in the X matrix. Why are there some missing values
#in the matrix?

summary(wafer.lm, correlation = TRUE) # -.45
cor(X) #There are some missing values in this matrix, denoted 'NA' because it's pointless to ask how a variable correlates with
#itself. Its a non-sensical question because any given variable corrilates perfectly with itself. (Think is is a wrong interpertation
# (actually)
cor(X)
X

wafer.lm
#(3) What difference in resistance is expected when moving from the low to the
#high level of x1?
# When moving from low to high level of x1 the difference in resitance is calculated by (1 * 25.76) - (0 * 25.76) = 25.76. When moving
# from low level to high level for x1 we can expect 25.76 resistance according to our linear model.


#(4) Refit the model without x4 and examine the regression coeficients and standard
# errors? What stayed the the same as the original fit and what changed?
wafer_nox4.lm <- lm(resist~x1+x2+x3, data = wafer)
summary(wafer_nox4.lm, correlation = TRUE)
wafer_nox4.lm
wafer.lm
#Stayed the same: x1, x2, x3 coefficients. Intercept is lower by 7 units.


#(5) Explain how the change in the regression coeficients is related to the correlation
#matrix of X.
newX <- model.matrix(wafer_nox4.lm)
wafer.lm
wafer_nox4.lm
#since there is 0 correlation between the any of the predictor variables we wouldn't expect there
#to be any change in the coefficents (other than the intercept) by removing x4 from the linear model



#-------------------Aside to look at s^2 matrix using wafer data-----------
X <- model.matrix(wafer.lm)
H<-(X%*%inv(t(X)%*%X))%*%t(X)

#s^2 = MSE(I-H)
summary(wafer.lm) #MSE = 26.42^2
MSE <- 26.42^2
ssquar.waf.matrix <- MSE*(diag(1,16)-H)
ssquar.waf.matrix
names(wafer)
s_sqr_b.matrix <- MSE*inv(t(X)%*%X) 
s_sqr_b.matrix #pxp matrix, (p-1) = num of predictor variables, each
#value in this matrix represents the standard deviation at each predicted predictor value


