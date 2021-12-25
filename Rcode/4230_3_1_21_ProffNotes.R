library(matlib)
library(stats)
#________________________________________________________________________
# Computing the error if beta values are known, as in lecture video "Matrix Form 
# for Multiple Regression" from Module 6
Y<-matrix(c(34.9, 27.7,35.9,30.1), ncol=1)
X<-matrix(c(1,1,1,1, 18,20,28,36, 0,1,7,8), nrow=4, ncol=3)
m.beta<-matrix(c(49.5,-1,2), ncol=1)

# Y=Xb+e so the error terms associated with data matrix X and parameter matrix
# m.beta is given by,
Y-X%*%m.beta

#________________________________________________________________________
# Computing the error when beta is estimated using the sample data
Y<-matrix(c(34.9, 27.7,35.9,30.1), ncol=1)
Y
X<-matrix(c(1,1,1,1, 18,20,28,36, 0,1,7,8), nrow=4, ncol=3)


# "manual" parameter estimates (consequence of maximizing likelihood function and 
# applying matrix algebra)
b<- inv(t(X)%*%X)%*%(t(X))%*%Y
H<- X%*%inv(t(X)%*%X)%*%t(X)
hat.Y<- H%*%Y
res.e<-Y-X%*%b

J<-matrix(1, nrow=4, ncol=4)
SST<-t(Y)%*%(diag(4)-1/4*J)%*%Y
SSE<-t(Y)%*%(diag(4)-H)%*%Y
SSR<-t(Y)%*%(H-1/4*J)%*%Y
matrix(c(SSR,SSE,SST), nrow=3)

# parameter estimates using R stats functions (more precise because numeric
# precision is sacrificed when using the linear algebra commands above; in particular,
# the inverse matrix operation is sensitive to the numerical precision of the entries)

lm(Y~X[,2]+X[,3])
summary(lm(Y~X[,2]+X[,3]))
anova(lm(Y~X[,2]+X[,3]))

#________________________________________________________________________
# Scatterplots
# 3d looks cool but can be hard to evaluate unless it is made dynamic ("bird's eye
# animation") or is colored/contoured to add relief
library(scatterplot3d)
scatterplot3d(Y~X[,2]+X[,3])

# Scatterplots of all pairs of variables are typically plotted unless a large number
# of predictors makes this intractable
pairs(cbind(Y,X[,2],X[,3]))


#________________________________________________________________________
# Trivial Example (s^2(e))sigma^2==0).  What should SSE be in this case? (try to
# guess before you compute it). What happens when you increase sample size?

nsmp<-100
x1<-rnorm(nsmp,25,3)
x2<-c(1:nsmp)/nsmp*8
X<- cbind(rep(1,nsmp),x1, x2)
Y<- -20 + 2*X[,2] - 1/3*X[,3] 

H<-(X%*%inv(t(X)%*%X))%*%t(X)
J<-matrix(1, nrow=nsmp, ncol=nsmp)
SST<-t(Y)%*%(diag(nsmp)-1/nsmp*J)%*%Y
SSE<-t(Y)%*%(diag(nsmp)-H)%*%Y
SSR<-t(Y)%*%(H-1/nsmp*J)%*%Y
matrix(c(SSR,SSE,SST), nrow=3) 

ex2.lm<-lm(Y~X[,2]+X[,3])
summary(ex2.lm)
anova(ex2.lm)

library(scatterplot3d)
scatterplot3d(Y~X[,2]+X[,3])

pairs(cbind(Y,X[,2],X[,3]))
cor(X)


#________________________________________________________________________
# Non-trivial Example (sigma^2=2).   What should MSE be in this case? (try to
# guess before you compute it). What happens when you increase sample size? 
# What happens when you increase standard deviation?

nsmp<-30

X<- cbind(rep(1,nsmp),rnorm(nsmp,25,3), 8*c(1:nsmp)/nsmp)
Y<- -20 + 2*X[,2] - 1/3*X[,3] + rnorm(nsmp,0,2)

H<-(X%*%inv(t(X)%*%X))%*%t(X)
J<-matrix(1, nrow=nsmp, ncol=nsmp)
SST<-t(Y)%*%(diag(nsmp)-1/nsmp*J)%*%Y
SSE<-t(Y)%*%(diag(nsmp)-H)%*%Y
SSR<-t(Y)%*%(H-1/nsmp*J)%*%Y
matrix(c(SSR,SSE,SST), nrow=3)

ex3.lm<-lm(Y~X[,2]+X[,3])
summary(ex3.lm)
anova(ex3.lm)

library(scatterplot3d)
scatterplot3d(Y~X[,2]+X[,3])

pairs(cbind(Y,X[,2],X[,3]))
cor(X)

