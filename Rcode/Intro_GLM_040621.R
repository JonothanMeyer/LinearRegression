# General Linear Models: Examples and stuff
library(ggplot2)
library(gridExtra)



m = 5
k = 4
ssize<-m*k

x1 <- rep(seq(from=2, to = 10, by = (10-2)/(k-1)),m)

x1 <- sample(rep(seq(from=2, to = 10, by =(10-2)/(k-1)),m))
x2 <- sample(rep(seq(from=-50, to = 50, by =(50-(-50))/(k-1)),m))
x3 <- sample(rep(seq(from=0, to = 1, by =(1-0)/(k-1)),m))

y <- 1.944*x1-.04*x2+x3

h1 <- qplot(x1) + ggtitle("x1")
h2 <- qplot(x2) + ggtitle("x2")
h3 <- qplot(x3) + ggtitle("x3")

grid.arrange(h1,h2,h3, nrow=1)

ex.df <- data.frame(resp=y, fac1=x1, fac2=x2, fac3 =x3)
head(ex.df)

pairs(ex.df)

y.1.2.3.lm <- lm(resp~fac1+fac2+fac3, data = ex.df)

ggplot(data=ex.df, aes(fitted(y.1.2.3.lm), resid(y.1.2.3.lm))) +
  geom_point()
anova(y.1.2.3.4.lm)
summary(y.1.2.3.lm)


#w/noise --------------------------------------------------------
m = 5 #determines the sample size
k = 4
ssize<-m*k

x1 <- sample(rep(seq(from=2, to = 10, by =(10-2)/(k-1)),m))
x2 <- sample(rep(seq(from=-50, to = 50, by =(50-(-50))/(k-1)),m))
x3 <- sample(rep(seq(from=0, to = 5, by =(5-0)/(k-1)),m))

y <- 3*x1^2 -.04*x2 +x3^2 + rnorm(ssize, 0, 5)

#h1 <- qplot(x1) + ggtitle("x1")
#h2 <- qplot(x2) + ggtitle("x2")
#h3 <- qplot(x3) + ggtitle("x3")
#grid.arrange(h1,h2,h3, nrow=1)
ex.df <- data.frame(resp=y, fac1=x1, fac2=x2, fac3 =x3)
head(ex.df)
#pairs(ex.df)

y.1.2.3.lm <- lm(resp~fac1+fac2+fac3+fac2*fac3, data = ex.df)
summary(y.1.2.3.lm)

y.l1.2.3.lm <- lm(resp~I(fac1^2)+fac2+I(fac3^2), data=ex.df)
summary(y.l1.2.3.lm)



ggplot(data=ex.df, aes(fitted(y.l1.2.3.lm), resid(y.l1.2.3.lm))) +geom_point()

ggplot(data=ex.df, aes(fitted(y.1.2.3.lm), resid(y.1.2.3.lm))) +geom_point()

 anova(y.1.2.3.4.lm)
summary(y.1.2.3.lm)
