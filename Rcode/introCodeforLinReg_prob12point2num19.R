

#Below allows you to copy a dataset from excel using *copy* > dataset, then highlight the code below and run
#to inport dataset. It's name is b.data
words.data <- read.table(file = "clipboard", sep="\t", header = TRUE)

#Create a linear model data = data, y = response, x = name of predictor
(b.lm = lm(y~x, data = b.data))

#show the residuals
residuals(b.lm)

#make histogram
hist(residuals(b.lm))

#Create a scatterplot for the data (predictor first, response second)
plot(b.data$x,b.data$y)
#change the label names of the scatterplot
plot(b.data$x,b.data$y,
     xlab = "Predictor, Burner Area Liberation Rate",
     ylab = "Response, NO emisson rate")

#create least square line to the scatterplot
abline(b.lm)

#Normal Probability plot of residuals
qqnorm(rstandard(b.lm))



#Point estimate of sigma (note: this uses n-2 in std deviation denmoinator)
sigma(b.lm)

#Fun summary things (min, max, quads, mean)
summary(b.data)
#Fun summary for r squared and other fun games
summary(b.lm)

plot(b.data$x,residuals(b.lm))


#-----------------------Differenet Stuff, 2/3/21---------------------------
words.data <- read.table(file = "clipboard", sep="\t", header = TRUE)

words.data
plot(words.data)
plot(words.data$Words~words.data$Font)
words.data$Font<-(words.data$Font)^(-1)

words.data$Font<-exp(-words.data$Font)

plot(words.data$Words~words.data$Font)

words.lm<-lm(words.data$Words~words.data$Font)
summary(words.lm)



words.data$Words<-log(words.data$Words)
plot(words.data$Words~words.data$Font)
abline(words.lm).

words2.lm<-lm(Words~I(Font^(-1)), words.data)
plot(words2.lm)

summary(words2.lm)






