####Problem 3.22
Conduct=c(143,141,150,146,152,149,137,143,134,136,132,127,129,127,132,129)
CoatType=as.factor(sort(rep(1:4, 4)))
RunNum=sample(1:16,16)
DATAConduct=cbind(CoatType,Conduct)
d=t(matrix(Conduct,4,4))

Y.=apply(d,1,sum)
Y..=sum(Conduct)

plot.design(Conduct~CoatType)
boxplot(Conduct~CoatType)

#ANOVA
out1=aov(Conduct~CoatType)
summary(out1)

#Fitted & residuals
out1$fitted.values
out1$residuals

#Standardized residuals:
out1$residuals/sqrt(19.69)

#Test normal residuals
#Figure
qqnorm(out1$residuals)
qqline(out1$residuals
plot(out1$residuals[RunNum])
abline(h=0)
plot(out1$fit,out1$residuals)
abline(h=0)
plot(jitter(out1$fit, amount=.25),out1$residuals)
abline(h=0)
plot(sort(rep(1:4,4)),out1$residuals)
abline(h=0)

#TEST
shapiro.test(out1$residuals)

ks.test(out1$residuals,"pnorm",mean=mean(out1$residuals),sd=sqrt(var(out1$residuals)))

library(tseries)
jarque.bera.test(out1$residuals)

library(randtests)
turning.point.test(out1$residuals[RunNum])
runs.test(out1$residuals[RunNum], threshold=0)
###or runs test by
library(tseries)
ff<-sign(out1$residuals[RunNum])
faff=as.factor(ff)
runs.test(faff)

#Test variance
bartlett.test(Conduct~CoatType)