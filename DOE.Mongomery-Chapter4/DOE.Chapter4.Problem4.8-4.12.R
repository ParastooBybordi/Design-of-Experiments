#### Problem 4.8
furance<-c(73,68,74,71,67,73,67,75,72,70,75,68,78,73,68,73,71,75,75,69)
rpm<-factor(sort(rep(LETTERS[1:4], each=5)))
block<-factor(rep(1:5,4))
z<-data.frame(furance,rpm,block)
furanceout<-aov(furance~rpm+block)
summary(furanceout)
library(agricolae)
out<-LSD.test(furanceout,"rpm",alpha = 0.05)
out
library(DescTools)
a<-PostHocTest(furanceout,which="rpm",method="lsd")
a
QQ<-qqnorm(furanceout$residuals)
plot(QQ$y,QQ$x)
abline(lm(QQ$x~QQ$y))
shapiro.test(furanceout$residuals)


eij<-round(furanceout$residuals,3)
Yhat<-furanceout$fitted.values
plot(Yhat,eij)
abline(h=0)


#### Problem 4.12
Hardness<-c(9.3,9.4,9.6,10,9.4,9.3,9.8,9.9,9.2,9.4,9.5,9.7,9.7,9.6,10,10.2)
Tip<-factor(sort(rep(LETTERS[1:4], each=4)))
Block<-factor(rep(1:4,4))
z<-data.frame(Hardness,Tip,Block)
Hardout<-aov(Hardness~Tip + Block)
library(agricolae)
LSD<-LSD.test( Hardout, "Tip", alpha = 0.05 )
LSD
eij<-round(Hardout$residuals,3)
Yhat<-Hardout$fitted.values
plot(Yhat,eij)
abline(h=0)

