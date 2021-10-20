DATA27=read.csv("C:/Users/NJ/Desktop/data/problem27.csv", header = TRUE)
DATA27
attach(DATA27)
Batch=as.factor(DATA27$Batch)
Day=as.factor(DATA27$Day)
Catalyst=as.factor(DATA27$Catalyst)
CPOUT=aov(Time~Catalyst+Day+Batch)
summary(CPOUT)

DATA27$fitted=CPOUT$fitted.values
DATA27
DATA27$residuals=CPOUT$residuals
DATA27
attach(DATA27)
QQ<-qqnorm(residuals,plot=F)
plot(QQ$y,QQ$x)
abline(lm(QQ$x~QQ$y))

shapiro.test(residuals)
library(tseries)
jarque.bera.test(residuals)

plot(fitted,residuals/sqrt(3.13))
abline(h=0)

bartlett.test(Time~Catalyst)
library(car)
leveneTest(Time~Catalyst)

library(DescTools)
PostHocTest(CPOUT,which="Catalyst",method="duncan")
PostHocTest(CPOUT,which="Catalyst",method="lsd")
PostHocTest(CPOUT,which="Catalyst",method="hsd")
library(multcomp)
DunHar=glht(CPOUT,linfct=mcp(Catalyst="Dunnett"))
summary(DunHar)

DATAMIS27=DATA27[-18,1:4]
TimeMIS=DATAMIS27$Time
BatchMIS=as.factor(DATAMIS27$Batch)
DayMIS=as.factor(DATAMIS27$Day)
CatalystMIS=as.factor(DATAMIS27$Catalyst)
CPMISOUT=aov(TimeMIS~CatalystMIS+DayMIS+BatchMIS)
summary(CPMISOUT)
c=aggregate(TimeMIS~CatalystMIS,FUN="sum")
d=aggregate(TimeMIS~DayMIS,FUN="sum")
b=aggregate(TimeMIS~BatchMIS,FUN="sum")
YTO=sum(TimeMIS)
YHAT354=(5*(b$TimeMIS[3]+c$TimeMIS[5]+d$TimeMIS[4])-2*YTO)/(3*4)
DATAM27=DATA27
DATAM27[18,4]=YHAT354
attach(DATAM27)
Batch=as.factor(DATAM27$Batch)
Day=as.factor(DATAM27$Day)
Catalyst=as.factor(DATAM27$Catalyst)
CPMOUT=aov(Time~Catalyst+Day+Batch)
summary(CPMOUT)
