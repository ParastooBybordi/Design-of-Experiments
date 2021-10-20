#### problem 3.10####

a1=c(4.51,7.95,4.97,3.00,7.97,2.23,3.95,5.64,9.35,6.52,4.96,6.10,7.19,4.03,2.72,9.19,5.17,5.70,5.85,6.45)
a2=c(5.32, 6.00,5.12,7.08,5.48,6.52,4.09,6.28,7.77,5.68,8.47,4.58,4.11,5.72,5.91,6.89,6.99, 4.98,4.94,6.38)
a3=c(4.73, 5.81, 5.69, 3.86, 4.06, 6.56, 8.34,3.01, 6.71, 6.51, 1.70, 5.89, 6.55, 5.34, 5.88, 7.50, 3.28, 5.38, 7.30, 5.46)
a4=c(7.03,4.65,6.65,5.49,6.98,4.85,7.26,5.92,5.58,7.91,4.90,4.54,8.18,5.42,6.03,7.04,5.17,7.60,7.90,7.91)
observations_BMD<-c(a1,a2,a2,a3)
PEMF<-c(rep(1,20),rep(2,20),rep(3,20),rep(4,20))
PEMF1<-factor(PEMF)
out<-aov(observations_BMD~PEMF1)
library( multcomp)
contcom<-glht(out,linfct=mcp(PEMF1="Dunnett" ))
summary(contcom)

####Problem 3.14######
Observations<-c(7,7,15,11,9,12,17,12,18,18,14,19,19,18,18,19,25,22,19,23,7,10,11,15,11)
Cotton_Weight<-c(rep(15,5),rep(20,5),rep(25,5),rep(30,5),rep(35,5))
Cotton_Weight1<-factor(Cotton_Weight)
out1<-aov(Observations ~ Cotton_Weight1)
out1
summary(out1)
library( multcomp)
contcoms<-glht(out1,linfct=mcp(Cotton_Weight1="Dunnett" ))
summary(contcoms)
Cotton_Weight2<-factor(Cotton_Weight1,levels = c("30","35","25","20","15"))
out2<-aov(Observations ~Cotton_Weight2)
cot<-glht(out2,linfct=mcp(Cotton_Weight2="Dunnett" ))
summary(cot)

###### Problem 3.26##### HSD or TUKEY Test
Response_Type<-c(9,12,10,8,15,20,21,23,17,30,6,5,8,10,7)
Circuit_Type<-c(rep(1,5),rep(2,5),rep(3,5))
Circuit_Type1<-factor(Circuit_Type)
out3<-aov(Response_Type ~ Circuit_Type1)
summary(out3)
library(agricolae)
HSDCOM<-HSD.test(out3,"Circuit_Type1", alpha=0.01)
HSDCOM

##### Problem 3.28####
Noised_Observed<-c(19,20,19,30,8,80,61,73,56,80,47,26,25,35,50,95,46,83,78,97)
Circuit_Design<-c(rep(1,5),rep(2,5),rep(3,5),rep(4,5))
Circuit_Design1<-factor(Circuit_Design)
ANOVA<-aov(Noised_Observed ~ Circuit_Design1)
cotq<-glht(ANOVA,linfct=mcp(Circuit_Design1="Dunnett" ))
summary(cotq)
