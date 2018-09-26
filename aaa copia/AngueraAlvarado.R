#Prova Avaluable 2, Guillem Anguera Alvarado
#Exercici2 Prova de Signes
df <- data.frame('AG' =c(7,9,6,5,4,9,7,5,8,9), 'DG' =c(8,9,5,5,3,8,6,7,9,8))
str(df)
diff <- df$AG - df$DG
sig <- sign(diff)
Spos <- sum(sig==1)
Sneg <- sum(sig==-1)
binom.test(Spos, Spos+Sneg, p=0.5, alternative = 'g') 
#Exercici2 Prova de Wilcoxon
diff2 <- diff[diff!=0]
abs_diff <- abs(diff2)
j <- rank(abs_diff)
j
s_j <- j*sign(diff2)
s_j
Tplus <- sum(s_j[s_j>0])
Tminus <- -sum(s_j[s_j<0])
Tplus
Tminus
qsignrank(0.001,length(diff2), log.p = FALSE)
w<-wilcox.test(df$AG,df$DG,  alternative = "g", paired = T, correct=F, conf.level = 0.95)
w
#Exercici3 (redwine)
setwd("/Users/guillemangueraalvarado/Desktop/UST/2n semestre/BIOEST AVAN/Practiques/")
df <- read.csv("red_wine.csv")
head(df)
str(df)
summary(df$alcohol)
boxplot(df)
f<-factor(df$alcohol,levels = c('low','high'),labels = c(2,1))
f
summary(f)
wilcox.test(df$quality~f,  alternative = "l", paired = FALSE,   correct=FALSE,  conf.level = 0.999)
#Exercici4
y <- c(1,12,41,124,242,303,241,169,126,111,79,48,29,14)
CHI2 <- sum((y-exp1*sum(y))^2 /(exp1*sum(y)))
CHI2
#Exercici5
setwd("/Users/guillemangueraalvarado/Desktop/UST/2n semestre/BIOEST AVAN/Practiques/")
df <- read.csv("weights_glucose.csv")
str(df)
x<-c(df$weights)
y<-c(df$glucose)
plot(y~x)
fit1<-lm(y~x)
fit1
sf1<-summary(fit1)
sf1
aa<-aov(y~x)
summary(aa)
df$weights
mean(df$weights)
summary(df$weights)
predict(lm(y~x),new, interval="confidence", level=.99)
