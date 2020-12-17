
bombeo15<-read.csv("15 grados - bombeo 15.csv",T)
attach(bombeo15)
b15<- bombeo15
b15
#boxplot de grafica 
boxplot(b15[,2:11])
#box plots individuales
#dia 1
boxplot(dia1b15, xlab = "dia1", ylab = "bombeo15") 
boxplot.stats(dia1b15, do.conf = TRUE, do.out = TRUE)
#dia2
boxplot(dia2b15, xlab = "dia2", ylab = "bombeo15") 
boxplot.stats(dia2b15, do.conf = TRUE, do.out = TRUE)
#dia3
boxplot(dia3b15, xlab = "dia3", ylab = "bombeo15") 
boxplot.stats(dia3b15, do.conf = TRUE, do.out = TRUE)
#dia4
boxplot(dia4b15, xlab = "dia4", ylab = "bombeo15")
boxplot.stats(dia4b15, do.conf = TRUE, do.out = TRUE)
#dia5
boxplot(dia5b15, xlab = "dia5", ylab = "bombeo15")
boxplot.stats(dia5b15, do.conf = TRUE, do.out = TRUE)
#dia6
boxplot(dia6b15, xlab = "dia6", ylab = "bombeo15") 
boxplot.stats(dia6b15, do.conf = TRUE, do.out = TRUE)
#dia7
boxplot(dia7b15, xlab = "dia7", ylab = "bombeo15") 
boxplot.stats(dia7b15, do.conf = TRUE, do.out = TRUE)
#dia8
boxplot(dia8b15, xlab = "dia8", ylab = "bombeo15") 
boxplot.stats(dia8b15, do.conf = TRUE, do.out = TRUE)
#dia9
boxplot(dia9b15, xlab = "dia9", ylab = "bombeo15") 
boxplot.stats(dia9b15, do.conf = TRUE, do.out = TRUE)
#dia10
boxplot(dia10b15, xlab = "dia10", ylab = "bombeo15")
boxplot.stats(dia10b15, do.conf = TRUE, do.out = TRUE)

#boxplot sin outliers
bombeo15out<-read.csv("15 grados - bombeo 15 outliers.csv",T)
attach(bombeo15out)
b15out<- bombeo15out
b15out
boxplot(b15out[,2:11])

#boxplot sin outliers 2
bombeo15out2<-read.csv("15 grados - bombeo 15 outliers2.csv",T)
attach(bombeo15out2)
b15out2<- bombeo15out2
b15out2
boxplot(b15out2[,2:11])

#boxplot sin aoutliers 3
bombeo15out3<-read.csv("15 grados - bombeo 15 outliers3.csv",T)
attach(bombeo15out2)
b15out3<- bombeo15out3
b15out3
boxplot(b15out3[,2:11])

#graficas sin outliers
#paquetes
library(devtools)
library(githubinstall)
library(ggpmisc)
bombeo15gout<-read.csv("15 grados - bombeo 15 grafica outliers3.csv",T)
attach(bombeo15gout)
b15gout<- bombeo15gout
b15gout$gusanob1520out <- as.factor(b15gout$gusanob1520out)
g1520out <- ggplot(b15gout, aes(x = diab1520out, y = bombeo1520out)) 
BOMBEO1520OUT<- g1520out + geom_point() + geom_smooth(method = "lm") + stat_fit_tb() 
BOMBEO1520OUT 
Bombeo1520OUTloess<-g1520out + geom_point()+geom_smooth(method = "loess") 
Bombeo1520OUTloess