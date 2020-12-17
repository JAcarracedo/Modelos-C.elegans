
bombeo20<-read.csv("20 Grados - Bombeo 20.csv",T)
attach(bombeo20)
b20<- bombeo20
b20
#boxplot de grafica 
boxplot(b20[, 2:11])
#box plots individuales
#dia 1
boxplot(dia1b20, xlab = "dia1", ylab = "bombeo20") 
boxplot.stats(dia1b20, do.conf = TRUE, do.out = TRUE)
#dia2
boxplot(dia2b20, xlab = "dia2", ylab = "bombeo20") 
boxplot.stats(dia2b20, do.conf = TRUE, do.out = TRUE)
#dia3
boxplot(dia3b20, xlab = "dia3", ylab = "bombeo20") 
boxplot.stats(dia3b20, do.conf = TRUE, do.out = TRUE)
#dia4
boxplot(dia4b20, xlab = "dia4", ylab = "bombeo20")
boxplot.stats(dia4b20, do.conf = TRUE, do.out = TRUE)
#dia5
boxplot(dia5b20, xlab = "dia5", ylab = "bombeo20")
boxplot.stats(dia5b20, do.conf = TRUE, do.out = TRUE)
#dia6
boxplot(dia6b20, xlab = "dia6", ylab = "bombeo20") 
boxplot.stats(dia6b20, do.conf = TRUE, do.out = TRUE)
#dia7
boxplot(dia7b20, xlab = "dia7", ylab = "bombeo20") 
boxplot.stats(dia7b20, do.conf = TRUE, do.out = TRUE)
#dia8
boxplot(dia8b20, xlab = "dia8", ylab = "bombeo20") 
boxplot.stats(dia8b20, do.conf = TRUE, do.out = TRUE)
#dia9
boxplot(dia9b20, xlab = "dia9", ylab = "bombeo20") 
boxplot.stats(dia9b20, do.conf = TRUE, do.out = TRUE)
#dia10
boxplot(dia10b20, xlab = "dia10", ylab = "bombeo20")
boxplot.stats(dia10b20, do.conf = TRUE, do.out = TRUE)

#boxplot sin outliers
bombeo20out<-read.csv("20 Grados - bombeo 20 outliers.csv",T)
attach(bombeo20out)
b20out<- bombeo20out
b20out
boxplot(b20out[,2:11])

#boxplot sin outliers 2
bombeo20out2<-read.csv("20 Grados - bombeo 20 outliers2.csv",T)
attach(bombeo20out2)
b20out2<- bombeo20out2
b20out2
boxplot(b20out2[,2:11])

#graficas sin outliers
#paquetes
library(devtools)
library(githubinstall)
library(ggpmisc)
bombeo20go<-read.csv("20 Grados - bombeo 20 grafica out2.csv",T)
attach(bombeo20go)
b20go<- bombeo20go
b20go$gusanob2020o2 <- as.factor(b20go$gusanob2020o2)
g2020o <- ggplot(b20go, aes(x = diab2020outo2, y = bombeo.2020outo2)) 
BOMBEO2020OUT<- g2020o + geom_point() + geom_smooth(method = "lm") 
BOMBEO2020OUT
Bombeo2020OUTloess<-g2020o + geom_point()+geom_smooth(method = "loess")
Bombeo2020OUTloess

