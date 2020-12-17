
defecacion15<-read.csv("Defecacion - Defecacion 15 boxplots.csv",T)
attach(defecacion15)
d15<- defecacion15
d15
#boxplot de grafica 
boxplot(d15[,1:10])
#box plots individuales
#dia 1
boxplot(dia1d15, xlab = "dia1", ylab = "defecacion15") 
boxplot.stats(dia1d15, do.conf = TRUE, do.out = TRUE)
#dia2
boxplot(dia2b15, xlab = "dia2", ylab = "defecacion15") 
boxplot.stats(dia2d15, do.conf = TRUE, do.out = TRUE)
#dia3
boxplot(dia3d15, xlab = "dia3", ylab = "defecacion15") 
boxplot.stats(dia3d15, do.conf = TRUE, do.out = TRUE)
#dia4
boxplot(dia4d15, xlab = "dia4", ylab = "defecacion15")
boxplot.stats(dia4d15, do.conf = TRUE, do.out = TRUE)
#dia5
boxplot(dia5d15, xlab = "dia5", ylab = "defecacion15")
boxplot.stats(dia5d15, do.conf = TRUE, do.out = TRUE)
#dia6
boxplot(dia6d15, xlab = "dia6", ylab = "defecacion15") 
boxplot.stats(dia6d15, do.conf = TRUE, do.out = TRUE)
#dia7
boxplot(dia7d15, xlab = "dia7", ylab = "defecacion15") 
boxplot.stats(dia7d15, do.conf = TRUE, do.out = TRUE)
#dia8
boxplot(dia8d15, xlab = "dia8", ylab = "defecacion15") 
boxplot.stats(dia8d15, do.conf = TRUE, do.out = TRUE)
#dia9
boxplot(dia9d15, xlab = "dia9", ylab = "defecacion15") 
boxplot.stats(dia9d15, do.conf = TRUE, do.out = TRUE)
#dia10
boxplot(dia10d15, xlab = "dia10", ylab = "defecacion15")
boxplot.stats(dia10d15, do.conf = TRUE, do.out = TRUE)

#boxplot sin outliers
defecacion15out<-read.csv("Defecacion - Defecacion 15 outliers.csv",T)
attach(defecacion15out)
d15out<- defecacion15out
d15out
boxplot(d15out[,1:10])

#boxplot sin outliers2
defecacion15out2<-read.csv("Defecacion - Defecacion 15 outliers2.csv",T)
attach(defecacion15out2)
d15out2<- defecacion15out2
d15out2
boxplot(d15out2[,1:10])

#boxplot sin outliers3
defecacion15out3<-read.csv("Defecacion - Defecacion 15 outliers3.csv",T)
attach(defecacion15out3)
d15out3<- defecacion15out3
d15out3
boxplot(d15out3[,1:10])

#graficas sin outliers
#paquetes
library(devtools)
library(githubinstall)
library(ggpmisc)
defecacion15gout<-read.csv("Defecacion - D15grafica outliers.csv",T)
attach(defecacion15gout)
d15gout<- defecacion15gout
gd15out <- ggplot(d15gout, aes(x = diad15out, y = defecacion15out)) 
DEFECACION15OUT<- gd15out + geom_point() + geom_smooth(method = "lm") 
DEFECACION15OUT
DEFECACION15OUTloess<-gd15out + geom_point()+geom_smooth(method = "loess")
DEFECACION15OUTloess

