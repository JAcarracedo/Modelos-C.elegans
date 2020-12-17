
defecacion20<-read.csv("Defecacion - Defecacion 20 boxplots.csv",T)
attach(defecacion20)
d20<- defecacion20
d20
#boxplot de grafica 
boxplot(d20[,1:10])
#box plots individuales
#dia 1
boxplot(dia1d20, xlab = "dia1", ylab = "defecacion20") 
boxplot.stats(dia1d20, do.conf = TRUE, do.out = TRUE)
#dia2
boxplot(dia2b20, xlab = "dia2", ylab = "defecacion20") 
boxplot.stats(dia2d20, do.conf = TRUE, do.out = TRUE)
#dia3
boxplot(dia3d20, xlab = "dia3", ylab = "defecacion20") 
boxplot.stats(dia3d20, do.conf = TRUE, do.out = TRUE)
#dia4
boxplot(dia4d20, xlab = "dia4", ylab = "defecacion20")
boxplot.stats(dia4d20, do.conf = TRUE, do.out = TRUE)
#dia5
boxplot(dia5d20, xlab = "dia5", ylab = "defecacion20")
boxplot.stats(dia5d20, do.conf = TRUE, do.out = TRUE)
#dia6
boxplot(dia6d20, xlab = "dia6", ylab = "defecacion20") 
boxplot.stats(dia6d20, do.conf = TRUE, do.out = TRUE)
#dia7
boxplot(dia7d20, xlab = "dia7", ylab = "defecacion20") 
boxplot.stats(dia7d20, do.conf = TRUE, do.out = TRUE)
#dia8
boxplot(dia8d20, xlab = "dia8", ylab = "defecacion20") 
boxplot.stats(dia8d20, do.conf = TRUE, do.out = TRUE)
#dia9
boxplot(dia9d20, xlab = "dia9", ylab = "defecacion20") 
boxplot.stats(dia9d20, do.conf = TRUE, do.out = TRUE)
#dia10
boxplot(dia10d20, xlab = "dia10", ylab = "defecacion20")
boxplot.stats(dia10d20, do.conf = TRUE, do.out = TRUE)

#boxplot sin outliers
defecacion20out<-read.csv("Defecacion - Defecacion 20 outliers.csv",T)
attach(defecacion20out)
d20out<- defecacion20out
d20out
boxplot(d20out[,1:10])

#boxplot sin outliers 2
defecacion20out2<-read.csv("Defecacion - Defecacion 20 outliers2.csv",T)
attach(bdefecacion20out2)
d20out2<- defecacion20out2
d20out2
boxplot(d20out2[,1:10])

#graficas sin outliers
#paquetes
library(devtools)
library(githubinstall)
library(ggpmisc)
defecacion20gout<-read.csv("Defecacion - D20grafica outliers.csv",T)
attach(defecacion20gout)
d20gout<- defecacion20gout
gd20out <- ggplot(d20gout, aes(x = diad20out, y = defecacion20out)) 
DEFECACION20OUT<- gd20out + geom_point() + geom_smooth(method = "lm") 
DEFECACION20OUT
DEFECACION20OUTloess<-gd20out + geom_point()+geom_smooth(method = "loess")
DEFECACION20OUTloess