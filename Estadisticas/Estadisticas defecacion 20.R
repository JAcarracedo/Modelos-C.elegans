defecacion20est<-read.csv("Defecacion - Defecacion 20 outliers.csv",T)
attach(defecacion20est)
d20est<- defecacion20est
d20est

#Coeficiente de variacion por media
#dia 1
sd(dia1 , na.rm=TRUE)/
  mean(dia1, na.rm=TRUE) *100
#dia2
sd(dia2 , na.rm=TRUE)/
  mean(dia2 , na.rm=TRUE) *100
#dia3
sd(dia3 , na.rm=TRUE)/
  mean(dia3, na.rm=TRUE) *100
#dia4
sd(dia4 , na.rm=TRUE)/
  mean(dia4, na.rm=TRUE) *100
#dia5
sd(dia5 , na.rm=TRUE)/
  mean(dia5 , na.rm=TRUE) *100
#dia6
sd(dia6 , na.rm=TRUE)/
  mean(dia6, na.rm=TRUE) *100
#dia7
sd(dia7, na.rm=TRUE)/
  mean(dia7, na.rm=TRUE) *100
#dia8
sd(dia8 , na.rm=TRUE)/
  mean(dia8, na.rm=TRUE) *100
#dia9
sd(dia9 , na.rm=TRUE)/
  mean(dia9, na.rm=TRUE) *100
#dia10
sd(dia10 , na.rm=TRUE)/
  mean(dia10, na.rm=TRUE) *100

#coeficiente de dispersion por cuartil (paquete: cvcqv)
library(cvcqv)
#dia1
cqv_versatile(dia1, na.rm = TRUE, digits = 3, method = "basic")
#dia2
cqv_versatile(dia2, na.rm = TRUE, digits = 3, method = "basic")
#dia3
cqv_versatile(dia3, na.rm = TRUE, digits = 3, method = "basic")
#dia4
cqv_versatile(dia4, na.rm = TRUE, digits = 3, method = "basic")
#dia5
cqv_versatile(dia5, na.rm = TRUE, digits = 3, method = "basic")
#dia6
cqv_versatile(dia6, na.rm = TRUE, digits = 3, method = "basic")
#dia7
cqv_versatile(dia7, na.rm = TRUE, digits = 3, method = "basic")
#dia8
cqv_versatile(dia8, na.rm = TRUE, digits = 3, method = "basic")
#dia9
cqv_versatile(dia9, na.rm = TRUE, digits = 3, method = "basic")
#dia10
cqv_versatile(dia10, na.rm = TRUE, digits = 3, method = "basic")
#Desviacion estandar
sd(dia1 , na.rm=TRUE)
sd(dia2 , na.rm=TRUE)
sd(dia3 , na.rm=TRUE)
sd(dia4 , na.rm=TRUE)
sd(dia5 , na.rm=TRUE)
sd(dia6 , na.rm=TRUE)
sd(dia7 , na.rm=TRUE)
sd(dia8 , na.rm=TRUE)
sd(dia9 , na.rm=TRUE)
sd(dia10 , na.rm=TRUE)

#Medidas de centralidad
#Media
mean(dia1, na.rm=TRUE)
mean(dia2, na.rm=TRUE)
mean(dia3, na.rm=TRUE)
mean(dia4, na.rm=TRUE)
mean(dia5, na.rm=TRUE)
mean(dia6, na.rm=TRUE)
mean(dia7, na.rm=TRUE)
mean(dia8, na.rm=TRUE)
mean(dia9, na.rm=TRUE)
mean(dia10, na.rm=TRUE)
#media
median(dia1, na.rm=TRUE)
median(dia2, na.rm=TRUE)
median(dia3, na.rm=TRUE)
median(dia4, na.rm=TRUE)
median(dia5, na.rm=TRUE)
median(dia6, na.rm=TRUE)
median(dia7, na.rm=TRUE)
median(dia8, na.rm=TRUE)
median(dia9, na.rm=TRUE)
median(dia10, na.rm=TRUE)

#Intervalos de confianza
library(boot)
#nivel de confianza
CF <- .95
#dia1 
intervalosd1d20<-read.csv("intervalos defecacion 20 - Dia 1.csv",T)
attach(intervalosd1d20)
id1d20<- intervalosd1d20
id1d20
mean(dia1d20ci)
bootd1d20 <- boot(dia1d20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd1d20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia2
intervalosd2d20<-read.csv("intervalos defecacion 20 - Dia 2.csv",T)
attach(intervalosd2d20)
id2d20<- intervalosd2d20
id2d20
mean(dia2d20ci)
bootd2d20 <- boot(dia2d20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd2d20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia3
intervalosd3d20<-read.csv("intervalos defecacion 20 - Dia 3.csv",T)
attach(intervalosd3d20)
id3d20<- intervalosd3d20
id3d20
mean(dia3d20ci)
bootd3d20 <- boot(dia3d20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd3d20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia4
intervalosd4d20<-read.csv("intervalos defecacion 20 - Dia 4.csv",T)
attach(intervalosd4d20)
id4d20<- intervalosd4d20
id4d20
mean(dia4d20ci)
bootd4d20 <- boot(dia4d20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd4d20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia5
intervalosd5d20<-read.csv("intervalos defecacion 20 - Dia 5.csv",T)
attach(intervalosd5d20)
id5d20<- intervalosd5d20
id5d20
mean(dia5d20ci)
bootd5d20 <- boot(dia5d20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd5d20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia6
intervalosd6d20<-read.csv("intervalos defecacion 20 - Dia 6.csv",T)
attach(intervalosd6d20)
id6d20<- intervalosd6d20
id6d20
mean(dia6d20ci)
bootd6d20 <- boot(dia6d20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd6d20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia7
intervalosd7d20<-read.csv("intervalos defecacion 20 - Dia 7.csv",T)
attach(intervalosd7d20)
id7d20<- intervalosd7d20
id7d20
mean(dia7d20ci)
bootd7d20 <- boot(dia7d20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd7d20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia8
intervalosd8d20<-read.csv("intervalos defecacion 20 - Dia 8.csv",T)
attach(intervalosd8d20)
id8d20 <- intervalosd8d20
id8d20
mean(dia8d20ci)
bootd8d20 <- boot(dia8d20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd8d20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia9
intervalosd9d20<-read.csv("intervalos defecacion 20 - Dia 9.csv",T)
attach(intervalosd9d20)
id9d20 <- intervalosd9d20
id9d20
mean(dia9d20ci)
bootd9d20 <- boot(dia9d20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd9d20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia10
intervalosd10d20<-read.csv("intervalos defecacion 20 - Dia 10.csv",T)
attach(intervalosd10d20)
id10d20 <- intervalosd10d20
id10d20
mean(dia10d20ci)
bootd10d20 <- boot(dia10d20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd10d20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))

#tamaño de la muestra por media(paquete: samplingbook)
library(samplingbook)
CF <- .95
#Mitad de intervalo por dia 1-3
TID20 <- 2.5
#dia1 
sample.size.mean(TID20, sd(dia1d20ci), N = Inf, level = CF)
#dia2
sample.size.mean(TID20, sd(dia2d20ci), N = Inf, level = CF)
#dia3
sample.size.mean(TID20, sd(dia3d20ci), N = Inf, level = CF)
#dia4
sample.size.mean(TID20, sd(dia4d20ci), N = Inf, level = CF)
#dia5
sample.size.mean(TID20, sd(dia5d20ci), N = Inf, level = CF)
#dia6
sample.size.mean(TID20, sd(dia6d20ci), N = Inf, level = CF)
#dia7
sample.size.mean(TID20, sd(dia7d20ci), N = Inf, level = CF)
#dia8
sample.size.mean(TID20, sd(dia8d20ci), N = Inf, level = CF)
#dia9
sample.size.mean(TID20, sd(dia9d20ci), N = Inf, level = CF)
#dia10
sample.size.mean(TID20, sd(dia10d20ci), N = Inf, level = CF)

#Pruebas de normalidad
defecacion20out<-read.csv("Defecacion - Defecacion 20 outliers.csv",T)
attach(defecacion20out)
d20out<- defecacion20out
d20out
library(nortest)
#dia1
qqnorm(dia1)   
hist(dia1)
lillie.test(dia1)
shapiro.test(dia1)
#dia2
qqnorm(dia2)    
hist(dia2)
lillie.test(dia2)
shapiro.test(dia2)
#dia3
qqnorm(dia3)    
hist(dia3)
lillie.test(dia3)
shapiro.test(dia3)
#dia4
qqnorm(dia4)   
hist(dia4)
lillie.test(dia4)
shapiro.test(dia4)
#dia5
qqnorm(dia5)    
hist(dia5)
lillie.test(dia5)
shapiro.test(dia5)
#dia6
qqnorm(dia6)   
hist(dia6)
lillie.test(dia6)
shapiro.test(dia6)
#dia7
qqnorm(dia7)   
hist(dia7)
lillie.test(dia7)
shapiro.test(dia7)
#dia8
qqnorm(dia8)    
hist(dia8)
lillie.test(dia8)
shapiro.test(dia8)
#dia9
qqnorm(dia9)    
hist(dia9)
lillie.test(dia9)
shapiro.test(dia9)
#dia10
qqnorm(dia10)   
hist(dia10)
lillie.test(dia10)
shapiro.test(dia10)

#homocedasticidad
#Dia1
fligner.test(dia1~dia2)
fligner.test(dia1~dia3)
fligner.test(dia1~dia4)
fligner.test(dia1~dia5)
fligner.test(dia1~dia6)
fligner.test(dia1~dia7)
fligner.test(dia1~dia8)
fligner.test(dia1~dia9)
fligner.test(dia1~dia10)
#Dia2
fligner.test(dia2~dia3)
fligner.test(dia2~dia4)
fligner.test(dia2~dia5)
fligner.test(dia2~dia6)
fligner.test(dia2~dia7)
fligner.test(dia2~dia8)
fligner.test(dia2~dia9)
fligner.test(dia2~dia10)
#Dia3
fligner.test(dia3~dia4)
fligner.test(dia3~dia5)
fligner.test(dia3~dia6)
fligner.test(dia3~dia7)
fligner.test(dia3~dia8)
fligner.test(dia3~dia9)
fligner.test(dia3~dia10)
#Dia4
fligner.test(dia4~dia5)
fligner.test(dia4~dia6)
fligner.test(dia4~dia7)
fligner.test(dia4~dia8)
fligner.test(dia4~dia9)
fligner.test(dia4~dia10)
#dia5
fligner.test(dia5~dia6)
fligner.test(dia5~dia7)
fligner.test(dia5~dia8)
fligner.test(dia5~dia9)
fligner.test(dia5~dia10)
#dia6
fligner.test(dia6~dia7)
fligner.test(dia6~dia8)
fligner.test(dia6~dia9)
fligner.test(dia6~dia10)
#dia7
fligner.test(dia7~dia8)
fligner.test(dia7~dia9)
fligner.test(dia7~dia10)
#dia8
fligner.test(dia8~dia9)
fligner.test(dia8~dia10)
#dia9
fligner.test(dia9~dia10)

#todo
defecacion20AV<-read.csv("intervalos defecacion 20 - Anova.csv",T)
d20av<- defecacion20AV
(framed20 <- stack(d20av))
names(framed20) <- c("defecaciond20","Diad20")
attach(framed20)
head(framed20)
fligner.test(defecaciond20~Diad20)
#pruebas de wilcoxon 
#Dia 1 
wilcox.test(dia1, dia2)
wilcox.test(dia1, dia3)
wilcox.test(dia1, dia4)
wilcox.test(dia1, dia5)
wilcox.test(dia1, dia6)
wilcox.test(dia1, dia7)
wilcox.test(dia1, dia8)
wilcox.test(dia1, dia9)
wilcox.test(dia1, dia10)
#dia 2 
wilcox.test(dia2, dia3)
wilcox.test(dia2, dia4)
wilcox.test(dia2, dia5)
wilcox.test(dia2, dia6)
wilcox.test(dia2, dia7)
wilcox.test(dia2, dia8)
wilcox.test(dia2, dia9)
wilcox.test(dia2t, dia10)
#dia 3 
wilcox.test(dia3, dia4)
wilcox.test(dia3, dia5)
wilcox.test(dia3, dia6)
wilcox.test(dia3, dia7)
wilcox.test(dia3, dia8)
wilcox.test(dia3, dia9)
wilcox.test(dia3, dia10)
#dia 4
wilcox.test(dia4, dia5)
wilcox.test(dia4, dia6)
wilcox.test(dia4, dia7)
wilcox.test(dia4, dia8)
wilcox.test(dia4, dia9)
wilcox.test(dia4, dia10)
#dia 5
wilcox.test(dia5, dia6)
wilcox.test(dia5, dia7)
wilcox.test(dia5, dia8)
wilcox.test(dia5, dia9)
wilcox.test(dia5, dia10)
#dia 6
wilcox.test(dia6, dia7)
wilcox.test(dia6, dia8)
wilcox.test(dia6, dia9)
wilcox.test(dia6, dia10)
#dia 7
wilcox.test(dia7, dia8)
wilcox.test(dia7, dia9)
wilcox.test(dia7, dia10)
#dia 8
wilcox.test(dia8, dia9)
wilcox.test(dia8, dia10)
#dia 9
wilcox.test(dia9, dia10)

#ANOVA
summary(aov(Bombeod20~Diad20))
plot(aov(Bombeod20~Diad20))

#homocedasticidad de datos 
defecacion20V<-read.csv("intervalos defecacion 20 - fligner.csv",T)
d20v<- defecacion20V
(framevd20 <- stack(d20v))
names(framevd20) <- c("defecacionvd20","Diavd20")
attach(framevd20)
head(framevd20)
fligner.test(defecacionvd20~Diavd20)
boxplot(d20v[,1:5])