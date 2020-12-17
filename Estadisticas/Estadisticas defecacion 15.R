#NOTA: Limpiar ambiente antes de correr
#pruebas estadisticas defecacion 15
defecacion15est<-read.csv("Defecacion - Defecacion 15 outliers.csv",T)
attach(defecacion15est)
d15est<- defecacion15est
d15est

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
CF<- .95
#dia1 
intervalosd1d15<-read.csv("intervalos defecacion 15 - Dia 1.csv",T)
attach(intervalosd1d15)
id1d15<- intervalosd1d15
id1d15
mean(dia1d15ci)
bootd1d15 <- boot(dia1d15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd1d15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia2
intervalosd2d15<-read.csv("intervalos defecacion 15 - Dia 2.csv",T)
attach(intervalosd2d15)
id2d15<- intervalosd2d15
id2d15
mean(dia2d15ci)
bootd2d15 <- boot(dia2d15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd2d15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia3
intervalosd3d15<-read.csv("intervalos defecacion 15 - Dia 3.csv",T)
attach(intervalosd3d15)
id3d15<- intervalosd3d15
id3d15
mean(dia3d15ci)
bootd3d15 <- boot(dia3d15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd3d15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia4
intervalosd4d15<-read.csv("intervalos defecacion 15 - Dia 4.csv",T)
attach(intervalosd4d15)
id4d15<- intervalosd4d15
id4d15
mean(dia4d15ci)
bootd4d15 <- boot(dia4d15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd4d15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia5
intervalosd5d15<-read.csv("intervalos defecacion 15 - Dia 5.csv",T)
attach(intervalosd5d15)
id5d15<- intervalosd5d15
id5d15
mean(dia5d15ci)
bootd5d15 <- boot(dia5d15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd5d15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia6
intervalosd6d15<-read.csv("intervalos defecacion 15 - Dia 6.csv",T)
attach(intervalosd6d15)
id6d15<- intervalosd6d15
id6d15
mean(dia6d15ci)
bootd6d15 <- boot(dia6d15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd6d15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia7
intervalosd7d15<-read.csv("intervalos defecacion 15 - Dia 7.csv",T)
attach(intervalosd7d15)
id7d15<- intervalosd7d15
id7d15
mean(dia7d15ci)
bootd7d15 <- boot(dia7d15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd7d15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia8
intervalosd8d15<-read.csv("intervalos defecacion 15 - Dia 8.csv",T)
attach(intervalosd8d15)
id8d15 <- intervalosd8d15
id8d15
mean(dia8d15ci)
bootd8d15 <- boot(dia8d15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd8d15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia9
intervalosd9d15<-read.csv("intervalos defecacion 15 - Dia 9.csv",T)
attach(intervalosd9d15)
id9d15 <- intervalosd9d15
id9d15
mean(dia9d15ci)
bootd9d15 <- boot(dia9d15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd9d15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia10
intervalosd10d15<-read.csv("intervalos defecacion 15 - Dia 10.csv",T)
attach(intervalosd10d15)
id10d15 <- intervalosd10d15
id10d15
mean(dia10d15ci)
bootd10d15 <- boot(dia10d15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd10d15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))

#tamaño de la muestra por media(paquete: samplingbook)
 library(samplingbook)
CF<- .95
#Mitad de intervalo por dia 1-3
TID15 <- 2.5
#dia1 
sample.size.mean(TID15, sd(dia1d15ci), N = Inf, level = CF)
#dia2
sample.size.mean(TID15, sd(dia2d15ci), N = Inf, level = CF)
#dia3
sample.size.mean(TID15, sd(dia3d15ci), N = Inf, level = CF)
#dia4
sample.size.mean(TID15, sd(dia4d15ci), N = Inf, level = CF)
#dia5
sample.size.mean(TID15, sd(dia5d15ci), N = Inf, level = CF)
#dia6
sample.size.mean(TID15, sd(dia6d15ci), N = Inf, level = CF)
#dia7
sample.size.mean(TID15, sd(dia7d15ci), N = Inf, level = CF)
#dia8
sample.size.mean(TID15, sd(dia8d15ci), N = Inf, level = CF)
#dia9
sample.size.mean(TID15, sd(dia9d15ci), N = Inf, level = CF)
#dia10
sample.size.mean(TID15, sd(dia10d15ci), N = Inf, level = CF)

#Pruebas de normalidad 
defecacion15out<-read.csv("Defecacion - Defecacion 15 outliers.csv",T)
attach(defecacion15out)
d15out<- defecacion15out
d15out
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
defecacion15AV<-read.csv("intervalos defecacion 15 - anova.csv",T)
d15av<- defecacion15AV
(framed15 <- stack(d15av))
names(framed15) <- c("defecaciond15","Diad15")
attach(framed15)
head(framed15)
fligner.test(defecaciond15~Diad15)
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
summary(aov(Bombeod15~Diad15))
plot(aov(Bombeod15~Diad15))

#homocedasticidad de datos
defecacion15V<-read.csv("intervalos defecacion 15 - fligner.csv",T)
d15v<- defecacion15V
(framevd15 <- stack(d15v))
names(framevd15) <- c("defecacionvd15","Diavd15")
attach(framevd15)
head(framevd15)
fligner.test(defecacionvd15~Diavd15)
boxplot(d15v[,1:5])
#homocedasticidad
defecacion15V2<-read.csv("intervalos defecacion 15 - fligner 2.csv",T)
d15v2<- defecacion15V2
(framevd152 <- stack(d15v2))
names(framevd152) <- c("defecaciond152","Diavd152")
attach(framevd152)
head(framevd15V2)
fligner.test(defecaciond152~Diavd152)
boxplot(d15v2[,1:5])
#graficas con homocedasticidad 1
#paquetes
library(devtools)
library(githubinstall)
library(ggpmisc)
defecacion15hom<-read.csv("intervalos defecacion 15 - Grafica flig1.csv",T)
attach(defecacion15hom)
d15hom<- defecacion15hom
g15dhom <- ggplot(d15hom, aes(x = diad15flig1, y = defecacion15flig1)) 
DEFECACION15hom<- g15dhom + geom_point() + geom_smooth(method = "lm") + stat_fit_tb() 
DEFECACION15hom 
DEFECACION15homloess<-g15dhom + geom_point()+geom_smooth(method = "loess") 
DEFECACION15homloess
#graficas con homocedasticidad 2
#paquetes
library(devtools)
library(githubinstall)
library(ggpmisc)
defecacion15hom2<-read.csv("intervalos defecacion 15 - Grafica flig2.csv",T)
attach(defecacion15hom2)
d15hom2<- defecacion15hom2
g15dhom2 <- ggplot(d15hom2, aes(x = diad15flig2, y = defecacion15flig2)) 
DEFECACION15hom2<- g15dhom2 + geom_point() + geom_smooth(method = "lm") + stat_fit_tb() 
DEFECACION15hom2
DEFECACION15homloess2<-g15dhom2 + geom_point()+geom_smooth(method = "loess") 
DEFECACION15homloess2