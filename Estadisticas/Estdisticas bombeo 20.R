#pruebas estadisticas Bombeo 20
bombeo20est<-read.csv("20 Grados - bombeo 20 outliers.csv",T)
attach(bombeo20est)
b20est<- bombeo20est
b20est

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
intervalosd1b20<-read.csv("intervalos bombeo 20 - Dia 1.csv",T)
attach(intervalosd1b20)
id1b20<- intervalosd1b20
id1b20
mean(dia1b20ci)
bootd1b20 <- boot(dia1b20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd1b20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia2
intervalosd2b20<-read.csv("intervalos bombeo 20 - Dia 2.csv",T)
attach(intervalosd2b20)
id2b20<- intervalosd2b20
id2b20
mean(dia2b20ci)
bootd2b20 <- boot(dia2b20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd2b20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia3
intervalosd3b20<-read.csv("intervalos bombeo 20 - Dia 3.csv",T)
attach(intervalosd3b20)
id3b20<- intervalosd3b20
id3b20
mean(dia3b20ci)
bootd3b20 <- boot(dia3b20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd3b20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia4
intervalosd4b20<-read.csv("intervalos bombeo 20 - Dia 4.csv",T)
attach(intervalosd4b20)
id4b20<- intervalosd4b20
id4b20
mean(dia4b20ci)
bootd4b20 <- boot(dia4b20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd4b20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia5
intervalosd5b20<-read.csv("intervalos bombeo 20 - Dia 5.csv",T)
attach(intervalosd5b20)
id5b20<- intervalosd5b20
id5b20
mean(dia5b20ci)
bootd5b20 <- boot(dia5b20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd5b20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia6
intervalosd6b20<-read.csv("intervalos bombeo 20 - Dia 6.csv",T)
attach(intervalosd6b20)
id6b20<- intervalosd6b20
id6b20
mean(dia6b20ci)
bootd6b20 <- boot(dia6b20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd6b20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia7
intervalosd7b20<-read.csv("intervalos bombeo 20 - Dia 7.csv",T)
attach(intervalosd7b20)
id7b20<- intervalosd7b20
id7b20
mean(dia7b20ci)
bootd7b20 <- boot(dia7b20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd7b20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia8
intervalosd8b20<-read.csv("intervalos bombeo 20 - Dia 8.csv",T)
attach(intervalosd8b20)
id8b20 <- intervalosd8b20
id8b20
mean(dia8b20ci)
bootd8b20 <- boot(dia8b20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd8b20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia9
intervalosd9b20<-read.csv("intervalos bombeo 20 - Dia 9.csv",T)
attach(intervalosd9b20)
id9b20 <- intervalosd9b20
id9b20
mean(dia9b20ci)
bootd9b20 <- boot(dia9b20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd9b20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia10
intervalosd10b20<-read.csv("intervalos bombeo 20 - Dia 10.csv",T)
attach(intervalosd10b20)
id10b20 <- intervalosd10b20
id10b20
mean(dia10b20ci)
bootd10b20 <- boot(dia10b20ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd10b20, conf = CF, type = c("norm", "basic" ,"perc", "bca"))

#tamaño de la muestra por media(paquete: samplingbook)
library(samplingbook)
CF <- .95
#Mitad de intervalo por dia 1-3
TIB20 <- 2
#dia1 
sample.size.mean(TIB20, sd(dia1b20ci), N = Inf, level = CF)
#dia2
sample.size.mean(TIB20, sd(dia2b20ci), N = Inf, level = CF)
#dia3
sample.size.mean(TIB20, sd(dia3b20ci), N = Inf, level = CF)
#dia4
sample.size.mean(TIB20, sd(dia4b20ci), N = Inf, level = CF)
#dia5
sample.size.mean(TIB20, sd(dia5b20ci), N = Inf, level = CF)
#dia6
sample.size.mean(TIB20, sd(dia6b20ci), N = Inf, level = CF)
#dia7
sample.size.mean(TIB20, sd(dia7b20ci), N = Inf, level = CF)
#dia8
sample.size.mean(TIB20, sd(dia8b20ci), N = Inf, level = CF)
#dia9
sample.size.mean(TIB20, sd(dia9b20ci), N = Inf, level = CF)
#dia10
sample.size.mean(TIB20, sd(dia10b20ci), N = Inf, level = CF)

#Pruebas de normalidad 
bombeo20out<-read.csv("20 Grados - bombeo 20 outliers.csv",T)
attach(bombeo20out)
b20out<- bombeo20out
b20out
gusanob2020out=as.factor(gusanob2020out)
gusanob2020out
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
bombeo20AV<-read.csv("intervalos bombeo 20 - Fligner.csv",T)
b20av<- bombeo20AV
(frameb20 <- stack(b20av))
names(frameb20) <- c("Bombeob20","Diab20")
attach(frameb20)
head(frameb20)
fligner.test(Bombeob20~Diab20)
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
summary(aov(Bombeob20~Diab20))
plot(aov(Bombeob20~Diab20))

#homocedasticidad de datos
bombeo20V<-read.csv("intervalos bombeo 20 - Fligner.csv",T)
b20v<- bombeo20V
(framevb20 <- stack(b20v))
names(framevb20) <- c("Bombeovb20","Diavb20")
attach(framevb20)
head(framevb20)
fligner.test(Bombeovb20~Diavb20)
boxplot(b20v[,1:7])
#graficas con homocedasticidad
#paquetes
library(devtools)
library(githubinstall)
library(ggpmisc)
bombeo20hom<-read.csv("intervalos bombeo 20 - Grafica flig.csv",T)
attach(bombeo20hom)
b20hom<- bombeo20hom
g20hom <- ggplot(b20hom, aes(x = diab20flig, y = bombeo20flig)) 
BOMBEO20hom<- g20hom + geom_point() + geom_smooth(method = "lm") + stat_fit_tb() 
BOMBEO20hom 
Bombeo20homloess<-g20hom + geom_point()+geom_smooth(method = "loess") 
Bombeo20homloess