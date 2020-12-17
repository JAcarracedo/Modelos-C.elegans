#pruebas estadisticas Bombeo 15
bombeo15est<-read.csv("15 grados - bombeo 15 outliers est.csv",T)
attach(bombeo15est)
b15est<- bombeo15est
b15est

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
#dia1 
library(boot)
#nivel de confianza
CF<- .95
intervalosd1<-read.csv("intervalos bombeo 15 - Dia1.csv",T)
attach(intervalosd1)
id1<- intervalosd1
id1
mean(dia1b15ci)
bootd1b15 <- boot(dia1b15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd1b15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia2
intervalosd2<-read.csv("intervalos bombeo 15 - Dia2.csv",T)
attach(intervalosd2)
id2<- intervalosd2
mean(dia2b15ci)
bootd2b15 <- boot(dia2b15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd2b15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia3
intervalosd3<-read.csv("intervalos bombeo 15 - Dia3.csv",T)
attach(intervalosd3)
id3<- intervalosd3
id3
mean(dia3b15ci)
bootd3b15 <- boot(dia3b15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd3b15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia4
intervalosd4<-read.csv("intervalos bombeo 15 - Dia4.csv",T)
attach(intervalosd4)
id4<- intervalosd4
id4
mean(dia4b15ci)
bootd4b15 <- boot(dia4b15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd4b15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia5
intervalosd5<-read.csv("intervalos bombeo 15 - Dia5.csv",T)
attach(intervalosd5)
id5<- intervalosd5
id5
mean(dia5b15ci)
bootd5b15 <- boot(dia5b15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd5b15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia6
intervalosd6<-read.csv("intervalos bombeo 15 - Dia6.csv",T)
attach(intervalosd6)
id6<- intervalosd6
id6
mean(dia6b15ci)
bootd6b15 <- boot(dia6b15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd6b15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia7
intervalosd7<-read.csv("intervalos bombeo 15 - Dia7.csv",T)
attach(intervalosd7)
id7<- intervalosd7
id7
mean(dia7b15ci)
bootd7b15 <- boot(dia7b15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd7b15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia8
intervalosd8<-read.csv("intervalos bombeo 15 - Dia8.csv",T)
attach(intervalosd8)
id8 <- intervalosd8
id8
mean(dia8b15ci)
bootd8b15 <- boot(dia8b15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd8b15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia9
intervalosd9<-read.csv("intervalos bombeo 15 - Dia9.csv",T)
attach(intervalosd9)
id9 <- intervalosd9
id9
mean(dia9b15ci)
bootd9b15 <- boot(dia9b15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd9b15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))
#dia10
intervalosd10<-read.csv("intervalos bombeo 15 - Dia10.csv",T)
attach(intervalosd10)
id10 <- intervalosd10
id10
mean(dia10b15ci)
bootd10b15 <- boot(dia10b15ci, function(x,i) mean(x[i]), R=10000)
boot.ci(bootd10b15, conf = CF, type = c("norm", "basic" ,"perc", "bca"))

#tamaño de la muestra por media(paquete: samplingbook)
library(samplingbook)
#Mitad de intervalo por dia 1-3
TIB15 <- 2
CF<- .95
#dia1 
sample.size.mean(TIB15, sd(dia1b15ci), N = Inf, level = CF)
#dia2
sample.size.mean(TIB15, sd(dia2b15ci), N = Inf, level = CF)
#dia3
sample.size.mean(TIB15, sd(dia3b15ci), N = Inf, level = CF)
#dia4
sample.size.mean(TIB15, sd(dia4b15ci), N = Inf, level = CF)
#dia5
sample.size.mean(TIB15, sd(dia5b15ci), N = Inf, level = CF)
#dia6
sample.size.mean(TIB15, sd(dia6b15ci), N = Inf, level = CF)
#dia7
sample.size.mean(TIB15, sd(dia7b15ci), N = Inf, level = CF)
#dia8
sample.size.mean(TIB15, sd(dia8b15ci), N = Inf, level = CF)
#dia9
sample.size.mean(TIB15, sd(dia9b15ci), N = Inf, level = CF)
#dia10
sample.size.mean(TIB15, sd(dia10b15ci), N = Inf, level = CF)

#Pruebas de distribucion
bombeo15out<-read.csv("15 grados - bombeo 15 outliers.csv",T)
attach(bombeo15out)
b15out<- bombeo15out
b15out
gusanob15out=as.factor(gusanob15out)
gusanob15out
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
bombeo15AV<-read.csv("15 grados - bombeo 15 outliers anova.csv",T)
b15av<- bombeo15AV
(frame <- stack(b15av))
names(frame) <- c("Bombeo","Dia")
attach(frame)
head(frame)
fligner.test(Bombeo~Dia)
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
summary(aov(Bombeo~Dia))
plot(aov(Bombeo~Dia))

#Obtencion de homocedasticidad
bombeo15v<-read.csv("intervalos bombeo 15 - fligner.csv",T)
b15v<- bombeo15v
(frame2 <- stack(b15v))
names(frame2) <- c("Bombeo15v","Dia15v")
attach(frame2)
head(frame2)
fligner.test(Bombeo15v~Dia15v)
# homocedasticidad con mayor cantidad de datos 
bombeo15v2<-read.csv("intervalos bombeo 15 - fligner2.csv",T)
b15v2<- bombeo15v2
(frame2v <- stack(b15v2))
names(frame2v) <- c("Bombeo15v2","Dia15v2")
attach(frame2v)
head(frame2v)
fligner.test(Bombeo15v2~Dia15v2)
boxplot(b15v2[,1:7])
#graficas con homocedasticidad
#paquetes
library(devtools)
library(githubinstall)
library(ggpmisc)
bombeo15hom<-read.csv("intervalos bombeo 15 - Grafica flig.csv",T)
attach(bombeo15hom)
b15hom<- bombeo15hom
g15hom <- ggplot(b15hom, aes(x = diab15flig, y = bombeo15flig)) 
BOMBEO15hom<- g15hom + geom_point() + geom_smooth(method = "lm") + stat_fit_tb() 
BOMBEO15hom 
Bombeo15homloess<-g15hom + geom_point()+geom_smooth(method = "loess") 
Bombeo15homloess