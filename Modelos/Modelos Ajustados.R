###Ajuste con datos completos###
##Bombeo a 15 grados##
#Exponencial#
ajusteb15<-read.csv("Datos - bombeo15.csv",T)
attach(ajusteb15)
Ae.0 <- 0.05028
B_0e.0 <- 105
datosb15e<- data.frame(y = B1, x = T1)
startb15e <- list(Ae = Ae.0, B_0e = B_0e.0)
startb15e
modelb15e <- nls(y ~ exp(-Ae*x)*(B_0e) , data = datosb15e, start = startb15e
             , control=nls.control(maxiter = 400))
summary(modelb15e)
plot(T1, B1, log = "y")
lines(datosb15e$x, predict(modelb15e, list(x = datosb15e$x)), col = 'red', lwd = 3)
#Oscilador armonico#

A.0 <- 0.05028
W.0 <- 0.219
B_0.0 <- 55
datosb15<- data.frame(y = B1, x = T1)
startb15 <- list(A = A.0, W = W.0, B_0 = B_0.0)
startb15
modelb15 <- nls(y ~ exp(-A*x)*(B_0*cos(W*x)) + B_0, data = datosb15, start = startb15
                , control=nls.control(maxiter = 400))
summary(modelb15)
plot(T1, B1, log = "y")
lines(datosb15$x, predict(modelb15, list(x = datosb15$x)), col = 'red', lwd = 3)
################################################################################################################################

##Defecacion a 20 grados##
ajusted20 <-read.csv("Datos - defecacion20.csv",T)
attach(ajusted20)
ajusted20
A2_F.0  <-96
A2_0.0  <-40
beta2.0 <-.16
datosd20<- data.frame(y = D2, x = T3)
startd20 <- list(A2_F = A2_F.0, A2_0 = A2_0.0, beta2 = beta2.0)
startd20
modeld20 <- nls( y ~((A2_F-A2_0)*(1- exp(-beta2*x))+ A2_0) , data = datosd20, start = startd20
                , control=nls.control(maxiter = 500))
summary(modeld20)
plot(T3, D2, log = "y")
lines(datosd20$x, predict(modeld20, list(x = datosd20$x)), col = 'red', lwd = 3)
################################################################################################################################

##Defecaccion a 15 grados##
#10 dias#
ajusted15 <-read.csv("Datos - defecacion15.csv",T)
attach(ajusted15)
ajusted15
A_F.0  <-104
A_0.0  <-52
beta.0 <-.2
datosd15<- data.frame(y = D1, x = T2)
startd15 <- list(A_F = A_F.0, A_0 = A_0.0, beta = beta.0)
startd15
modeld15 <- nls( y ~((A_F-A_0)*(1- exp(-beta*x))+ A_0) , data = datosd15, start = startd15
                 , control=nls.control(maxiter = 500))
summary(modeld15)
plot(T2, D1, log = "y")
lines(datosd15$x, predict(modeld15, list(x = datosd15$x)), col = 'red', lwd = 3)
#12 dias#
ajusted152 <-read.csv("Datos - defecacion152.csv",T)
attach(ajusted152)
ajusted152
A_F1.0  <-104
A_01.0  <-52
beta1.0 <-.2
datosd152<- data.frame(y = D12, x = T22)
startd152 <- list(A_F1 = A_F1.0, A_01 = A_01.0, beta1 = beta1.0)
startd152
modeld152 <- nls( y ~((A_F1-A_01)*(1- exp(-beta1*x))+ A_01) , data = datosd152, start = startd152
                 , control=nls.control(maxiter = 500))
summary(modeld152)
plot(T22, D12, log = "y")
lines(datosd152$x, predict(modeld152, list(x = datosd152$x)), col = 'red', lwd = 3)

############################################################################################################################
############################################################################################################################

###Ajuste con medias y medianas###
##Bombeo 15##
#exponencial#
ajusteb15<-read.csv("Datos - bombeo15.csv",T)
attach(ajusteb15)
Ame.0 <- 0.05028
B_0me.0 <- 105
X_b15 <- 1:10
#Cambiar de media a mediana#
Y_b15<-c()
for( i in 1:10){
  Y_b15_p<-ajusteb15[ajusteb15$T1==i,]
  Y_b15[i]<-apply(Y_b15_p,2,mean)[2]
}
Y_b15
datosmb15<- data.frame(y = Y_b15, x = X_b15)
startmb15e <- list(Ame = Ame.0, B_0me = B_0me.0)
startmb15e
modelmb15e <- nls(y ~ exp(-Ame*x)*(B_0me) , data = datosmb15, start = startmb15e
                , control=nls.control(maxiter = 400))
summary(modelmb15e)
plot(T1, B1, log = "y")
#Mediana#
lines(datosmb15$x, predict(modelmb15e, list(x = datosmb15$x)), col = 'blue', lwd = 3)
#Media#
lines(datosmb15$x, predict(modelmb15e, list(x = datosmb15$x)), col = 'darkgreen', lwd = 3)

#oscilador#
Am.0 <- 0.05028
Wm.0 <- 0.219
B_0m.0 <- 55
X_b15 <- 1:10
#Cambiar de media a mediana#
Y_b15<-c()
for( i in 1:10){
  Y_b15_p<-ajusteb15[ajusteb15$T1==i,]
  Y_b15[i]<-apply(Y_b15_p,2,mean)[2]
}
Y_b15
datosmb15<- data.frame(y = Y_b15, x = X_b15)
startmb15 <- list(Am = Am.0, Wm = Wm.0, B_0m = B_0m.0)
startmb15
modelmb15 <- nls(y ~ exp(-Am*x)*(B_0m*cos(Wm*x)) + B_0m, data = datosmb15, start = startmb15
                 , control=nls.control(maxiter = 400))
summary(modelmb15)
plot(T1, B1, log = "y")
#Mediana#
lines(datosmb15$x, predict(modelmb15, list(x = datosmb15$x)), col = 'blue', lwd = 3)
#Media#
lines(datosmb15$x, predict(modelmb15, list(x = datosmb15$x)), col = 'darkgreen', lwd = 3)
################################################################################################################################

##Defecacion 20##
ajusted20 <-read.csv("Datos - defecacion20.csv",T)
attach(ajusted20)
ajusted20
A2_Fm.0  <-96
A2_0m.0  <-40
beta2m.0 <-.16
X_d20 <- 1:13
#Cambiar de media a mediana#
Y_d20<-c()
for( i in 1:13){
  Y_d20_p<-ajusted20[ajusted20$T3==i,]
  Y_d20[i]<-apply(Y_d20_p,2,mean)[2]
}
Y_d20
datosmd20 <- data.frame(y = Y_d20, x = X_d20)
startmd20 <- list(A2_Fm = A2_Fm.0, A2_0m = A2_0m.0, beta2m = beta2m.0)
startmd20
modelmd20 <- nls( y ~((A2_Fm-A2_0m)*(1- exp(-beta2m*x))+ A2_0m) , data = datosmd20, start = startmd20, )
summary(modelmd20)
plot(T3, D2, log = "y")
#Mediana#
lines(datosmd20$x, predict(modelmd20, list(x = datosmd20$x)), col = 'blue', lwd = 3)
#Media#
lines(datosmd20$x, predict(modelmd20, list(x = datosmd20$x)), col = 'darkgreen', lwd = 3) 
################################################################################################################################

##Defecaccion a 15 grados##
#10 dias#
ajusted15 <-read.csv("Datos - defecacion15.csv",T)
attach(ajusted15)
ajusted15
A_Fm.0  <-104
A_0m.0  <-52
betam.0 <-.2
X_d15 <- 1:10
#Cambiar de media a mediana#
Y_d15<-c()
for( i in 1:10){
  Y_d15_p<-ajusted15[ajusted15$T2==i,]
  Y_d15[i]<-apply(Y_d15_p,2,mean)[2]
}
Y_d15
datosmd15 <- data.frame(y = Y_d15, x = X_d15)
startmd15 <- list(A_Fm = A_Fm.0, A_0m = A_0m.0, betam = betam.0)
startmd15
modelmd15 <- nls( y ~((A_Fm-A_0m)*(1- exp(-betam*x))+ A_0m) , data = datosmd15, start = startmd15
                 , control=nls.control(maxiter = 500))
summary(modelmd15)
plot(T2, D1, log = "y")
#Mediana#
lines(datosmd15$x, predict(modelmd15, list(x = datosmd15$x)), col = 'blue', lwd = 3)
#Media#
lines(datosmd15$x, predict(modelmd15, list(x = datosmd15$x)), col = 'darkgreen', lwd = 3)

#12 dias#
ajusted152 <-read.csv("Datos - defecacion152.csv",T)
attach(ajusted152)
ajusted152
A_Fm1.0  <-104
A_0m1.0  <-52
betam1.0 <-.2
X_d152 <- 1:12
#Cambiar de media a mediana#
Y_d152<-c()
for( i in 1:12){
  Y_d152_p<-ajusted152[ajusted152$T22==i,]
  Y_d152[i]<-apply(Y_d152_p,2,median)[2]
}
Y_d152
datosmd152 <- data.frame(y = Y_d152, x = X_d152)
startmd152 <- list(A_Fm1 = A_Fm1.0, A_0m1 = A_0m1.0, betam1 = betam1.0)
startmd152
modelmd152 <- nls( y ~((A_Fm1-A_0m1)*(1- exp(-betam1*x))+ A_0m1) , data = datosmd152, start = startmd152
                  , control=nls.control(maxiter = 400))
summary(modelmd152)
plot(T22, D12, log = "y")
#Mediana#
lines(datosmd152$x, predict(modelmd152, list(x = datosmd152$x)), col = 'blue', lwd = 3)
#Media#
lines(datosmd152$x, predict(modelmd152, list(x = datosmd152$x)), col = 'darkgreen', lwd = 3)

############################################################################################################################
############################################################################################################################
############################################################################################################################

###Graficas####
##bombeo 15##
#exponente#
bombeo15out<-read.csv("15 grados - bombeo 15 outliers.csv",T)
attach(bombeo15out)
b15out<- bombeo15out
b15out
boxplot(b15out[,2:11], main = "Modelo ajustado de exponencial para bombeo a 15 grados", ylab = "Bombeos en 30 Segundos")
lines(datosb15e$x, predict(modelb15e, list(x = datosb15e$x)), col = 'red', lwd = 3)
#oscilador#
bombeo15out<-read.csv("15 grados - bombeo 15 outliers.csv",T)
attach(bombeo15out)
b15out<- bombeo15out
b15out
boxplot(b15out[,2:11], main = "Modelo ajustado de oscilador amortiguado para bombeo a 15 grados", ylab = "Bombeos en 30 Segundos")
lines(datosb15$x, predict(modelb15, list(x = datosb15$x)), col = 'red', lwd = 3)
#Mediana#
lines(datosmb15$x, predict(modelmb15, list(x = datosmb15$x)), col = 'blue', lwd = 3)
#Media#
lines(datosmb15$x, predict(modelmb15, list(x = datosmb15$x)), col = 'darkgreen', lwd = 3)
#Intervalos de confianza#
datosb15$pred <- predict(modelb15) 
se = summary(modelb15)$sigma
ci = outer(datosb15$pred, c(outer(se, c(-1,1), '*'))*1.96, '+')
ii = order(datosb15$x) 
# Grafica con lineas#
with(datosb15[ii,], plot(x, pred, ylim=range(ci), type='l'))
matlines(datosb15[ii,'x'], ci[ii,], lty=2, col=1)
with(datosb15[ii,], lines(x, pred, col='red'))
with(datosb15, points(x, y))
title("Intervalos de confianza para el modelo de bombeo a 15 grados")
# Grafica con area#
low = ci[ii,1]; high = ci[ii,2]; base = datosb15[ii,'x']
polygon(c(base,rev(base)), c(low,rev(high)), col='gray')
with(datosb15[ii,], lines(x, pred, col='red'))
with(datosb15, points(x, y))
title("Intervalos de confianza para el modelo de bombeo a 15 grados")
######################################################################################################################
######################################################################################################################

##defecacion 20##
defecacion20out<-read.csv("Defecacion - Defecacion 20 outliers.csv",T)
attach(defecacion20out)
d20out<- defecacion20out
d20out
boxplot(d20out[,1:10], main = "Modelo ajustado de saturacion para Defecación a 20 grados", ylab = "Tiempo en segundos")
lines(datosd20$x, predict(modeld20, list(x = datosd20$x)), col = 'red', lwd = 3)
#Mediana#
lines(datosmd20$x, predict(modelmd20, list(x = datosmd20$x)), col = 'blue', lwd = 3)
#Media#
lines(datosmd20$x, predict(modelmd20, list(x = datosmd20$x)), col = 'darkgreen', lwd = 3) 

#Intervalos de confianza#
datosd20$pred <- predict(modeld20) 
se = summary(modeld20)$sigma
ci = outer(datosd20$pred, c(outer(se, c(-1,1), '*'))*1.96, '+')
ii = order(datosd20$x) 
# Grafica con lineas#
with(datosd20[ii,], plot(x, pred, ylim=range(ci), type='l', xlim = c(1,10)))
matlines(datosd20[ii,'x'], ci[ii,], lty=2, col=1)
with(datosd20[ii,], lines(x, pred, col='red'))
with(datosd20, points(x, y))
title("Intervalos de confianza para el modelo de defecacion a 20 grados")
# Grafica con area#
low = ci[ii,1]; high = ci[ii,2]; base = datosd20[ii,'x']
polygon(c(base,rev(base)), c(low,rev(high)), col='gray')
with(datosd20[ii,], lines(x, pred, col='red'))
with(datosd20, points(x, y))
title("Intervalos de confianza para el modelo de defecacion a 15 grados")

#######################################################################################################################
#######################################################################################################################

##defecacion 15## 
#10 dias#
defecacion15out<-read.csv("Defecacion - Defecacion 15 outliers.csv",T)
attach(defecacion15out)
d15out<- defecacion15out
d15out
boxplot(d15out[,1:10], main = "Modelo ajustado de saturacion para Defecación a 15 grados (10)", ylab = "Tiempo en segundos")
lines(datosd15$x, predict(modeld15, list(x = datosd15$x)), col = 'red', lwd = 3)
#Mediana#
lines(datosmd15$x, predict(modelmd15, list(x = datosmd15$x)), col = 'blue', lwd = 3)
#Media#
lines(datosmd15$x, predict(modelmd15, list(x = datosmd15$x)), col = 'darkgreen', lwd = 3)

#Intervalos de confianza#
datosd15$pred <- predict(modeld15) 
se = summary(modeld15)$sigma
ci = outer(datosd15$pred, c(outer(se, c(-1,1), '*'))*1.96, '+')
ii = order(datosd15$x) 
# Grafica con lineas#
with(datosd15[ii,], plot(x, pred, ylim=range(ci), type='l', xlim = c(1,10)))
matlines(datosd15[ii,'x'], ci[ii,], lty=2, col=1)
with(datosd15[ii,], lines(x, pred, col='red'))
with(datosd15, points(x, y))
title("Intervalos de confianza para el modelo de defecacion a 15 grados (10)")
# Grafica con area#
low = ci[ii,1]; high = ci[ii,2]; base = datosd15[ii,'x']
polygon(c(base,rev(base)), c(low,rev(high)), col='gray')
with(datosd15[ii,], lines(x, pred, col='red'))
with(datosd15, points(x, y))
title("Intervalos de confianza para el modelo de defecacion a 15 grados (10)")


#12 dias#
defecacion15out<-read.csv("Defecacion - Defecacion 15 outliers.csv",T)
attach(defecacion15out)
d15out<- defecacion15out
d15out
boxplot(d15out[,1:10], main = "Modelo ajustado de saturacion para Defecación a 15 grados (12)", ylab = "Tiempo en segundos")
lines(datosd152$x, predict(modeld152, list(x = datosd152$x)), col = 'red', lwd = 3)
#Mediana#
lines(datosmd152$x, predict(modelmd152, list(x = datosmd152$x)), col = 'blue', lwd = 3)
#Media#
lines(datosmd152$x, predict(modelmd152, list(x = datosmd152$x)), col = 'darkgreen', lwd = 3)

#Intervalos de confianza#
datosd152$pred <- predict(modeld152) 
se = summary(modeld152)$sigma
ci = outer(datosd152$pred, c(outer(se, c(-1,1), '*'))*1.96, '+')
ii = order(datosd152$x) 
# Grafica con lineas#
with(datosd152[ii,], plot(x, pred, ylim=range(ci), type='l', xlim = c(1,10)))
matlines(datosd152[ii,'x'], ci[ii,], lty=2, col=1)
with(datosd152[ii,], lines(x, pred, col='red'))
with(datosd152, points(x, y))
title("Intervalos de confianza para el modelo de defecacion a 15 grados (12)")
# Grafica con area#
low = ci[ii,1]; high = ci[ii,2]; base = datosd152[ii,'x']
polygon(c(base,rev(base)), c(low,rev(high)), col='gray')
with(datosd152[ii,], lines(x, pred, col='red'))
with(datosd152, points(x, y))
title("Intervalos de confianza para el modelo de defecacion a 15 grados (12)")