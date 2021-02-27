library(ggplot2)
#Leer csv
ajusteb15<-read.csv("Datos - bombeo15.csv",T)
attach(ajusteb15)
ajusteb20<-read.csv("Datos - bombeo 20.csv", T)
ajusted20 <-read.csv("Datos - defecacion20 - Datos - defecacion20.csv",T)
attach(ajusted20)
ajusted15 <-read.csv("Datos - defecacion15.csv",T)
attach(ajusted15)

#Figura 9
ajusteb20$T4 <- as.factor(ajusteb20$T4)
ggplot(data=ajusteb20, aes(x=T4, y=B2, fill= T4)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  labs(x = "Día", y = "Bombeos en 30 segundos", title = "Bombeo a 20 °C")

ajusted20$T3 <- as.factor(ajusted20$T3)
ggplot(data=ajusted20, aes(x=T3, y=D2, fill= T3)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  labs(x = "Día", y = "Tiempo entre defecaciones", title = "Defecación a 20 °C")

ajusteb15$T1 <- as.factor(ajusteb15$T1)  
ggplot(data=ajusteb15, aes(x=T1, y=B1, fill= T1)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  labs(x = "Día", y = "Bombeos en 30 segundos", title = "Bombeo a 15 °C")

ajusted15$T2 <- as.factor(ajusted15$T2)
ggplot(data=ajusted15, aes(x=T2, y=D1, fill= T2)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  labs(x = "Día", y = "Tiempo entre defecaciones", title = "Defecación a 15 °C")

#Figura 12
A <- .0635
W <- 0.1103
B_0 <- 55.25
bombeo <-function(vector){
  
  return(exp(-A*vector)*(B_0*cos(W*vector)) + B_0)
  
}

ajusteb15$T1 <- as.factor(ajusteb15$T1)  
ggplot(data=ajusteb15, aes(x=T1, y=B1, fill= T1)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  stat_function(fun = bombeo,  colour = "red", lwd = 1) + 
  labs(x = "Día", y = "Bombeos en 30 segundos", title = "Modelo de oscilador ajustado para bombeo a 15 °C")



A <- 0.05028
W <- .077841
B_0 <- 55
bombeo <-function(vector){
  
  return(exp(-A*vector)*(B_0*cos(W*vector)) + B_0)
  
}

ajusteb15$T1 <- as.factor(ajusteb15$T1)  
ggplot(data=ajusteb15, aes(x=T1, y=B1, fill= T1)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  stat_function(fun = bombeo,  colour = "red", lwd = 1) + 
  labs(x = "Día", y = "Bombeos en 30 segundos", title = "Modelo de oscilador estimado para bombeo a 15 °C")


#Figura 13#
A2_F  <-96
A2_0  <-40
beta2 <-.16
defecacion20<-function(vector){
  
  return((A2_F-A2_0)*(1- exp(-beta2*vector))+ A2_0)
  
}

ajusted20$T3 <- as.factor(ajusted20$T3)
ggplot(data=ajusted20, aes(x=T3, y=D2, fill= T3)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  stat_function(fun = defecacion20,  colour = "red", lwd = 1) +
  labs(x = "Día", y = "Tiempo entre defecaciones", title = "Modelo de saturación estimado para defecación a 20 °C")


A2_F  <-119.5715
A2_0  <-32.9239
beta2 <-.1354
defecacion20<-function(vector){
  
  return((A2_F-A2_0)*(1- exp(-beta2*vector))+ A2_0)
  
}

ajusted20$T3 <- as.factor(ajusted20$T3)
ggplot(data=ajusted20, aes(x=T3, y=D2, fill= T3)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  stat_function(fun = defecacion20,  colour = "red", lwd = 1) +
  labs(x = "Día", y = "Tiempo entre defecaciones", title = "Modelo de saturación ajustado para defecación a 20 °C")


#Figura14
ajusted15 <-read.csv("Datos - defecacion15.csv",T)
attach(ajusted15)

A_F  <-104
A_0  <-52
beta <-0.2
defecacion15<-function(vector){
  
  return((A_F-A_0)*(1- exp(-beta*vector)) + A_0)
  
}

ajusted15$T2 <- as.factor(ajusted15$T2)
ggplot(data=ajusted15, aes(x=T2, y=D1, fill= T2)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  stat_function(fun = defecacion15,  colour = "red", lwd = 1) +
  labs(x = "Día", y = "Tiempo entre defecaciones", title = "Modelo de saturación estimado para defecación a 15 °C")

A_F  <-117.5904
A_0  <-50.7384
beta <-0.1799
defecacion15<-function(vector){
  
  return((A_F-A_0)*(1- exp(-beta*vector)) + A_0)
  
}

ajusted15$T2 <- as.factor(ajusted15$T2)
ggplot(data=ajusted15, aes(x=T2, y=D1, fill= T2)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  stat_function(fun = defecacion15,  colour = "red", lwd = 1) +
  labs(x = "Día", y = "Tiempo entre defecaciones", title = "Modelo de saturación ajustado para defecación a 15 °C (10)")

A_F  <-111.4791
A_0  <-48.8409
beta <-0.2211
defecacion15<-function(vector){
  
  return((A_F-A_0)*(1- exp(-beta*vector)) + A_0)
  
}

ajusted15$T2 <- as.factor(ajusted15$T2)
ggplot(data=ajusted15, aes(x=T2, y=D1, fill= T2)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  stat_function(fun = defecacion15,  colour = "red", lwd = 1) +
  labs(x = "Día", y = "Tiempo entre defecaciones", title = "Modelo de saturación ajustado para defecación a 15 °C (12)")

#figura s1
ajusteb25<-read.csv("otros ensayos - Datos bombeo25.csv",T)
attach(ajusteb25)
ajusteb25$T5 <- as.factor(ajusteb25$T5)
ggplot(data=ajusteb25, aes(x=T5, y=B3, fill= T5)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  labs(x = "Día", y = "Bombeos en 30 segundos", title = "Bombeo a 25 °C")

#Figura s2
ajustec15<-read.csv("otros ensayos - Datos coleteo 15.csv",T)
attach(ajustec15)
ajustec15$T7 <- as.factor(ajustec15$T7)
ggplot(data=ajustec15, aes(x=T7, y=C2, fill= T7)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  labs(x = "Día", y = "Coleteos en 1 minuto", title = "Coleteo a 15 °C")

#Figura s3
ajustec20<-read.csv("otros ensayos - Datos coleteo20.csv",T)
attach(ajustec20)
ajustec20$T6 <- as.factor(ajustec20$T6)
ggplot(data=ajustec20, aes(x=T6, y=C1, fill= T6)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  labs(x = "Día", y = "Coleteos en 1 minuto", title = "Coleteo a 20 °C")

#Figura S8
A <- .0635
W <- 0.1103
B_0 <- 55.25
bombeo <-function(vector){
  
  return(exp(-A*vector)*(B_0*cos(W*vector)) + B_0)
  
}

A1 <- 0.0535
W1 <- 0.1161
B_01 <- 54.86
bombeo1 <-function(vector){
  
  return(exp(-A1*vector)*(B_01*cos(W1*vector)) + B_01)
  
}

A2 <- 0.0636
W2<- 0.1105
B_02 <- 55.28
bombeo2 <-function(vector){
  
  return(exp(-A2*vector)*(B_02*cos(W2*vector)) + B_02)
  
}
ajusteb15$T1 <- as.factor(ajusteb15$T1)  
ggplot(data=ajusteb15, aes(x=T1, y=B1, fill= T1)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  stat_function(fun = bombeo,  colour = "red", lwd = 1.3) + 
  stat_function(fun = bombeo1,  colour = "blue", lwd = 1.1) + 
  stat_function(fun = bombeo2,  colour = "darkgreen", lwd = 1) + 
  labs(x = "Día", y = "Bombeos en 30 segundos", title = "Modelo de oscilador ajustado para bombeo a 15 °C")

#Figura s9
A2_F  <-119.5715
A2_0  <-32.9239
beta2 <-.1354
defecacion20<-function(vector){
  
  return((A2_F-A2_0)*(1- exp(-beta2*vector))+ A2_0)
  
}

A2_F1  <-112.1280
A2_01  <-30.3934
beta21 <-0.1576


defecacion201<-function(vector){
  
  return((A2_F1-A2_01)*(1- exp(-beta21*vector))+ A2_01)
  
}

A2_F2  <-114.7604
A2_02  <-28.656
beta22 <-0.1671
defecacion202<-function(vector){
  
  return((A2_F2-A2_02)*(1- exp(-beta22*vector))+ A2_02)
  
}
ajusted20$T3 <- as.factor(ajusted20$T3)
ggplot(data=ajusted20, aes(x=T3, y=D2, fill= T3)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  stat_function(fun = defecacion20,  colour = "red", lwd = 1) +
  stat_function(fun = defecacion201,  colour = "blue", lwd = 1) +
  stat_function(fun = defecacion202,  colour = "darkgreen", lwd = 1) +
  labs(x = "Día", y = "Tiempo entre defecaciones", title = "Modelo de saturación ajustado para defecación a 20 °C")

#Figura S10
A_F  <-117.5904
A_0  <-50.7384
beta <-0.1799
defecacion15<-function(vector){
  
  return((A_F-A_0)*(1- exp(-beta*vector)) + A_0)
  
}

A_F1  <-122.0704
A_01  <-52.0607
beta1 <-0.1488
defecacion151<-function(vector){
  
  return((A_F1-A_01)*(1- exp(-beta1*vector)) + A_01)
  
}

A_F2  <-117.1988
A_02  <-50.2274
beta2 <-0.1847
defecacion152<-function(vector){
  
  return((A_F2-A_02)*(1- exp(-beta2*vector)) + A_02)
  
}

ajusted15$T2 <- as.factor(ajusted15$T2)
ggplot(data=ajusted15, aes(x=T2, y=D1, fill= T2)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  stat_function(fun = defecacion15,  colour = "red", lwd = 1) +
  stat_function(fun = defecacion151,  colour = "blue", lwd = 1) +
  stat_function(fun = defecacion152,  colour = "darkgreen", lwd = 1) +
  labs(x = "Día", y = "Tiempo entre defecaciones", title = "Modelo de saturación ajustado para defecación a 15 °C (10)")

#Figura S11
A_F  <-111.4791
A_0  <-48.8409
beta <-0.2211
defecacion15<-function(vector){
  
  return((A_F-A_0)*(1- exp(-beta*vector)) + A_0)
  
}

A_F1  <-110.7644
A_01  <-49.0269
beta1 <-0.2127
defecacion151<-function(vector){
  
  return((A_F1-A_01)*(1- exp(-beta1*vector)) + A_01)
  
}

A_F2  <-111.0506
A_02  <-47.9909
beta2 <-0.2292
defecacion152<-function(vector){
  
  return((A_F2-A_02)*(1- exp(-beta2*vector)) + A_02)
  
}

ajusted15$T2 <- as.factor(ajusted15$T2)
ggplot(data=ajusted15, aes(x=T2, y=D1, fill= T2)) +
  geom_boxplot(notch=TRUE) + geom_jitter(shape=16, position=position_jitter(0.1)) +
  stat_function(fun = defecacion15,  colour = "red", lwd = 1) +
  stat_function(fun = defecacion151,  colour = "blue", lwd = 1) +
  stat_function(fun = defecacion152,  colour = "darkgreen", lwd = 1) +
  labs(x = "Día", y = "Tiempo entre defecaciones", title = "Modelo de saturación ajustado para defecación a 15 °C (12)")
#Figura S12
A <- .0635
W <- 0.1103
B_0 <- 55.25
bombeo <-function(vector){
  
  return(exp(-A*vector)*(B_0*cos(W*vector)) + B_0)
  
}

A2 <- .05028
W2 <- .077841
B_02 <- 55
bombeo2 <-function(vector){
  
  return(exp(-A2*vector)*(B_02*cos(W2*vector)) + B_02)
  
}

ajusteb15$T1 <- as.factor(ajusteb15$T1)  
ggplot(data=ajusteb15) +
  stat_function(fun = bombeo,  colour = "blue", lwd = 1) + xlim(0,40) + ylim(10,110) + 
  stat_function(fun = bombeo2,  colour = "red", lwd = 1) +
  labs(x = "Día", y = "Bombeos en 30 segundos", title = "Modelos de oscilador para bombeo a 15 °C")

#Figura S13
A <- 0.0494
B_0 <- 113.6
bombeo <-function(vector){
  
  return(exp(-A*vector)*( B_0))
  
}

A2 <- .05028
B_02 <- 105
bombeo2 <-function(vector){
  
  return(exp(-A2*vector)*( B_02))
  
}
ajusteb15$T1 <- as.factor(ajusteb15$T1)  
ggplot(data=ajusteb15) +
  stat_function(fun = bombeo,  colour = "blue", lwd = 1) + xlim(0,40) + 
  stat_function(fun = bombeo2,  colour = "red", lwd = 1) +
  labs(x = "Día", y = "Bombeos en 30 segundos", title = "Modelos exponenciales para bombeo a 15 °C")

#Figura S14
A2_F  <-96
A2_0  <-40
beta2 <-.16
defecacion20<-function(vector){
  
  return((A2_F-A2_0)*(1- exp(-beta2*vector))+ A2_0)
  
}

A2_F2  <-119.5715
A2_02  <-32.9239
beta22 <-.1354
defecacion202<-function(vector){
  
  return((A2_F2-A2_02)*(1- exp(-beta22*vector))+ A2_02)
  
}
ajusted20$T3 <- as.factor(ajusted20$T3)
ggplot(data=ajusted20) +
  stat_function(fun = defecacion20,  colour = "red", lwd = 1) + xlim(0,35) +
  stat_function(fun = defecacion202,  colour = "blue", lwd = 1) +
  labs(x = "Día", y = "Tiempo entre defecaciones", title = "Modelos de saturación para defecación a 20 °C")

#Figura S15
A_F  <-104
A_0  <-52
beta <-0.2
defecacion15<-function(vector){
  
  return((A_F-A_0)*(1- exp(-beta*vector)) + A_0)
  
}

A_F1  <-117.5904
A_01  <-50.7384
beta1 <-0.1799
defecacion151<-function(vector){
  
  return((A_F1-A_01)*(1- exp(-beta1*vector)) + A_01)
  
}

A_F2  <-111.4791
A_02  <-48.8409
beta2 <-0.2211
defecacion152<-function(vector){
  
  return((A_F2-A_02)*(1- exp(-beta2*vector)) + A_02)
  
}

ajusted15$T2 <- as.factor(ajusted15$T2)
ggplot(data=ajusted15) +
  stat_function(fun = defecacion15,  colour = "red", lwd = 1) + xlim(0,40) + 
  stat_function(fun = defecacion151,  colour = "blue", lwd = 1) + 
  stat_function(fun = defecacion152,  colour = "darkgreen", lwd = 1) + 
  labs(x = "Día", y = "Tiempo entre defecaciones", title = "Modelos de saturación para defecación a 15 °C")
