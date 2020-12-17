###Bombeo a 15 grados##
##Exponencial##
#Estimado#
A <- .05028
B_0 <- 105
bombeo <-function(vector){
  
  return(exp(-A*vector)*( B_0))
  
}

diase <-seq(1,40,0.001)
bome <-bombeo(dias)
plot(diase,bome, type = "l", col = "red", lwd = 3, ylab = "Bombeos en 30 segundos", 
     main = "Exponencial de bombeo a 15 grados" )
#Ajustado#
A <- .0494
B_0 <- 113.6
bombeo <-function(vector){
  
  return(exp(-A*vector)*( B_0))
  
}

dias <-seq(1,40,0.001)
bom <-bombeo(dias)
lines(dias, bom, col = "blue")
plot(dias,bom, type = "l", col = "red", lwd = 3, ylab = "Bombeos en 30 segundos", 
     main = "Exponencial ajustada, de bombeo a 15 grados" )
#Grafica#
plot(diase,bome, type = "l", col = "red", lwd = 2, ylab = "Bombeos en 30 segundos", 
     main = "Modelos exponenciales de bombeo a 15 grados", ylim = c(10, 110))
lines(dias, bom, col = "blue",lwd = 2 )
##Oscilador##
#Estimado#
A <- .05028
W <- 0.5282
B_0 <- 55
bombeo <-function(vector){
  
  return(exp(-A*vector)*(B_0*cos(W*vector)) + B_0)
  
}

dias <-seq(1,40,0.001)
bom <-bombeo(dias)
plot(dias,bom, type = "l", col = "red", lwd = 3, ylab = "Bombeos en 30 segundos", 
     main = "Modelo estimado de bombeo a 15 grados") 
Est <- A^2 - (4*W)
Est
#Ajustado#
A2 <- .063526
W2 <- 0.11
B_02 <- 55.25
bombeoob15 <-function(vector){
  
  return(exp(-A2*vector)*(B_02*cos(W2*vector)) + B_02)
  
}

diasob15 <-seq(1,40,0.001)
bomob15 <-bombeoob15(diasob15)
plot(diasob15,bomob15, type = "l", col = "red", lwd = 3, ylab = "Bombeos en 30 segundos", 
     main = "Modelo ajustado de bombeo a 15 grados" )
Est <- A2^2 - (4*W2)
Est

#Grafica#
plot(dias,bom, type = "l", col = "red", lwd = 2, ylab = "Bombeos en 30 segundos", 
     main = "Modelos de oscilador para bombeo a 15 grados", ylim = c(10,110)) 
lines(diasob15, bomob15, col = "blue", lwd = 2)

################################################################################################################################
###Defecacion a 20 grados##
##Estimado##
A2_F  <-96
A2_0  <-40
beta2 <-.16
defecacion20<-function(vector){
  
  return((A2_F-A2_0)*(1- exp(-beta2*vector))+ A2_0)
  
}

dias<-seq(1,35,0.001)
def20<-defecacion20(dias)
plot(dias,def20, type = "l", col = "red", lwd = 3, ylab = "Tiempo en segundos", 
     main = "Modelo estimado de defecacion a 20 grados" )

##Ajustado##
A2_F  <-119.57
A2_0  <-32.92
beta2 <-.1354
defecacion201<-function(vector){
  
  return((A2_F-A2_0)*(1- exp(-beta2*vector))+ A2_0)
  
}

dias1<-seq(1,35,0.001)
def201<-defecacion201(dias1)
plot(dias1,def201, type = "l", col = "red", lwd = 3, ylab = "Tiempo en segundos", 
     main = "Modelo ajustado de defecacion a 20 grados" )
#Graficas#
plot(dias,def20, type = "l", col = "red", lwd = 2, ylab = "Tiempo en segundos", 
     main = "Modelos de saturacion para defecacion a 20 grados", ylim = c(45,120) )
lines(dias1, def201, col = "blue", lwd = 2)
################################################################################################################################
###Defecacion a 15 grados##
##Estimado##
A_F  <-104
A_0  <-52
beta <-0.2
defecacion15<-function(vector){
  
  return((A_F-A_0)*(1- exp(-beta*vector)) + A_0)
  
}

dias<-seq(1,40,0.001)
def<-defecacion15(dias)
plot(dias,def, type = "l", col = "red", lwd = 3, ylab = "Tiempo en segundos", 
     main = "Modelo estimado de defecacion a 15 grados" )

##Ajusrtado 10##
A_F  <-117.59
A_0  <-50.73
beta <-0.1799
defecacion152<-function(vector){
  
  return((A_F-A_0)*(1- exp(-beta*vector)) + A_0)
  
}

dias2<-seq(1,40,0.001)
def2<-defecacion152(dias2)
plot(dias2,def2, type = "l", col = "red", lwd = 3, ylab = "Tiempo en segundos", 
     main = "Modelo ajustado de defecacion a 15 grados(10)" )
##Ajustado 12##
A_F  <-111.47
A_0  <-48.84
beta <-.2211
defecacion153<-function(vector){
  
  return((A_F-A_0)*(1- exp(-beta*vector)) + A_0)
  
}

dias3<-seq(1,40,0.001)
def3<-defecacion153(dias3)
plot(dias3,def3, type = "l", col = "red", lwd = 3, ylab = "Tiempo en segundos", 
     main = "Modelo ajustado de defecacion a 15 grados(12)" )
#Grafica#
plot(dias,def, type = "l", col = "red", lwd = 2, ylab = "Tiempo en segundos", 
     main = "Modelos de saturacion para defecacion a 15 grados", ylim = c(60,120) )
lines(dias2, def2, col = "blue", lwd = 2)
lines(dias3, def3, col = "dark green", lwd = 2)