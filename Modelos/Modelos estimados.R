#Modelo de oscilador armonico para bombeo a 15 grados
 
#determinacion de parametro gamma
f3b<-read.csv("coeficientes de friccion - bombeo15 3B.csv",T)
attach(f3b)
B3 <- lm(log(X) ~ T)
B3
plot(B3)
#Modelo de exponencial 
A <- .05028
B_0 <- 105
bombeo <-function(vector){
  
  return(exp(-A*vector)*( B_0))
  
}

dias <-seq(1,10,0.001)
bom <-bombeo(dias)
plot(dias,bom, type = "l", col = "red", lwd = 3)
#Modelo de oscilador armonico amortiguado
A <- .05028
W <- 0.5282
B_0 <- 55
bombeo <-function(vector){
  
  return(exp(-A*vector)*(B_0*cos(W*vector)) + B_0)
  
}

dias <-seq(1,10,0.001)
bom <-bombeo(dias)
plot(dias,bom, type = "l", col = "red", lwd = 3 )
Est <- A^2 - (4*W)
Est
#Estabilidad
Est <- A^2 - (4*W)
Est


#Modelo de saturacion para defecacion a 20 grados
A2_F  <-96
A2_0  <-40
beta2 <-.16
defecacion20<-function(vector){
  
  return((A2_F-A2_0)*(1- exp(-beta2*vector))+ A2_0)
  
}

dias<-seq(1,10,0.001)
def20<-defecacion20(dias)
plot(dias,def20, type = "l", col = "red", lwd = 3)


#Modelo de saturacion para defecacion a 15 grados
#opcion 1 beta estimada
library(drc)
library(nlme)
library(aomisc)
X <- 1:10
Y <- c(62, 72, 76, 77, 92, 96, 103, 92.5, 108, 104.5)
model <- drm(Y ~ X,fct=DRC.asymReg())
plot(model, log="", main = "Asymptotic regression")
model
model$fit

A_F  <-104
A_0  <-55
beta <-0.1489
defecacion15<-function(vector){
  
  return((A_F-A_0)*(1- exp(-beta*vector)) + A_0)
  
}

dias<-seq(1,10,0.001)
def<-defecacion15(dias)
plot(dias,def, type = "l", col = "red", lwd = 3)


#opcion 2 beta manual
A_F  <-104
A_0  <-52
beta <-0.2
defecacion15<-function(vector){
  
  return((A_F-A_0)*(1- exp(-beta*vector)) + A_0)
  
}

dias<-seq(1,10,0.001)
def<-defecacion15(dias)
plot(dias,def, type = "l", col = "red", lwd = 3)


