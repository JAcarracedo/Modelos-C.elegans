#NOTA: Limpiar ambiente entre ensayos
#bombeo 15
bombeo15out<-read.csv("15 grados - bombeo 15 outliers.csv",T)
attach(bombeo15out)
b15out<- bombeo15out
b15out
boxplot(b15out[,2:11], main = "Bombeo a 15 grados", ylab = "Bombeos en 30 Segundos")
#bombeo 20
bombeo20out<-read.csv("20 Grados - bombeo 20 outliers.csv",T)
attach(bombeo20out)
b20out<- bombeo20out
b20out
boxplot(b20out[,2:11], main = "Bombeo a 20 grados", ylab = "Bombeos en 30 Segundos")
#defecacion 15
defecacion15out<-read.csv("Defecacion - Defecacion 15 outliers.csv",T)
attach(defecacion15out)
d15out<- defecacion15out
d15out
boxplot(d15out[,1:10], main = "Defecación a 15 grados", ylab = "Tiempo en segundos")
#defecacion 20
defecacion20out<-read.csv("Defecacion - Defecacion 20 outliers.csv",T)
attach(defecacion20out)
d20out<- defecacion20out
d20out
boxplot(d20out[,1:10], main = "Defecación a 20 grados", ylab = "Tiempo en segundos")


#Soluciones 
#bombeo 20 
bombeo20out<-read.csv("20 Grados - bombeo 20 outliers.csv",T)
attach(bombeo20out)
b20out<- bombeo20out
b20out
boxplot(b20out[,2:11], main = "Solución 1A", ylab = "Bombeos en 30 Segundos")
boxplot(b20out[,2:11], main = "Solución 2A", ylab = "Bombeos en 30 Segundos")
boxplot(b20out[,2:11], main = "Solución 3A", ylab = "Bombeos en 30 Segundos")
boxplot(b20out[,2:11], main = "Solución 4A", ylab = "Bombeos en 30 Segundos")
#bombeo 15
bombeo15out<-read.csv("15 grados - bombeo 15 outliers.csv",T)
attach(bombeo15out)
b15out<- bombeo15out
b15out
boxplot(b15out[,2:11], main = "Solución 1B", ylab = "Bombeos en 30 Segundos")
boxplot(b15out[,2:11], main = "Solución 2B", ylab = "Bombeos en 30 Segundos")
boxplot(b15out[,2:11], main = "Solución 3B", ylab = "Bombeos en 30 Segundos")
#defecacion 20
defecacion20out<-read.csv("Defecacion - Defecacion 20 outliers.csv",T)
attach(defecacion20out)
d20out<- defecacion20out
d20out
boxplot(d20out[,1:10], main = "Solución 1C", ylab = "Tiempo en segundos")
boxplot(d20out[,1:10], main = "Solución 2C", ylab = "Tiempo en segundos")
boxplot(d20out[,1:10], main = "Solución 3C", ylab = "Tiempo en segundos")
#defecacion 15
defecacion15out<-read.csv("Defecacion - Defecacion 15 outliers.csv",T)
attach(defecacion15out)
d15out<- defecacion15out
d15out
boxplot(d15out[,1:10], main = "Solución 1D", ylab = "Tiempo en segundos")
boxplot(d15out[,1:10], main = "Solución 2D", ylab = "Tiempo en segundos")
boxplot(d15out[,1:10], main = "Solución 3D", ylab = "Tiempo en segundos")

#Modelos
#Oscilador de bombeo 15
bombeo15out<-read.csv("15 grados - bombeo 15 outliers.csv",T)
attach(bombeo15out)
b15out<- bombeo15out
b15out
boxplot(b15out[,2:11], main = "Modelo de oscilador amortiguado para bombeo a 15 grados", ylab = "Bombeos en 30 Segundos")
#Saturacion defecacion 20
defecacion20out<-read.csv("Defecacion - Defecacion 20 outliers.csv",T)
attach(defecacion20out)
d20out<- defecacion20out
d20out
boxplot(d20out[,1:10], main = "Modelo de saturacion para Defecación a 20 grados", ylab = "Tiempo en segundos")
#Saturación defecacion 15
defecacion15out<-read.csv("Defecacion - Defecacion 15 outliers.csv",T)
attach(defecacion15out)
d15out<- defecacion15out
d15out
boxplot(d15out[,1:10], main = "Modelo de saturacion para Defecación a 15 grados", ylab = "Tiempo en segundos")

#Otros ensayos
#Coleteo 20
coleteo20<-read.csv("otros ensayos - coleteo 20.csv",T)
attach(coleteo20)
c20<- coleteo20
c20
boxplot(c20[,2:11], main = "Ensayo de coleteo a 20 grados", ylab = "Coleteos en un minuto")
#coleteo15
coleteo15<-read.csv("otros ensayos - coleteo 15.csv",T)
attach(coleteo15)
c15<- coleteo15
c15
boxplot(c15[,2:11], main = "Ensayo de coleteo a 15 grados", ylab = "Coleteos en un minuto")
#bombeo 25
bombeo25<-read.csv("otros ensayos - bombeo25.csv",T)
attach(bombeo25)
b25<- bombeo25
b25
boxplot(b25[,2:7], main = "Ensayo de bombeo a 25 grados", ylab = "Bombeos en 30 Segundos")
