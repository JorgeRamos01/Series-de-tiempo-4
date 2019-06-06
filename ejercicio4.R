#Autor: Jorge Luis Ramos Zavaleta
#Tarea 4: modulo de series de tiempo
#Ejercicio 4

library(readr)
library(tseries)
library(forecast)
library(vars)
datos2 <- read_csv("Documentos/Econometria y matrices aleatorias/Tarea 4/datos2.csv")
datos2<-as.data.frame(datos2)
datos2<-log(datos2[,-1])

##Probamos para establecer la estacionarionaridad de las series
adf.test(diff(datos2$M0),k=1) #Estas 3 no eran estacionarias, pero con la primera diferencia lo son
adf.test(diff(datos2$INPC),k=1)
adf.test(diff(datos2$Desocupacion),k=1)

datos3<-as.data.frame(apply(datos2,2, diff))

#Generamos algunos graficos de ACF y PACF para ver como se comportan los lags de nuestras series
par(mfrow=c(1,2))

acf(datos3$M0)
pacf(datos3$M0)

acf(datos3$INPC)
pacf(datos3$INPC)

acf(datos3$Desocupacion)
pacf(datos3$Desocupacion)

#Graficamos las series diferenciadas
par(mfrow=c(1,1))
plot(datos3$INPC, type="l", col="blue", ylim=c(-0.15,0.15), ylab="y", xlab="Tiempo")
lines(datos3$M0, col="red")
lines(datos3$Desocupacion, col="orange")
legend(140,-0.05, col=c("blue","red","orange"), legend=c("INPC","M0","Des"),lty=1)



#Buscamos el rezago optimo considerando criterios de informacion usando modelos de tipo const
VARselect(datos3, lag.max=25,
          type="trend",season=12)[["selection"]]

#Generamos modelos de tipo trend iniciando con 12 rezagos bajo el criterio HQ(n)
var1 <- VAR(datos3, p=12, type="trend", season=12)
serial.test(var1, lags.pt=25, type="PT.asymptotic")


#Realizamos una prueba para revisar la heterocedasticidad
arch.test(var1)

#Realizamos una prueba para identificar causalidad en el sentido de Granger
causality(var1, cause="M0")$Granger$p.value
causality(var1, cause="INPC")$Granger$p.value
causality(var1, cause="Desocupacion")$Granger$p.value

#Iniciando la prediccion
a<-predict(var1, n.ahead = 2, ci = 0.95)
fanchart(a, colors = c("red","blue", "green"))


sept_log<-a$fcst$INPC[1,1]+datos2$INPC[nrow(datos2)]
septiembre<-exp(sept_log)
octubre<-exp(a$fcst$INPC[2,1]+sept_log)

interanual<-((octubre-96.7)/96.7)*100
interanual
