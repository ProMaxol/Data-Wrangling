library(dplyr)
library(lubridate)
library(tidyverse)
library(readxl)


# Primera Parte -----------------------------------------------------------



eclipse <- dmy_hms("21 august 2017 18:26:40")
Synodic_Month <- days(29) + hours(12) + minutes(44) + seconds(3)
Saros <- Synodic_Month*223
segundoeclipse <- eclipse + Saros

paste("El siguiente eclipse el :", segundoeclipse)




# Segunda Parte -----------------------------------------------------------



a <- read_excel("data.xlsx")


a$`Fecha Creación` <- dmy(a$`Fecha Creación`)
a$`Fecha Final` <- dmy(a$`Fecha Final`)
a$Mes <- month(a$`Fecha Creación`)

a <- na.omit(a)


#1
b <- a %>% filter(a$Call == 1,) %>% 
  select(Cod, Call, Mes) %>% 
  group_by(Mes, Cod) %>%
  summarise(llamadas = n()) %>% arrange(-llamadas)

# Meses mayores , Mayo:314, Marzo:306, Diciembre:300.




#2
a$dia <- weekdays(a$`Fecha Creación`)
c <- a %>% group_by(dia) %>% summarise(consultas = n()) %>% arrange(-consultas)

# El dia mas ocupado es el lunes





#3
d <- a %>% group_by(Mes) %>%summarise(consultas = n()) %>% arrange(-consultas)

# El mes mas ocupado es Marzo 




#4
llamdas <- a %>% group_by(Mes) %>% summarise(llamadas = sum(Call)) %>% arrange(-llamadas)
hist(llamdas$llamadas)

# si se analiza por rango entre 280 - 300 si hay una concentracion pero individualmente no.





#5
a$tiempo <- (a$`Hora Final` - a$`Hora Creación`)/60
tiempopromedio <- a %>% 
  filter(a$Call == 1,) %>% 
  summarise(Promedio = mean(tiempo))

#el promedio de las llamadas es de 14.61 minutos.




#6
e <- a %>% select(tiempo)
tablafrecuencias <- as.data.frame(table(e))
colnames(tablafrecuencias) <- c("Tiempo de llamada en minutos" , "Cantidad de llamadas")




# Tercera Parte -----------------------------------------------------------


Mi_signo <- function(x){
signo <-  c("capricornio", "acuario", "piscis", "aries", "tauro", "géminis", "cáncer", "leo", "virgo", "libra", "escorpio", "sagitario")
fechas <- c(20, 19, 20, 20, 21, 21, 22, 22, 22, 22, 22, 21)

dia <- as.numeric((readline(prompt = "Que dia naciste ( numero de día ) ")))
mes <-  as.numeric((readline(prompt = "Que mes naciste ( numero de mes ) ")))



if (dia>fechas[mes] ) {
  mes=mes+1
}
if (mes==13) {
  mes=1
} 

 return(paste("Tu signo es: ",signo[mes]))
}





