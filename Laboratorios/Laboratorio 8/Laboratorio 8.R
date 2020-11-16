# Marlon Tzorin 20180072

library(tidyverse)
library(lubridate)
library(caret)
library(devtools)
library(naniar)
library(DescTools)
library(NCmisc)
library(DT)

# Missing Data ------------------------------------------------------------

titanic_MD <- read_csv("titanic_MD.csv")

# Para poder aplicar mayores metodos cambiare dos varaibles categorias a numeros
# siguen siendo categoricas pero simplifican el codigo y razonamiento
titanic_MD$Sex <-  na_if(x = titanic_MD$Sex,y = "?")
titanic_MD$Sex <-  ifelse(test = titanic_MD$Sex == "female",yes = 1,no = 0)
titanic_MD$Embarked <- ifelse(test = titanic_MD$Embarked == "C",yes = 0,no = 
                                ifelse(test = titanic_MD$Embarked == "Q",yes = 1,no =
                                         ifelse(test = titanic_MD$Embarked == "S",yes = 2,no = "Hola roto")))

# Reporte de NA's ---------------------------------------------------------

summary(titanic_MD)


# Tratamiento de missing values -------------------------------------------

#Sex , para esta columna llenaria los NA´s manteniendo la distribucion actual de los valores
# con esta linea : 
table(as.data.frame(titanic_MD$Sex))  #lo cual mantendria la relacion 51% hombres y 49% mujeres

#Age , para esta columna llenaria los NA´s con la mediana
mean(titanic_MD$Age,na.rm = T) # lo cual redondaria a los 36 años de edad

#SibSP , ya que esta variable solo tiene 3 NA´s no creo que haga mucho ruido sin embargo usaria la moda la cual es 0
DescTools::Mode(x = titanic_MD$SibSp,na.rm = T)

#Parch, ya que este representa humanos tambien no puedo usar media o mediana por lo cual usaria moda tambien. Lo cual tambien es 0
table(as.data.frame(titanic_MD$Parch))
DescTools::Mode(x = titanic_MD$Parch,na.rm = T)


#fare , para esta variable usaria la mediana como remplazo para los NA´s , la cual seria de 56.92
median(x = titanic_MD$Fare,na.rm = T)


#Embarked , ya que este representa puertos algo fisico no puedo usar media o mediana por lo cual usaria moda tambien. Lo cual es 2 "S"
DescTools::Mode(x = titanic_MD$Embarked,na.rm = T)



# Columnas Completas ------------------------------------------------------


#Passenger ID no contiene NA´s sin embargo hay numero que se saltan
summary(titanic_MD$PassengerId)

#Survived no contiene NA´s y la iformación represeta a los muertos
summary(titanic_MD$Survived)

#Pcllas no contiene NA´s y representa la clase a la que pertenecio
summary(titanic_MD$Pclass)

#Name  no contiene NA´s y representa los nombres de los pasajeros

#Ticket, no contiene NA´s y representa los tickets de los pasajeros.
summary(titanic_MD$Ticket)

#Cabin, no contienes NA´s y representan el numero de cabina
summary(titanic_MD$Cabin)


na.omit(titanic_MD)


# Metodos -----------------------------------------------------------------

correlations <- cor(x = titanic_MD[,c(1:3,5:8,10)],use = "pairwise.complete.obs")


imputaciones <- function(x){
  a <- mean(x = x,na.rm = T)
  a1 <- replace_na(data = x,replace = a)
  b <- median(x = x,na.rm = T)
  b1 <- replace_na(data = x,replace = b)
  c <- as.numeric(DescTools::Mode(x = x,na.rm = T))
  c1 <- replace_na(data = x,replace = c)
  im <- as.data.frame(cbind(a1,b1,c1))
  names(im) <- c("Media","Mediana","Moda")
  return(im)
}

sectorizada <- function(x){
  mod <-  as.numeric(DescTools::Mode(x = x,na.rm = T))
  mod2 <- ifelse(test = is.na(x),yes = mod,x)
  #mod3 <- ifelse(test = x == "titanic_MD$Sex",
                 #yes = ifelse(test = mod2 == 1, yes = "Female",no = "Male" ),
                 #no = ifelse(test = mod2 == 0 , yes = "C",
                              #no = ifelse(test = mod2 == 1,yes = "Q",no = "S" )))
  g <- data.frame(mod2)
  return(g)
}


# regresion ---------------------------------------------------------------


# regresion <- function(x,x1){
#   x <- lm(x ~ x1,titanic_MD )
#   j <- titanic_MD[[x1]]
#   j <- ifelse(test = is.na(j),yes = round(mean(titanic_MD[[x1]],na.rm = T),0),no = j)
#   p <- ifelse(test = is.na(titanic_MD[[x]]),yes = ((j*x$coefficients[2])+x$coefficients[1]),no = titanic_MD[[x]])
#   y <- as.data.frame(p)
#   names(y) <- c("Regresión")
#   return(p)
# }

# la regresion no la logre hacer me tira un error de factores 


# outliers ----------------------------------------------------------------


outliers <- function(x){
  z <- na.omit(x)
  media <- mean(z)
  stand <- sd(z)
  desvest <- ifelse(test = z > media+(2*stand),
                           yes = round(media+(2*stand),0),
                          no = ifelse(test = z< media-(2*stand),
                                      yes = round(media-(2*stand),0),no = z))
  y <- na.omit(x)
  top <- quantile(y,0.9)
  low <- quantile(y,0.1)
  percentil <- ifelse(test = y > top,yes = top,
                          no = ifelse(test = y< low,
                                      yes = low,no = y))
  l <- as.data.frame(cbind(desvest,percentil))
  names(l) <- c("Desviación Estandar","Percentiles")
  return(l)
}


# Sex --------------------------------------------------------------------

# para sex no se puede aplicar imputaciones de media o mediana por tratarse 
# de personas , sin embargo como lo pase a numero se puede ver que sale
# pero igual no tiene logica

Imputaciones_Sex <- imputaciones(titanic_MD$Sex)
Outliers_Sex <- outliers(titanic_MD$Sex)


# la que se tiene que usar es la sectorizada

Sectorizada_Sex <- sectorizada(titanic_MD$Sex)

# Age ---------------------------------------------------------------------

# para la variable age si aplica las imputaciones y outliers

Imputaciones_Age <- imputaciones(titanic_MD$Age)
Outliers_Age <- outliers(titanic_MD$Age)



# SibSp -------------------------------------------------------------------

# para la variable SibSp si aplica las imputaciones y outliers

Imputaciones_SibSp <- imputaciones(titanic_MD$SibSp)
Outliers_SibSp <- outliers(titanic_MD$SibSp)



# Parch -------------------------------------------------------------------
# para la variable Parch si aplica las imputaciones y outliers

Imputaciones_Parch <- imputaciones(titanic_MD$Parch)
Outliers_Parch <- outliers(titanic_MD$Parch)


# Fare --------------------------------------------------------------------
# para la variable Fare si aplica las imputaciones y outliers

Imputaciones_Fare <- imputaciones(titanic_MD$Fare)
Outliers_Fare <- outliers(titanic_MD$Fare)


# Embarked ----------------------------------------------------------------
# para Embarked no se puede aplicar imputaciones de media o mediana por tratarse 
# de tickets unicos , sin embargo como lo pase a numero se puede ver que sale
# pero igual no tiene logica

Imputaciones_Embarked <- imputaciones(titanic_MD$Embarked)
Outliers_Embarked <- outliers(titanic_MD$Embarked)

# la que se tiene que usar es la sectorizada

Sectorizada_Embarked <- sectorizada(titanic_MD$Embarked)



# Comparación -------------------------------------------------------------

titanic_CP <- read_csv("titanic.csv")
titanic_CP$Sex <-  ifelse(test = titanic_CP$Sex == "female",yes = 1,no = 0)
titanic_CP$Embarked <- ifelse(test = titanic_CP$Embarked == "C",yes = 0,no = 
                                ifelse(test = titanic_CP$Embarked == "Q",yes = 1,no =
                                         ifelse(test = titanic_CP$Embarked == "S",yes = 2,no = "Hola roto")))
id <- 1:183

Sex <- cbind(titanic_CP$Sex,Imputaciones_Sex,Sectorizada_Sex)
names(Sex) <- c("Real","Media","Mediana","Moda","Sectorizado")
Age <- cbind(titanic_CP$Age,Imputaciones_Age)
names(Age) <- c("Real","Media","Mediana","Moda")
SibSp <- cbind(titanic_CP$SibSp,Imputaciones_SibSp)
names(SibSp) <- c("Real","Media","Mediana","Moda")
Parch <- cbind(titanic_CP$Parch,Imputaciones_Parch)
names(Parch) <- c("Real","Media","Mediana","Moda")
Fare <- cbind(titanic_CP$Fare,Imputaciones_Fare)
names(Fare) <- c("Real","Media","Mediana","Moda")
Embarked <- cbind(titanic_CP$Embarked,Imputaciones_Embarked,Sectorizada_Embarked)
names(Embarked) <- c("Real","Media","Mediana","Moda","Sectorizado")

summary(Sex)
summary(Age)
summary(SibSp)
summary(Parch)
summary(Fare)
summary(as.integer(Embarked))

# para sex basandonos en las estadisticas descripitvas el que mas se parece es el sectorizado y el unico que aplica

# para age basandonos en las estadisticas descripitvas el que mas se parece es el metodo de mediana por sus rangos y parecidos

# Para SibSp basandonos en las estadisticas descripitvas el que mas se parece es el metodo de media ya que predomina la gente sin hermanos

# para parch basandonos en las estadisticas descripitvas el que mas se parece es el metodo de media ya que predomina la gente sin familaires

# para Fare basandonos en las estadisticas descripitvas el que mas se parece es el metodo de moda ya que casi tiene las mismas estadisticas

# para embarked basandonos en las estadisticas descripitvas el que mas se parece es el sectorizado y el unico que aplica



# Conclusiones -------------------------------------------------------------

#No hubo ningun metodo de imputacion especifico que haya predominado como el que se parecia mas a los datos reales
# sin embargo, me falto analizar la regresión lineal para poder analizar todos los diferentes modelos




# Parte 2 , Normalización -------------------------------------------------

titanic_MD$Sex <- Sex$Mediana
titanic_MD$Fare <- Fare$Moda

# solo se puede normalizar Age y Fare y para compara tengo que normalizar los misssing y completos

NAGEMD <- titanic_MD %>%
  mutate(AgeN = (Age-mean(Age,na.rm = T))/sd(Age,na.rm = T)) %>% 
  select(AgeN)
NFAREMD <- titanic_MD %>% 
  mutate(FareN = (Fare-mean(Fare,na.rm = T))/sd(Fare,na.rm = T)) %>% 
  select(FareN)
MMAGEMD <- titanic_MD %>% 
  mutate(AgeMM = (Age-min(Age,na.rm = T)) / (max(Age,na.rm = T)-min(Age,na.rm = T))) %>% 
  select(AgeMM)
MMFAREMD<- titanic_MD %>% 
  mutate(FareMM = (Fare-min(Fare,na.rm = T)) / (max(Fare,na.rm = T)-min(Fare,na.rm = T))) %>% 
  select(FareMM)
MAAGEMD <- titanic_MD %>% 
  mutate(AgeMA = (Age - mean(c(max(Age,na.rm = T),min(Age,na.rm = T)))) / (max(Age,na.rm = T) - mean(c(max(Age,na.rm = T),min(Age,na.rm = T))))) %>% 
  select(AgeMA)
MAFAREMD <- titanic_MD %>% 
  mutate(FareMA = (Fare - mean(c(max(Fare,na.rm = T),min(Fare,na.rm = T)))) / (max(Fare,na.rm = T) - mean(c(max(Fare,na.rm = T),min(Fare,na.rm = T))))) %>% 
  select(FareMA)

NAGECP <- titanic_CP %>% 
  mutate(AgeN = (Age-mean(Age))/sd(Age)) %>% 
  select(AgeN)
NFARECP <- titanic_CP %>% 
  mutate(FareN = (Fare-mean(Fare))/sd(Fare)) %>% 
  select(FareN)
MMAGECP <- titanic_CP %>% 
  mutate(AgeMM = (Age-min(Age)) / (max(Age)-min(Age))) %>% 
  select(AgeMM)
MMFARECP <- titanic_CP %>% 
  mutate(FareMM = (Fare-min(Fare)) / (max(Fare)-min(Fare))) %>%
  select(FareMM)
MAAGECP <- titanic_CP %>% 
  mutate(AgeMA = (Age - mean(c(max(Age),min(Age)))) / (max(Age) - mean(c(max(Age),min(Age))))) %>% 
  select(AgeMA)
MAFARECP <- titanic_CP %>% 
  mutate(FareMA = (Fare - mean(c(max(Fare),min(Fare)))) / (max(Fare) - mean(c(max(Fare),min(Fare))))) %>% 
  select(FareMA)


# comparacion -------------------------------------------------------------


summary(NAGEMD)
summary(NAGECP)
summary(MMAGEMD)
summary(MMAGECP)
summary(MAAGEMD)
summary(MAAGECP)
summary(NFAREMD)
summary(NFARECP)
summary(MMFAREMD)
summary(MMFARECP)
summary(MAFAREMD)
summary(MAFARECP)



