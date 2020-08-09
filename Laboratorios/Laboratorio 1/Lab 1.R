library(dplyr)
library(readr)
library(readxl)
library(tidyverse)
library(tidytext)
library(plyr)

m1 <- read_excel("01-2018.xlsx")
m2 <- read_excel("02-2018.xlsx")
m3 <- read_excel("03-2018.xlsx")
m4 <- read_excel("04-2018.xlsx")
m5 <- read_excel("05-2018.xlsx")
m6 <- read_excel("06-2018.xlsx")

#New Var TIPO
m7 <- read_excel("07-2018.xlsx")
m9 <- read_excel("09-2018.xlsx")
m10 <- read_excel("10-2018.xlsx")
m11 <- read_excel("11-2018.xlsx")

#VAR TIPO & 10A
m8 <- read_excel("08-2018.xlsx")



# Limpieza

Clean <- function(x){
  x_clean <- x %>% dplyr::select(1:8)
  return(x_clean)
}
#substr
#list.files

m7 <- Clean(m7)
m8 <- Clean(m8)
m9 <- Clean(m9)
m10 <- Clean(m10)
m11 <- Clean(m11)

# Fechas

m1$Fecha = "01-2018"
m2$Fecha = "02-2018"
m3$Fecha = "03-2018"
m4$Fecha = "04-2018"
m5$Fecha = "05-2018"
m6$Fecha = "06-2018"
m7$Fecha = "07-2018"
m8$Fecha = "08-2018"
m9$Fecha = "09-2018"
m10$Fecha = "10-2018"
m11$Fecha = "11-2018"


# unification

Año_2018 <- rbind.data.frame(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11)

head(Año_2018)
tail(Año_2018)

# Exportation a Excel

write_excel_csv2(x = Año_2018,path = "Entegas Embotelladora 2018.xls" , delim =",")




# Problem 2

v1 <- sample(1:20,10,replace = T)
v2 <- sample(1:10,10,replace = T)
v3 <- sample(1:10,10,replace = T)

Lista_V <- list(v1,v2,v3)

# Moda

Moda <- function(x){
  vector <- unique(x)
 M <- vector[which.max(tabulate(match(x,vector)))]
  return(M)
}

Modas <- lapply(Lista_V, Moda)


# Problema 3


Vechiculos_2019 <- (read_delim(file = "INE_PARQUE_VEHICULAR_080720.txt", delim = "|"))



