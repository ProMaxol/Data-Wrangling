library(tidyverse)
library(readr)
library(readxl)
library(dplyr)
library(highcharter)
library(reshape2)
library(ggplot2)
library(stringr)
library(writexl)

data <- read_csv("tabla_completa2.csv")

Clientes <- c("TAQUERIA EL CHINITO","UNIVERSIDAD FRANCISCO MARROQUIN","BAR LA OFICINA",
              "EL PINCHE OBELISCO","ABARROTERIA EBENEZER","TIENDA LA BENDICION",
              "UBIQUO LABS","EL GALLO NEGRO","SPORTA, S.A.","POLLO PINULITO",
              "CHICHARRONERIA EL RICO COLESTEROL","HOSPITAL ROOSEVELT","HOSPITAL LAS AMERICAS")

data2 <- as.character(data$CLIENTE)

tviajes <- str_replace_all(string = data2,pattern = paste(Clientes,collapse = "|"),replacement = "")


dc <- c("/ Despacho a cliente","/Despacho a cliente")

data2 <-str_replace_all(string = data2,pattern = paste(dc,collapse = "|"),replacement = "") 

f <- c("(\\|\\|\\|)Faltante")
data2 <-str_replace_all(string = data2,pattern = f,replacement = "") 

FA <- c("(\\|\\|\\|)FALTANTE")
data2 <-str_replace_all(string = data2,pattern = FA,replacement = "")

DV <- c("(\\|\\|\\|)DEVOLUCION")
data2 <-str_replace_all(string = data2,pattern = DV,replacement = "") 


b1 <- c("\\|")
data2 <-str_replace_all(string = data2,pattern = b1,replacement = "")



b2 <- c("Faltante","FALTANTE")
tviajes <- str_replace_all(string = tviajes,pattern = b1,replacement = "")
tviajes <- str_replace_all(string = tviajes,pattern = paste(dc,collapse = "|"),replacement = "Despacho a cliente")
tviajes <- str_replace_all(string = tviajes,pattern = "DEVOLUCION",replacement = "Devolucion")
tviajes <- str_replace_all(string = tviajes,pattern = paste(b2,collapse = "|"),replacement = "Falto Producto")


data$CLIENTE <- data2
TablaLimpia <- as.data.frame(cbind(data[,1:3],tviajes,data[,4:length(data)]))
TablaLimpia$X1 = NULL



writexl::write_xlsx(TablaLimpia,path = "TablaLimpia.xlsx")
TablaLimpia %>% summarise(clinetes =n_distinct(CLIENTE))
