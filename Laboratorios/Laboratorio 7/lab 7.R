library(readr)
library(lubridate)
library(dplyr)
library(writexl)
library(tidyr)
library(readxl)
library(stringr)
library(ggplot2)


c1 <- read_csv("c1.csv", col_types = cols(Camion_5 = col_number(), 
                                          Pickup = col_number(), Moto = col_number(), 
                                          factura = col_number(), directoCamion_5 = col_number(), 
                                          directoPickup = col_number(), directoMoto = col_number(), 
                                          fijoCamion_5 = col_number(), fijoPickup = col_number(), 
                                          fijoMoto = col_number()))

c1$Fecha <- dmy(c1$Fecha)
c1 <- c1[-(23:28)]

c1$`5-30` <- ifelse(c1$`5-30` == "x" , 1 , 0)
c1$`30-45` <- ifelse(c1$`30-45` == "x" , 1 , 0)
c1$`45-75` <- ifelse(c1$`45-75` == "x" , 1 , 0)
c1$`75-120` <- ifelse(c1$`75-120` == "x" , 1 , 0)
c1$`120+` <- ifelse(c1$`120+` == "x" , 1 , 0)

#writexl::write_xlsx(x = c1,path = "Data Limpia.xlsx")



# tidy data ---------------------------------------------------------------


Data_Limpia <- read_excel("Data Limpia.xlsx", 
                          col_types = c("date", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "text", "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric"))


a <- Data_Limpia[c(2,19:23)]
along <- na.omit(pivot_longer(data = a,cols =2:6 ))
names(along) <- c("ID de Refenrecia","Tiempo De Viaje","vale")
c <- Data_Limpia[c(2,12:14)]
d <- Data_Limpia[c(2,15:17)]
clong <- na.omit(pivot_longer(c,2:4))
names(clong) <- c("ID de Refenrecia","Tipo de Vehiculo ","Costo Directo")
dlong <- na.omit(pivot_longer(d,2:4))
names(dlong) <- c("ID de Refenrecia","Tipo de Vehiculo CF","Costo Fijo")


Data_Limpia <- cbind(Data_Limpia,along,clong,dlong)
Data_Limpia <- Data_Limpia[-c(12:17,19:24,26,27,30)]

Data_Limpia$`Tipo de Vehiculo ` <- str_remove_all(string = Data_Limpia$`Tipo de Vehiculo `,pattern = "directo")
Data_Limpia <- Data_Limpia[c(-4,-5,-6,-16)]
Data_Limpia$Costo_Total <- Data_Limpia$`Costo Directo`+ Data_Limpia$`Costo Fijo`
Data_Limpia$Ingreso <- Data_Limpia$factura - Data_Limpia$Costo_Total
Data_Limpia$`Tipo de Vehiculo ` <- str_remove_all(string = Data_Limpia$`Tipo de Vehiculo `,pattern = "_5")
Data_Limpia$Fecha <- ymd(Data_Limpia$Fecha)

# Preguntas Directas ------------------------------------------------------

ER <- Data_Limpia %>% summarise(Ventas = sum(factura),Costo_Operativo = sum(Costo_Total))
ER$EBITDA <- ER$Ventas - ER$Costo_Operativo
#write_xlsx(ER,"Estado de Resultados 2017.xlsx")

Unidades <- Data_Limpia %>% 
  group_by(`Tipo de Vehiculo `) %>% 
  summarise(n = n())


Tarifario <- Data_Limpia %>%
  group_by(Cod,`Tipo de Vehiculo `) %>%
  summarise(Precio = (mean(factura)))
#write_xlsx(Tarifario,"Tarifario 2017.xlsx")


Aceptabilidad <- Data_Limpia %>% 
  group_by(Fecha) %>% 
  summarise(Ingreso = sum(Ingreso)) %>% 
  arrange(Fecha)

Aceptabilidad_Clientes <- ggplot(Aceptabilidad,aes(Fecha,Ingreso)) +
  geom_point() + stat_smooth() + ggtitle("Actividad 2017") + xlab("Trimestres")

Aceptabilidad_Clientes<- Aceptabilidad_Clientes + 
  theme(plot.title = element_text(color="brown", size=20, face="bold.italic",hjust = 0.5),
              axis.title.x = element_text(color="brown", size=13, face="bold"),
              axis.title.y = element_text(color="brown", size=15, face="bold"))
ggsave("Aceptabilidad de Clientes 2017.png")
Aceptabilidad_Clientes


IngresoTotal_Tarifario <- Data_Limpia %>%
  group_by(Cod,`Tipo de Vehiculo `) %>%
  summarise(Ingreso = sum(factura)/n(),Costo = sum(Costo_Total)/n())

IngresoTotal_Tarifario$Margen <- IngresoTotal_Tarifario$Ingreso - IngresoTotal_Tarifario$Costo
IngresoTotal_Tarifario <- IngresoTotal_Tarifario[order(IngresoTotal_Tarifario$Margen),]
#write_xlsx(IngresoTotal_Tarifario,"Margenes.xlsx")

Centrosd <-  Data_Limpia %>% 
  group_by(origen,`Tipo de Vehiculo `,`Tiempo De Viaje`) %>% 
  summarise(Ingreso = sum(Ingreso)/n(),Viajes = n())

centrosdwide <- Data_Limpia %>% 
  group_by(origen,`Tipo de Vehiculo `) %>% 
  summarise(Ingreso= sum(Ingreso))
centrosdwide <- pivot_wider(data = centrosdwide,names_from = `Tipo de Vehiculo `,values_from = Ingreso)

#write_xlsx(centrosdwide,"Viajes por Origen.xlsx")

centrosd2 <- Data_Limpia %>% 
  group_by(origen) %>% 
  summarise(Viajes = n())
names(centrosd2) <- c("Centro de Distribucón","Viajes")
#write_xlsx(centrosd2,"Viajes por CD.xlsx")


ingresototal <- sum(Data_Limpia$factura)
ochentaveinte <- Data_Limpia %>% 
  group_by(ID) %>% 
  summarise(Porcentaje = sum(factura)/ingresototal)
#write_xlsx(ochentaveinte,"80-20.xlsx")


