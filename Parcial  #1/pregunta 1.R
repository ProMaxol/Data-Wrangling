library(dplyr)

a <- readRDS(file = "data-wrangling-2020-parcial-1/parcial_anonimo.rds")


b <- a %>% select(Pais,Cliente) %>% group_by(Cliente) %>% summarise(clientes = n_distinct(Pais)) %>%  arrange(desc(clientes))
b <- b[1:7,]


c <- a %>% group_by(Cliente) %>% summarise(Ingresos=sum(Venta)) 
clientes_internacionales <- c("044118d4","a17a7558","bf1e94e9","c53868a0","f2aab44e","f676043b","ff122c3f")
CI <- match(x = clientes_internacionales,table = c$Cliente)
c <- c[CI,]

