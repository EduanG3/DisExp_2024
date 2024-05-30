## Dan Eduardo González García 
## 08 / 05 / 2024
## 1977438

# Importar datos  ---------------------------------------------------------

library(readr) 

file <- paste0 ("https://raw.githubusercontent.com/mgtagle/202_Analisis_Estadistico_2020/02680a60a88f56facda17fa38af265fb81f7f9f6/cuadro1.csv")

inventario <- read.csv(file)


# Seleccion de datos  -----------------------------------------------------

### ALTURA 
H.media <- subset(inventario, Altura <= mean(Altura))
H.16 <- subset(inventario, Altura < 16.5) 

### VECINOS
V.3 <- subset(inventario, Vecinos <= 3)
V.4 <- subset(inventario, Vecinos > 4)

### DIAMETRO
Dm <- subset(inventario, Diametro < mean(Diametro))
D16 <- subset(inventario, Diametro > 16)

### ESPECIE 
CR <- subset(inventario, Especie == "C")
TH <- subset(inventario, Especie == "H")
DV <- subset(inventario, Especie == "F")


# Observaciones -----------------------------------------------------------

### DIAMETRO <= 16.9 cm 
d16.9 <- subset(inventario, Diametro <= 16.9)
## d16.9 = 31 observaciones

### ALTURA > 18.5 m
a18.5 <- subset(inventario, Altura > 18.5)
## a18.5 = 2 observaciones


# Visualización datos -----------------------------------------------------

### ALTURAS

hist(inventario$Altura,
     ylab = "Frecuencia",
     xlab = "Altura",
     main = "Altura de los arboles",
     col = "brown")

hist(H.media$Altura,
     ylab = "Frecuencia",
     xlab = "Altura",
     main = "Altura media",
     col = "yellow")

hist(H.16$Altura,
     ylab = "Frecuencia",
     xlab = "Altura",
     main = "Altura menor a 16.5 m",
     col = "blue")

### VECINOS 

hist(inventario$Vecinos,
     ylab = "Frecuencia",
     xlab = "Vecinos",
     main = "Vecinos cercanos",
     col = "orange")

hist(V.3$Vecinos,
     ylab = "Frecuencia",
     xlab = "Vecinos",
     main = "Vecinos <3",
     col = "white",
     xlim = c(0,3))

hist(V.4$Vecinos,
     ylab = "Frecuencia",
     xlab = "Vecinos",
     main = "Vecinos >4",
     col = "gold",
     xlim = c(5,6))

### DIAMETRO

hist(inventario$Diametro,
     ylab = "Frecuencia",
     xlab = "Diametro",
     main = "Diametro de los arboles",
     col = "pink",
     xlim = c(5,25))

hist(Dm$Diametro,
     ylab = "Frecuencia",
     xlab = "Diametro",
     main = "Diametro media",
     col = "lightblue",
     xlim = c(6,16))

hist(D16$Diametro,
     ylab = "Frecuencia",
     xlab = "Diametro",
     main = "Diametros mayor a 16",
     col = "gold")