# Dan Eduardo González García 
# 1977438
# 08/05/2024


# Ingresar Datos --------------------------------------------------------

# Ingresar datos del diseño aleatorio 
# 5 diseños germinativos 
# 4 repeticiones cada tratamiento 


germ <- c(3, 3, 4, 2, 7, 8, 7, 6, 8, 9, 8, 7, 6, 7, 7, 6, 3, 2, 1, 3)

trat <- gl(5, 4, 20, labels = c("Ctrl", "EM", "Ra4h", "AG", "AC"))


Exper <- data.frame(trat, germ)


boxplot(Exper$germ ~ Exper$trat, col = "green", xlab
        = "tratamientos",  ylab = "germinacion (%)")

# Revisar Normalidad 
shapiro.test(Exper$germ)

# Revisa la igualdad de varianza
bartlett.test(Exper$germ, Exper$trat)

med.trat <- tapply(Exper$germ, Exper$trat, mean)
med.trat

# Media General
MG <- mean(Exper$germ)

var.trat <- tapply(Exper$germ, Exper$trat, var)
var.trat 

Exper$SC <- (Exper$germ - MG)^2

# Suma de cuadrados del experimento SCTotal
SCtotal <- sum(Exper$SC)

# Suma de cuadrados del tratamiento SCTrat

SCTrat <- sum((med.trat-MG)^2 * 4)
SCTrat

# Suma cuadrado del error 
SCtotal - SCTrat

SCTrat/4
9.7/15
26.2/.64

# ANOVA usando funcion AOV

Exper.aov <- aov(Exper$germ ~ Exper$trat)
summary(Exper.aov)

# Existen diferencias entre los tratamientos de germinacion 
# Por lo tanto, aplicaremos una prueba de Tukey 

TukeyHSD(Exper.aov)
plot(TukeyHSD(Exper.aov))