# Dan Eduardo González García 
# 20/05/2024
# 1977438

# Experimento ganancia en peso (GP) basado en diferentes 

# Niveles de factor: 4 (die1, die2, die3, die4)

die1 <- c(2.4, 2.2, 3.4, 1.6)
die2 <- c(2.2, 1.9, 1.7, 2.1)
die3 <- c(3.3, 1.3, 2.8, 2.1)
die4 <- c(1.6, 2.5, 1.4, 2.4)

# Para peso bajo sumar las ganancias de peso 
sum(die1[1]+die2[1]+sum(die3[1]+sum(die4[1])
sum(die1[2]+die2[2]+sum(die3[2]+sum(die4[2])
sum(die1[3]+die2[3]+sum(die3[3]+sum(die4[3])
sum(die1[4]+die2[4]+sum(die3[4]+sum(die4[4])
                        
# Sumatoria de las dietas independientes de grupo/bloque
sum(die1); sum(die2); sum(die3); sum(die4)



GP <- c(die1, die2, die3, die4),

trat <- gl(4,4,16, labels = c("die1", "die2", "die3", "die4")),
Bloq <- gl (4,4,16, labels = c("Bajo", "Normal", "SP", "OB")),

Dietas <- data.frame (trat, Bloq, GP),
head(Dietas),

boxplot(Dietas$GP ~ Dietas$trat,
        col = "salmon",
        xlab = "Dietas",
        ylab = "Ganancia en peso (kg)"),


tapply(Dietas$GP, Dietas$Trat, var)


fligner.test(Dietas$GP, Dietas$Trat)
barlett.test(Dietas$GP ~ Dietas$Trat)

diet.aov <-(Dietas$GP ~ Dietas$Trat)
summary(diet.aov)

Dietas2 <- read.csv("Scripts/Dietas.csv", header = T)
Dietas2$trat <- as.factor(Dietas2$trat)
Dietas2$Bloq <- as.factor(Dietas2$Bloq)

Di2.aov <-(Dietas$GP ~ Dietas$trat + Dietas$Bloq)
summary(Di2.aov)