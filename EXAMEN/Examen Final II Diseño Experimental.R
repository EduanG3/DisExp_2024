### Dan Eduardo Gonzalez Garcia
### 1977438
### 30/05/2024


data(InsectSprays)
head(InsectSprays)

summary(InsectSprays)

boxplot(InsectSprays$count ~ InsectSprays$spray, col="lightblue",
        ylab = "Nématodos encontrados",
        xlab = "Tipo de insecticida")

tapply(InsectSprays$count, InsectSprays$spray, sd)

ins.aov <- aov(InsectSprays$count  ~ InsectSprays$spray)
summary(ins.aov)

TukeyHSD(ins.aov)
