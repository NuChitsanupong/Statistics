setwd("????")  

df <- read.csv("tomato.csv", sep=",", header=T, 
               fileEncoding="UTF-8-BOM")

varietyFactor <- factor(df$Variety)
densityFactor <- factor(df$Density)
anovaResult <- aov(df$Yield ~ varietyFactor * densityFactor, data=df)
summary(anovaResult)

qf(0.01, 6, 24, lower.tail=F)   # interaction
qf(0.01, 2, 24, lower.tail=F)   # variety
qf(0.01, 3, 24, lower.tail=F)   # density

tk1 <- TukeyHSD(anovaResult, 'varietyFactor', conf.level = 0.99)
plot(tk1)
tk2 <- TukeyHSD(anovaResult, 'densityFactor', conf.level = 0.99)
plot(tk2)

boxplot(df$Yield~df$Variety, horizontal=T,
        xlab="Variety", ylab="Yield")

boxplot(df$Yield~df$Density, horizontal=T,
        xlab="Density", ylab="Yield")

interaction.plot(df$Variety, df$Density, df$Yield)

