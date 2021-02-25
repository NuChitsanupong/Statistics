setwd("???")  

df <- read.csv("detergent.csv", sep=",", header=T, 
               fileEncoding="UTF-8-BOM")

detergentFactor <- factor(df$Detergent)
penFactor <- factor(df$Pen)
anovaResult <- aov(df$Response ~ detergentFactor + penFactor, data=df)
summary(anovaResult)

qf(0.05, 2, 6, lower.tail=F)   # pen
qf(0.05, 3, 6, lower.tail=F)   # detergent

qtukey(0.05, nmeans=3, df=6, lower.tail=F)
qtukey(0.05, nmeans=4, df=6, lower.tail=F)

tk1 <- TukeyHSD(anovaResult, 'penFactor', conf.level = 0.95)
plot(tk1)

tk2 <- TukeyHSD(anovaResult, 'detergentFactor', conf.level = 0.95)
plot(tk2)


boxplot(df$Response~df$Pen, horizontal=T, 
        xlab="Pen", ylab="Response")

boxplot(df$Response~df$Detergent, horizontal=T, 
        xlab="Detergent", ylab="Response")





