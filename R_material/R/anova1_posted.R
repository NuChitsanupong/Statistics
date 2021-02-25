setwd("???")   

df <- read.csv("fabric.csv", sep=",", header=T, fileEncoding="UTF-8-BOM")

polymerFactor <- factor(df$polymer)
anovaResult <- aov(df$soiling~polymerFactor, data=df)
summary(anovaResult)

qf(0.99, 2, 12)

qtukey(0.01, nmeans=anovaResult$rank, df=anovaResult$df.residual, lower.tail=F)

tk <- TukeyHSD(anovaResult, conf.level = 0.99)
plot(tk)

boxplot(df$soiling~df$polymer, horizontal=T, xlab="Strength", ylab="Soil")