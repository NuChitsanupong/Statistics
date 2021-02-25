setwd("???") 

df <- read.csv("uspop.csv", sep=",", header=T, 
               fileEncoding="UTF-8-BOM")
attach(df)

x <- year - 1790
y <- population

b0_start <- 350      # asymtote
b1_start <- log((b0_start/y[1]) - 1)                # population size at x = 0
b2_start <- log((b0_start/y[2]) - 1) - b1_start     # growth rate

model <- nls(y~b0/(1+exp(b1+b2*x)),
             start=list(b0=b0_start,
                        b1=b1_start,
                        b2=b2_start))
summary(model)

#get some estimation of goodness of fit
cor(y,predict(model))
plot(x,y)
lines(x,predict(model),col="red")