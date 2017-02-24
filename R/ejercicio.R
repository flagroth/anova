library(dplyr)
library(effects)
library(tidyr)
library(ggplot2)

datos <- read.csv("data/ACMETelephoneABT.csv")

# Corregir NAs y unificar valores en regionType

datos$regionType[which(datos$regionType == " unknown")] <- NA
datos$regionType[which(datos$regionType == " ")] <- NA
datos$regionType[which(datos$regionType == " r")] <- " rural"
datos$regionType[which(datos$regionType == " s")] <- " suburban"
datos$regionType[which(datos$regionType == " t")] <- " town"
datos$regionType = factor(datos$regionType, levels=c(levels(datos$regionType), " otro"))
datos$regionType[is.na(datos$regionType)] <- " otro"
datos$regionType <- droplevels(datos$regionType)

datos <- datos[which(datos$avgBill>0 & datos$avgMins>0),]
datos <- datos[which(datos$avgBill>0 | datos$avgMins>0),]

summary(datos$avgBill)
summary(datos$avgMins)
summary(datos$regionType)

# Ejercicio 1

model1 <- lm(log(avgBill)~log(avgMins)+regionType,data = datos, na.action = NULL)
summary(model1)
# Ordenada origen                                                            b0=2.148986
# Incremento de avgBill en grupo rural (ref) incremento unitario en avgMins  b1=0.308630
# Incremento de avgBill en grupo suburban por incremento unitario en avgMins b0+b2=2.148986+(-0.025620)
# Incremento de avgBill en grupo town con avgMins constante                  b0+b3=2.148986+(-0.023006)
# Incremento de avgBill en grupo otro con avgMins constante                  b0+b4=2.148986+(-0.003376)
# La factura es independiente de la regionType (p-value!=0)

predicted_fit = predict(model1,interval="confidence")
datos %>% mutate(fit = predicted_fit[,1],
                      lwr = predicted_fit[,2],
                      upr = predicted_fit[,3]) %>%
  ggplot(mapping=aes(x=log(avgBill), y=log(avgMins), color=regionType)) + geom_point() + geom_smooth(se=FALSE,method="lm") +
  ggtitle("Rectas de regresión por grupo")


# Ejercicio 2

anova(model1)

# Ejercicio 3

e <- effect("regionType", model1)

plot(e)

