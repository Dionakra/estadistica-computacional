####################################################################
###    ESTADÍSTICA COMPUTACIONAL
###    TEMA 4. MODELOS DE REGRESIÓN.
###    
###    GRADO EN INGENIERÍA INFORMÁTICA-INGENIERÍA DEL SOFTWARE
###    GRADO EN INGENIERÍA INFORMÁTICA- TECNOLOGÍAS INFORMÁTICAS
###    Juan M. Muñoz Pichardo - María Dolores Cubiles de la Vega
###    Departamento de Estadística e I.O. Universidad de Sevilla
####################################################################
###  SCRIPT DEL DESARROLLO TEÓRICO DEL TEMA
####################################################################

### DIRECTORIO DE TRABAJO ####################################################

setwd("L:/TRABAJOPERSONAL/DOCENCIA/ECOMPUTACIONAL/Practicas")

### LECTURA FICHERO DE DATOS ######################################
## Datos: Advertinsig.csv 
## Descripción: 
##      ventas de un producto en 200 mercados diferentes, junto con los 
##      presupuestos de publicidad del producto en cada uno de esos mercados 
##      en tres diferentes medios de comunicación: tv, radio y prensa.
## Variables: Case ; TV ; Radio ; Newspaper : Sales
###################################################################

datos <- read.table("Advertising.csv", header=TRUE, sep=",")
names(datos)

par(mfrow=c(1,3))
plot(datos$TV,datos$Sales,lwd=2, pch=16,xlab="Gastos Publicitarios en TV",ylab="Ventas")
plot(datos$Radio,datos$Sales,lwd=2,pch=16,xlab="Gastos Publicitarios en Radio",ylab="Ventas")
plot(datos$Newspaper,datos$sales,lwd=2,pch=16,xlab="Gastos Publicitarios en Prensa",ylab="Ventas")
par(mfrow=c(1,1))

##################################################################################
##                               REGRESIÓN LINEAL SIMPLE
##
##  Objetivo: Sales
##  Explicativa: TV
##
##  Orden básica: lm (linear models)
##    lm(formula, data= , ....)
##     Crea un objeto "lm" que recoge la información del modelo ajustado
##     Los datos utilizados quedan incorporados a la información del modelo a 
##       través de la especificación (por defecto) 
##                  lm(formula, data= , model=TRUE)
##       no obstante se puede eliminar con "model=FALSE", y se puede incorporar
##       la variable explicativa (x=TRUE) y/o la variable objetivo (y=TRUE)
##
##    Con la orden "anova" proporciona la Tabla de Análisis de la Varianza
##
##    Con la orden "summary" proporciona lo más relevante del modelo, en concreto:
##      - REsumen descriptivo de los residuos
##      - Tabla de coeficientes con: estimación, error de estimación, contraste 
##              sobre la nulidad del coeficiente o significación del coeficiente
##      - Error estandar residual = estimación del parámetro sigma =raiz(C.M.error)
##      - Cuadrado del Coeficiente de correlación múltiple (R2)
##      - Estimación de R2 ajustada
##      - Estadístico F del contraste ANOVA, con su p-valor
##################################################################################

sales1.mod <- lm(Sales~TV,data=datos)
sales1.mod  ## proporciona sólo las estimaciones de los coeficientes


anova(sales1.mod)
summary(sales1.mod) 

class(sales1.mod) ## proporciona información sobre el tipo de objeto
names(sales1.mod) ## proporciona los nombres de lo que contiene el objeto

sales1.mod$coefficients ## proporciona las estimaciones de los coeficientes
coef(sales1.mod)

sales1.mod$residuals    ## proporciona el vector de residuos
sales1.mod$residuals[200]

sales1.mod$rank  ## rango numérico del modelo ajustado (detalles en regr. múltiple)

sales1.mod$fitted.values ## proporciona el vector de valores ajustados

sales1.mod$df.residual ## proporciona los grados de libertad de los residuos (n-2)

sales1.mod$model ## proporciona los datos usados
sales1.mod$model[1,]
datos$Sales[1]
datos$TV[1]

confint(sales1.mod)  ## proporciona intervalos de confianza para los parámetros
confint(sales1.mod,level=0.99) 


############ GRAFICOS ############

plot(datos$TV,datos$Sales)
abline(sales1.mod)
abline(sales1.mod,lwd=3)
abline(sales1.mod,lwd=3,col="red")

plot(datos$TV,datos$Sales,col="red")

plot(datos$TV,datos$Sales,pch=20)

plot(datos$TV,datos$Sales,pch="+")

plot(1:20,1:20,pch=1:20)




############
par(mfrow=c(2,2))
plot(sales1.mod)
par(mfrow=c(1,1))

plot(predict(sales1.mod), residuals(sales1.mod))
plot(predict(sales1.mod), rstudent(sales1.mod))
plot(hatvalues(sales1.mod))
which.max(hatvalues(sales1.mod))


############ PREDICCIONES #######
##
## Permite predecir con el modelo ajustado sobre un conjunto de datos fijado,
## con un intervalo de confianza para la media o esperanza de la predicción 
## y con el denominado intervalo de predicción 
##
## Orden básica: 
##   predict(mod,subdatos, interval="confidence")
##       - mod es el modelo previamente ajustado con "lm"
##       - subdatos es el conjunto de datos de var. explicativas sobre el que se
##                  dese obtener las predicciones
##       - interval="confidence", o bien, interval="prediction"
##################################
                            
predict(sales1.mod,data.frame(TV=(c(0,50,100,150,200,250,300,350,400))), interval="confidence")
predict(sales1.mod,data.frame(TV=(c(0,50,100,150,200,250,300,350,400))), interval="prediction")


