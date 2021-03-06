####################################################################
###    ESTAD�STICA COMPUTACIONAL
###    TEMA 5. T�CNICAS DE REDUCCI�N DE LA DIMENSI�N.
###    
###    GRADO EN INGENIER�A INFORM�TICA-INGENIER�A DEL SOFTWARE
###    GRADO EN INGENIER�A INFORM�TICA- TECNOLOG�AS INFORM�TICAS
###    Juan M. Mu�oz Pichardo - Mar�a Dolores Cubiles de la Vega
###    Departamento de Estad�stica e I.O. Universidad de Sevilla
####################################################################
###   PRIMER SCRIPT DEL DESARROLLO TE�RICO DEL TEMA
###           AN�LISIS DE COMPONENTES PRINCIPALES
####################################################################

### DIRECTORIO DE TRABAJO ####################################################

setwd("L:/TRABAJOPERSONAL/DOCENCIA/ECOMPUTACIONAL/Practicas")


### LECTURA FICHERO DE DATOS ######################################
##         epf1999.xls
## Los datos corresponden a la Encuesta de Presupuestos Familiares (1990/91)
## en Espa�a (fichero epf1999.csv) y presentan los gastos medios de las 
## familias espa�olas para las 51 provincias (Ceuta y Melilla aparecen 
## unidas como una provincia) en nueve ep�grafes:
##   X1 =alimentaci�n,         X2 =vestido y calzado,    X3 =vivienda,
##   X4 =mobiliario dom�stico, X5 =gastos sanitarios,    X6 =transportes,
##   X7 =ense�anza y cultura,  X8 =turismo y ocio,       X9 =otros gastos.
##
## Est�n extra�dos del libro
##Pe�a, D. (2002). An�lisis de datos multivariantes. McGraw-Hill.
## Y se pueden obtener en la web:
##   http://www.mhe.es/universidad/ciencias_matematicas/pena/ficheros.html
##
## La matriz de covarianzas resume la variabilidad de estas 9 variables 
## en los 51 elementos observados. 
## Como las distribuciones de los gastos son muy asim�tricas, las variables 
## se han expresado en logaritmos.
###################################################################


datos <- read.table("epf1999.csv", header=TRUE, sep=";",row.names="Provincia")

names(datos)
datos
class(datos)

##################################################################################
##                               AN�LISIS DE COMPONENTES PRINCIPALES (ACP)
##
##  
##
##  Orden b�sica: princomp(...)
##
##    princomp(formula, data = NULL, subset, ...)
##    princomp(x, cor = FALSE, scores = TRUE, covmat = NULL, subset = rep(TRUE, nrow(as.matrix(x))), ...)
##
##    ARGUMENTOS:
##   
##  formula:  formula sin variable respuesta, s�lo con variables num�ricas
##            Por ejemplo:  ~ varX1 + varX2 + varX3 + varX4
##
##  data  : un marco de datos opcional que contenga las variables de la f�rmula
##
##  subset : un vestor opcional para seleccionar las filas (observaciones) de la matriz de datos
##
##  x  : matriz o marco de datos que proporciona los datos que proporciona los datos para el ACP
##
##  cor: Valor l�gico (T � F) indicando si se usa la matriz de correlaci�n (T) o la matriz de covarianzas (F).
##       
## scores : valor l�gico (T/F) indicando si las puntuaciones de cada c.p. deben ser calculadas  
##
## 
##  Crea un objeto "princomp" que recoge la siguiente informaci�n 
##   sdev : desviaciones est�ndar de las comp.principales.
##   loadings : matriz de cargas (es decir, matriz de autovectores)
##   center : las medias 
##   scaling : la escala aplicada a cada variable
##   n.obs   : n�mero de observaciones 
##   scores  : Si se ha solicitado, las puntuaciones de los datos en las c.p.
##################################################################################

# Resumen, media y desviaci�n t�pica.
summary(datos)
colMeans(datos)
apply(datos,2,mean)
apply(datos,2,sd)

# Matriz de covarianzas y matriz de correlaci�n
Sigmaestim <- round(cov(datos),3)
Sigmaestim
Restim<-round(cor(datos),3)
Restim


#An�lisis de Componenetes Principales sobre la matriz de Covarianzas
cp<-princomp(~.,data=data.frame(datos),cor=FALSE,scores=T)
cp
names(cp)

# Desviacion t�pica, porcentaje de varianza explicativa
summary(cp)
cp$sdev

#Autovalores (recu�rdese que coinciden con la varianza de cada cp)
eigen(Sigmaestim)$values
cp$sdev[1]^2 #Coincide con el primer autovalor

#Representaci�n gr�fica de la varianza explicada
plot(cp)

# Autovectores o matriz de cargas
cp$loadings
eigen(Sigmaestim)$vectors

# Puntuaciones de las cp en cada uno de los casos
cp$scores
predict(cp)


# Correlaciones entre las variables originales y las C.P.
C=cor(datos,predict(cp))
C

# Centros o Medias muestrales de las variables originales
cp$center
colMeans(datos)

#Escalas (en este caso 1) y desviaciones t�picas de las variables originales
cp$scale # (divide por n numero de casos)
apply(datos,2,sd) #(divide por (n-1) )


summary(cp)

##########################################################################
# Suponiendo que se consideran s�lo las 2 primeras componentes principales
##########################################################################
# Variabilidad explicada conjuntamente 80,95%
# Variabilidad explicada por CP1: 74,12%
# Variabilidad explicada por CP2:  6,83%

#Matriz de correlaciones de cp con variables originales
C=cor(datos,predict(cp)[,1:2])
C




#Puntuaciones
cp$scores[,1:2]
plot(datos)


plot(cp$score[,1],cp$score[,2],pch=16,xlab="Primera C.P.",ylab="Segunda C.P.")
text( cp$score[,1],cp$score[,2], row.names(datos), cex=0.7, pos=3, col="blue")
grid()
abline(h=0,v=0)




plot(C[,1],C[,2],pch=16,xlab="Primera C.P.",ylab="Segunda C.P.",xlim=c(-1.0,1.0),ylim=c(-1,1))
text( C[,1],C[,2], row.names(C), cex=0.7, pos=3, col="blue")
grid()
abline(h=0,v=0)

