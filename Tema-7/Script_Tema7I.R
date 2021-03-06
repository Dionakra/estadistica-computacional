####################################################################
###    ESTAD�STICA COMPUTACIONAL
###    TEMA 7. CLASIFICACI�N Y DISCRIMINACION.
###    
###    GRADO EN INGENIER�A INFORM�TICA-INGENIER�A DEL SOFTWARE
###    GRADO EN INGENIER�A INFORM�TICA- TECNOLOG�AS INFORM�TICAS
###    Juan M. Mu�oz Pichardo - Mar�a Dolores Cubiles de la Vega
###    Departamento de Estad�stica e I.O. Universidad de Sevilla
####################################################################
###           PRIMER SCRIPT DEL DESARROLLO TE�RICO DEL TEMA
###           AN�LISIS DISCRIMINANTE LINEAL CLASICO BAJO CONDICIONES DE NORMALIDAD
###
###      Funciones: 
###             "lda"   
###             Estimaci�n de tasas a trav�s del m�todo Holdout
####################################################################

### DIRECTORIO DE TRABAJO ####################################################

setwd("m:/TRABAJOPERSONAL/DOCENCIA/ECOMPUTACIONAL/Practicas")
library(graphics)
library(MASS)   # Cargamos la libreria que contiene a  lda

#### Datos salmon.dat   ############################################
## Estudio sobre los salmones en funci�n de su origen
##
## Variables:
## 
## SalmonOrigin : Orogen del salmon (Canad� - Alaska)
## Freshwater   : di�metro (en 1/100 de pulgada) de los anillos sobre las escamas 
##                en el crecimiento del primer a�o en agua dulce 
## Marine       : di�metro (en 1/100 de pulgada) de los anillos sobre las escamas 
##                en el crecimiento del primer a�o en agua dulce 
#####################################################################


datos <- read.table("salmon.dat", header=TRUE)

datos
dim(datos)


plot(datos[,-1],col=as.factor(datos[,1]), pch=19)
legend("bottomleft",legend=levels(datos$SalmonOrigin),col=c(1,2),pch=c(19,19))


######## An�lisis discriminante Lineal "lda" ###############
##
## Es necesario cargar la librer�a MASS
## library(MASS)   
##
## Orden
## lda(formula,datos,subset,...)
## 
## formula  : Indica la variable de agrupaci�n y las Variables discriminantes que deben 
##            ser incluidas en el an�lisis. 
##            Ejemplo: vargrupo ~ x1 + x2 + x3
## datos    : data frame que contiene las variables indicadas.
## 
## Alternativamente, en lugar de "formula" se puede proporcionar el par:
##       datos
##       grouping : factor o variable grupo
##
## prior    : Probabilidades a priori. Si no se indican, se utilizan las proporciones
##            del conjunto de entrenamiento o proporciones muestrales.
## subset   : Un vector de �ndices especificando los casos que deben usarse en
##            la mustra de entrenamiento.
##            Ejemplo:
##            muestraentrenamiento <- sample(1:200,150)
##            lda(vargrupo ~ .,datos, subset=muestraentrenamiento)
## na.action : funci�n para especificar la acci�n en caso de datos perdidos
##            Por defecto, el procedimiento falla y no se realiza.
##            Alternativa, na.omit, que conduce al rechazo de los casos con 
##            valores perdidos en cualquier variable deseada. 
## method    : "moment" para los estimadores est�ndar de la media y la varianza,
##            "mle"  para estimadores de m�xima verosimilitud
##            "mve" para estimadors robustos de localizaci�n y dispersi�n (v�ase cov.mve)
##            "t" para estimaciones robustos basados en la distribuci�n t
## CV        : Si "TRUE", incluye en los resultados las clases o gurpos asignados y
##             las probabilidades a posterior, a trav�s del m�todo de validaci�n 
##             cruzada leave-one-out. 
## nu        : grados de libertad en caso de que se utilice el m�todo "t"
##########################################################################################
## 
## REsultados:
##
## Si CV = TRUE, proporciona una lista con componentes: "class", clasificaci�n realizada, y 
## "posterior", probabilidades a posteriori. 
## En otro caso, el objeto contiene:
##
## prior      : probabilidades a priori utilizados.
## means      : medias de los grupos
## scaling    : Matriz que transforma las observaciones a las funciones discriminantes,
##              normalizadas  de forma que la matriz de varianzas y covarianzas de los 
##              grupos sea esf�rica.
## svd        : valores singulares, que proporcionan la raz�n de las desviaciones
##              estandar entre los grupos y dentro de las variables discriminantes
##             lineales. 
## N          : n�mero de observaciones utilizadas
##########################################################################################



aprdat=datos[c(1:40,51:90),]   # conjunto de "entrenamiento" o "aprendizaje"
testdat=datos[c(41:50,91:100),]  # conjunto "test"


aprdat.lda=lda(aprdat[,c(2,3)],grouping=aprdat[,1])
aprdat.lda
attributes(aprdat.lda)
aprdat.lda$prior
aprdat.lda$counts
aprdat.lda$means
aprdat.lda$scaling
aprdat.lda$lev
aprdat.lda$svd
aprdat.lda$N



#### Determinaci�n de la recta de separaci�n
medsemi=0.5*(aprdat.lda$means[1,]+aprdat.lda$means[2,])
medsemi #punto medio del segmento formado por los puntos medios de los grupos

pend.rect=-aprdat.lda$scaling[1]/aprdat.lda$scaling[2]
intercept.rect=-pend.rect*medsemi[1]+medsemi[2]


plot(datos[,-1],col=as.factor(datos[,1]), pch=19)
legend("bottomleft",legend=levels(datos$SalmonOrigin),col=c(1,2),pch=c(19,19))
abline(a=intercept.rect,b=pend.rect,col="blue")
###############################################


###### Clasificaci�n y Estimaci�n de tasas de error

## Clasificaci�n sobre el conjunto test
testpred <- predict(aprdat.lda, testdat[,c(2,3)])
testpred

# Comprobaci�n de la puntuaci�n obtenida para cada caso test
testpred$x[1]  # Transformaci�n del caso "1" al espacio discriminante

testdat[1,]  # Datos originales del caso "1"
mapr1 <- mean(aprdat$Freshwater)  # media global primera variable
mapr2 <- mean(aprdat$Marine)      # media global segunda variable

aprdat.lda$scaling[1]*(testdat[1,2]-mapr1)+aprdat.lda$scaling[2]*(testdat[1,3]-mapr2)
# Obs�rvese que coincide con "testpred$x[1]"


## Tabla de clasificacion correctos/incorrectos
ct <- table(testdat$SalmonOrigin, testpred$class)
ct
# Porcentaje correcto por grupos
diag(prop.table(ct, 1))
# total porcentaje correcto
sum(diag(prop.table(ct)))

###### Clasificaci�n sobre un caso nuevo

predict(aprdat.lda,c(120,380))  # Caso con Freshwater=120,Marine=380
attributes(predict(aprdat.lda,c(120,380)))
predict(aprdat.lda,c(120,380))$x
predict(aprdat.lda,c(120,380))$class
predict(aprdat.lda,c(120,380))$posterior

predict(aprdat.lda,c(100,475))  # Caso con Freshwater=100,Marine=475
attributes(predict(aprdat.lda,c(100,475)))
predict(aprdat.lda,c(100,475))$x
predict(aprdat.lda,c(100,475))$class
predict(aprdat.lda,c(100,475))$posterior


######################################################################
#### Visualizaci�n de algunos resultados

plot(aprdat.lda) # Histogramas de las muestras iniciales transformadas

library(klaR)
partimat(SalmonOrigin~Freshwater+Marine,data=aprdat,method="lda",
         main="Plot partici�n del espacio muestral. Datos de aprendizaje") 
partimat(SalmonOrigin~Freshwater+Marine,data=datos,method="lda",
         main="Plot partici�n del espacio muestral. Datos completos") 
