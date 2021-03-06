####################################################################
###    ESTAD�STICA COMPUTACIONAL
###    TEMA 7. CLASIFICACI�N Y DISCRIMINACION.
###    
###    GRADO EN INGENIER�A INFORM�TICA-INGENIER�A DEL SOFTWARE
###    GRADO EN INGENIER�A INFORM�TICA- TECNOLOG�AS INFORM�TICAS
###    Juan M. Mu�oz Pichardo - Mar�a Dolores Cubiles de la Vega
###    Departamento de Estad�stica e I.O. Universidad de Sevilla
####################################################################
###           SEGUNDO SCRIPT DEL DESARROLLO TE�RICO DEL TEMA
###           AN�LISIS DISCRIMINANTE LINEAL CLASICO BAJO CONDICIONES DE NORMALIDAD
###           ESTIMACI�N DE TASAS POR VALIDACI�N CRUZADA
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

datos.lda=lda(datos[,c(2,3)],grouping=datos[,1])
datos.lda
attributes(datos.lda)
datos.lda$prior
datos.lda$counts
datos.lda$means
datos.lda$scaling
datos.lda$lev
datos.lda$svd
datos.lda$N


datos.ldaCV=lda(datos[,c(2,3)],grouping=datos[,1],CV=TRUE)
attributes(datos.ldaCV)

datos.ldaCV$class
datos.ldaCV$posterior



#### Determinaci�n de la recta de separaci�n
medsemi=0.5*(datos.lda$means[1,]+datos.lda$means[2,])
medsemi #punto medio del segmento formado por los puntos medios de los grupos

pend.rect=-datos.lda$scaling[1]/datos.lda$scaling[2]
intercept.rect=-pend.rect*medsemi[1]+medsemi[2]


plot(datos[,-1],col=as.factor(datos[,1]), pch=19)
legend("bottomleft",legend=levels(datos$SalmonOrigin),col=c(1,2),pch=c(19,19))
abline(a=intercept.rect,b=pend.rect,col="blue")
###############################################


###### Clasificaci�n y Estimaci�n de tasas de error


## Tabla de clasificacion correctos/incorrectos
ct <- table(datos$SalmonOrigin, datos.ldaCV$class)
ct
# Porcentaje correcto por grupos
diag(prop.table(ct, 1))
# total porcentaje correcto
sum(diag(prop.table(ct)))

###### Clasificaci�n sobre un caso nuevo

predict(datos.lda,c(120,380))  # Caso con Freshwater=120,Marine=380
attributes(predict(datos.lda,c(120,380)))
predict(datos.lda,c(120,380))$x
predict(datos.lda,c(120,380))$class
predict(datos.lda,c(120,380))$posterior

predict(datos.lda,c(100,475))  # Caso con Freshwater=100,Marine=475
attributes(predict(datos.lda,c(100,475)))
predict(datos.lda,c(100,475))$x
predict(datos.lda,c(100,475))$class
predict(datos.lda,c(100,475))$posterior


######################################################################
#### Visualizaci�n de algunos resultados

plot(datos.lda) # Histogramas de las muestras iniciales transformadas

library(klaR)

partimat(SalmonOrigin~Freshwater+Marine,data=datos,method="lda",
         main="Plot partici�n del espacio muestral. Datos completos") 
