####################################################################
###    ESTAD?STICA COMPUTACIONAL
###    TEMA 7. CLASIFICACI?N Y DISCRIMINACION.
###    
###    GRADO EN INGENIER?A INFORM?TICA-INGENIER?A DEL SOFTWARE
###    GRADO EN INGENIER?A INFORM?TICA- TECNOLOG?AS INFORM?TICAS
###    Juan M. Mu?oz Pichardo - Mar?a Dolores Cubiles de la Vega
###    Departamento de Estad?stica e I.O. Universidad de Sevilla
####################################################################
###           SEGUNDO SCRIPT DEL DESARROLLO TE?RICO DEL TEMA
###           AN?LISIS DISCRIMINANTE LINEAL CLASICO BAJO CONDICIONES DE NORMALIDAD
###           ESTIMACI?N DE TASAS POR VALIDACI?N CRUZADA
###      Funciones: 
###             "lda"   
###             Estimaci?n de tasas a trav?s del m?todo Holdout
####################################################################

### DIRECTORIO DE TRABAJO ####################################################

library(graphics)
library(MASS)   # Cargamos la libreria que contiene a  lda

#### Datos "enzyme.dat  ###############################################
## The data consists of 218 patients with liver diseases (Plomteux, 1980)
## Four diseases are considered: 
##       acute viral hepatitis (AVH) (n1 = 57 patients), 
##       persistent chronic hepatitis (PCH) (n2 = 44), 
##       aggressive chronic hepatitis (ACH) (n3 = 40) and
##       post-necrotic cirrhosis (PNC) (n4 = 77). 
## The diagnosis of AVH was carried out by biological and clinical signs. 
## PCH, ACH and PNC were diagnosed by laparoscopy and biopsy. 
## 
## For convenience, the cases corresponding to each group are labeled by 1 to 57,
##  58 to 101, 102 to 141, and 142 to 218, respectively.
## The aim of this study is to obtain a differential diagnosis of the four liver diseases
## considered by means of an enzyme profile.
## The discrimination between the four diseases could be achieved on the basis of three 
## liver function tests: 
##      aspartate aminotransferase (ASP),
##      alanine aminotransferase (ALA), and 
##      glutamate dehydrogenase (GLU) 
## (all expressed in international units per litre). 
## The observed variables have been transformed, X = ln Y; to verify the normality assumption.
##
#####################################################################


datos <- read.table("enzyme.dat", header=TRUE)

names(datos)
dim(datos)
datos[1:4,]


plot(datos[,-4],col=as.factor(datos[,4]), pch=19)
plot(datos$ASP,datos$ALA,col=as.factor(datos[,4]), pch=19)
legend("bottomright",legend=levels(datos$DIS),col=c(1,2,3,4),pch=c(19,19,19,19))

plot(datos$ASP,datos$GLU,col=as.factor(datos[,4]), pch=19)
legend("bottomright",legend=levels(datos$DIS),col=c(1,2,3,4),pch=c(19,19,19,19))

plot(datos$ALA,datos$GLU,col=as.factor(datos[,4]), pch=19)
legend("bottomright",legend=levels(datos$DIS),col=c(1,2,3,4),pch=c(19,19,19,19))


######## An?lisis discriminante Lineal "lda" ###############
##
## Es necesario cargar la librer?a MASS
## library(MASS)   
##
## Orden
## lda(formula,datos,subset,...)
## 
## formula  : Indica la variable de agrupaci?n y las Variables discriminantes que deben 
##            ser incluidas en el an?lisis. 
##            Ejemplo: vargrupo ~ x1 + x2 + x3
## datos    : data frame que contiene las variables indicadas.
## 
## Alternativamente, en lugar de "formula" se puede proporcionar el par:
##       datos
##       grouping : factor o variable grupo
##
## prior    : Probabilidades a priori. Si no se indican, se utilizan las proporciones
##            del conjunto de entrenamiento o proporciones muestrales.
## subset   : Un vector de ?ndices especificando los casos que deben usarse en
##            la mustra de entrenamiento.
##            Ejemplo:
##            muestraentrenamiento <- sample(1:200,150)
##            lda(vargrupo ~ .,datos, subset=muestraentrenamiento)
## na.action : funci?n para especificar la acci?n en caso de datos perdidos
##            Por defecto, el procedimiento falla y no se realiza.
##            Alternativa, na.omit, que conduce al rechazo de los casos con 
##            valores perdidos en cualquier variable deseada. 
## method    : "moment" para los estimadores est?ndar de la media y la varianza,
##            "mle"  para estimadores de m?xima verosimilitud
##            "mve" para estimadors robustos de localizaci?n y dispersi?n (v?ase cov.mve)
##            "t" para estimaciones robustos basados en la distribuci?n t
## CV        : Si "TRUE", incluye en los resultados las clases o gurpos asignados y
##             las probabilidades a posterior, a trav?s del m?todo de validaci?n 
##             cruzada leave-one-out. 
## nu        : grados de libertad en caso de que se utilice el m?todo "t"
##########################################################################################
## 
## REsultados:
##
## Si CV = TRUE, proporciona una lista con componentes: "class", clasificaci?n realizada, y 
## "posterior", probabilidades a posteriori. 
## En otro caso, el objeto contiene:
##
## prior      : probabilidades a priori utilizados.
## means      : medias de los grupos
## scaling    : Matriz que transforma las observaciones a las funciones discriminantes,
##              normalizadas  de forma que la matriz de varianzas y covarianzas de los 
##              grupos sea esf?rica.
## svd        : valores singulares, que proporcionan la raz?n de las desviaciones
##              estandar entre los grupos y dentro de las variables discriminantes
##             lineales. 
## N          : n?mero de observaciones utilizadas
##########################################################################################

datos.lda=lda(datos[,c(1,2,3)],grouping=datos[,4])
datos.lda
attributes(datos.lda)
datos.lda$prior
datos.lda$counts
datos.lda$means
datos.lda$scaling
datos.lda$lev
datos.lda$svd
datos.lda$N


datos.ldaCV=lda(datos[,c(1,2,3)],grouping=datos[,4],CV=TRUE)
attributes(datos.ldaCV)

datos.ldaCV$class
datos.ldaCV$posterior



###### Clasificaci?n y Estimaci?n de tasas de error


## Tabla de clasificacion correctos/incorrectos
ct <- table(datos$DIS, datos.ldaCV$class)
ct
# Porcentaje correcto por grupos
diag(prop.table(ct, 1))
# total porcentaje correcto
sum(diag(prop.table(ct)))

###### Clasificaci?n sobre un caso nuevo
names(datos)
predict(datos.lda,c(4.1,4.2,4.3))  # Caso con "ASP=4.1" "ALA=4.2" "GLU=4.3" 
attributes(predict(datos.lda,c(4.1,4.2,4.3)))
predict(datos.lda,c(4.1,4.2,4.3))$x
predict(datos.lda,c(4.1,4.2,4.3))$class
predict(datos.lda,c(4.1,4.2,4.3))$posterior

predict(datos.lda,c(6.1,5.2,3))  # Caso con "ASP=6.1" "ALA=5.2" "GLU=3" 
attributes(predict(datos.lda,c(6.1,5.2,3)))
predict(datos.lda,c(6.1,5.2,3))$x
predict(datos.lda,c(6.1,5.2,3))$class
predict(datos.lda,c(6.1,5.2,3))$posterior


######################################################################
#### Visualizaci?n de algunos resultados


datos.pred <- predict(datos.lda,datos[,1:3])
names(datos.pred)
plot(datos.pred$x[,1],datos.pred$x[,2],col=as.factor(datos$DIS),xlab="LD1",ylab="LD2")
legend("bottomright",legend=levels(datos$DIS),col=c(1,2,3,4),pch=c(19,19,19,19))

plot(datos.pred$x[,1],datos.pred$x[,3],col=as.factor(datos$DIS),xlab="LD1",ylab="LD3")
legend("bottomright",legend=levels(datos$DIS),col=c(1,2,3,4),pch=c(19,19,19,19))

plot(datos.pred$x[,2],datos.pred$x[,3],col=as.factor(datos$DIS),xlab="LD2",ylab="LD3")
legend("bottomright",legend=levels(datos$DIS),col=c(1,2,3,4),pch=c(19,19,19,19))


library(klaR)

partimat(DIS~ASP+ALA+GLU,data=datos,method="lda",
         main="Plot partici?n del espacio muestral. Datos completos") 
