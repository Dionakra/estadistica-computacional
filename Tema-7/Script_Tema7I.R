####################################################################
###    ESTADÍSTICA COMPUTACIONAL
###    TEMA 7. CLASIFICACIÓN Y DISCRIMINACION.
###    
###    GRADO EN INGENIERÍA INFORMÁTICA-INGENIERÍA DEL SOFTWARE
###    GRADO EN INGENIERÍA INFORMÁTICA- TECNOLOGÍAS INFORMÁTICAS
###    Juan M. Muñoz Pichardo - María Dolores Cubiles de la Vega
###    Departamento de Estadística e I.O. Universidad de Sevilla
####################################################################
###           PRIMER SCRIPT DEL DESARROLLO TEÓRICO DEL TEMA
###           ANÁLISIS DISCRIMINANTE LINEAL CLASICO BAJO CONDICIONES DE NORMALIDAD
###
###      Funciones: 
###             "lda"   
###             Estimación de tasas a través del método Holdout
####################################################################

### DIRECTORIO DE TRABAJO ####################################################

setwd("m:/TRABAJOPERSONAL/DOCENCIA/ECOMPUTACIONAL/Practicas")
library(graphics)
library(MASS)   # Cargamos la libreria que contiene a  lda

#### Datos salmon.dat   ############################################
## Estudio sobre los salmones en función de su origen
##
## Variables:
## 
## SalmonOrigin : Orogen del salmon (Canadá - Alaska)
## Freshwater   : diámetro (en 1/100 de pulgada) de los anillos sobre las escamas 
##                en el crecimiento del primer año en agua dulce 
## Marine       : diámetro (en 1/100 de pulgada) de los anillos sobre las escamas 
##                en el crecimiento del primer año en agua dulce 
#####################################################################


datos <- read.table("salmon.dat", header=TRUE)

datos
dim(datos)


plot(datos[,-1],col=as.factor(datos[,1]), pch=19)
legend("bottomleft",legend=levels(datos$SalmonOrigin),col=c(1,2),pch=c(19,19))


######## Análisis discriminante Lineal "lda" ###############
##
## Es necesario cargar la librería MASS
## library(MASS)   
##
## Orden
## lda(formula,datos,subset,...)
## 
## formula  : Indica la variable de agrupación y las Variables discriminantes que deben 
##            ser incluidas en el análisis. 
##            Ejemplo: vargrupo ~ x1 + x2 + x3
## datos    : data frame que contiene las variables indicadas.
## 
## Alternativamente, en lugar de "formula" se puede proporcionar el par:
##       datos
##       grouping : factor o variable grupo
##
## prior    : Probabilidades a priori. Si no se indican, se utilizan las proporciones
##            del conjunto de entrenamiento o proporciones muestrales.
## subset   : Un vector de índices especificando los casos que deben usarse en
##            la mustra de entrenamiento.
##            Ejemplo:
##            muestraentrenamiento <- sample(1:200,150)
##            lda(vargrupo ~ .,datos, subset=muestraentrenamiento)
## na.action : función para especificar la acción en caso de datos perdidos
##            Por defecto, el procedimiento falla y no se realiza.
##            Alternativa, na.omit, que conduce al rechazo de los casos con 
##            valores perdidos en cualquier variable deseada. 
## method    : "moment" para los estimadores estándar de la media y la varianza,
##            "mle"  para estimadores de máxima verosimilitud
##            "mve" para estimadors robustos de localización y dispersión (véase cov.mve)
##            "t" para estimaciones robustos basados en la distribución t
## CV        : Si "TRUE", incluye en los resultados las clases o gurpos asignados y
##             las probabilidades a posterior, a través del método de validación 
##             cruzada leave-one-out. 
## nu        : grados de libertad en caso de que se utilice el método "t"
##########################################################################################
## 
## REsultados:
##
## Si CV = TRUE, proporciona una lista con componentes: "class", clasificación realizada, y 
## "posterior", probabilidades a posteriori. 
## En otro caso, el objeto contiene:
##
## prior      : probabilidades a priori utilizados.
## means      : medias de los grupos
## scaling    : Matriz que transforma las observaciones a las funciones discriminantes,
##              normalizadas  de forma que la matriz de varianzas y covarianzas de los 
##              grupos sea esférica.
## svd        : valores singulares, que proporcionan la razón de las desviaciones
##              estandar entre los grupos y dentro de las variables discriminantes
##             lineales. 
## N          : número de observaciones utilizadas
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



#### Determinación de la recta de separación
medsemi=0.5*(aprdat.lda$means[1,]+aprdat.lda$means[2,])
medsemi #punto medio del segmento formado por los puntos medios de los grupos

pend.rect=-aprdat.lda$scaling[1]/aprdat.lda$scaling[2]
intercept.rect=-pend.rect*medsemi[1]+medsemi[2]


plot(datos[,-1],col=as.factor(datos[,1]), pch=19)
legend("bottomleft",legend=levels(datos$SalmonOrigin),col=c(1,2),pch=c(19,19))
abline(a=intercept.rect,b=pend.rect,col="blue")
###############################################


###### Clasificación y Estimación de tasas de error

## Clasificación sobre el conjunto test
testpred <- predict(aprdat.lda, testdat[,c(2,3)])
testpred

# Comprobación de la puntuación obtenida para cada caso test
testpred$x[1]  # Transformación del caso "1" al espacio discriminante

testdat[1,]  # Datos originales del caso "1"
mapr1 <- mean(aprdat$Freshwater)  # media global primera variable
mapr2 <- mean(aprdat$Marine)      # media global segunda variable

aprdat.lda$scaling[1]*(testdat[1,2]-mapr1)+aprdat.lda$scaling[2]*(testdat[1,3]-mapr2)
# Obsérvese que coincide con "testpred$x[1]"


## Tabla de clasificacion correctos/incorrectos
ct <- table(testdat$SalmonOrigin, testpred$class)
ct
# Porcentaje correcto por grupos
diag(prop.table(ct, 1))
# total porcentaje correcto
sum(diag(prop.table(ct)))

###### Clasificación sobre un caso nuevo

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
#### Visualización de algunos resultados

plot(aprdat.lda) # Histogramas de las muestras iniciales transformadas

library(klaR)
partimat(SalmonOrigin~Freshwater+Marine,data=aprdat,method="lda",
         main="Plot partición del espacio muestral. Datos de aprendizaje") 
partimat(SalmonOrigin~Freshwater+Marine,data=datos,method="lda",
         main="Plot partición del espacio muestral. Datos completos") 
