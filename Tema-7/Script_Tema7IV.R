####################################################################
###    ESTADÍSTICA COMPUTACIONAL
###    TEMA 7. CLASIFICACIÓN Y DISCRIMINACION.
###    
###    GRADO EN INGENIERÍA INFORMÁTICA-INGENIERÍA DEL SOFTWARE
###    GRADO EN INGENIERÍA INFORMÁTICA- TECNOLOGÍAS INFORMÁTICAS
###    Juan M. Muñoz Pichardo - María Dolores Cubiles de la Vega
###    Departamento de Estadística e I.O. Universidad de Sevilla
####################################################################
###           CUARTO SCRIPT DEL DESARROLLO TEÓRICO DEL TEMA
###           AARBOLES DE CLASIFICACIÓN          
###                    Conjunto de datos: spam.txt   
####################################################################

### DIRECTORIO DE TRABAJO ####################################################

setwd("m:/TRABAJOPERSONAL/DOCENCIA/ECOMPUTACIONAL/Practicas")
library(rpart) 
library(graphics)

###  LECTURA DE DATOS  #######################################################

### cONJUNTO DE DATOS: spam.txt
##   Variables predictoras:
##     crl.total  : Longitud total de las palabras en mayúsculas
##     dollar     : frecuencia del símbolo $, como % de todos los caracteres
##     bang       : idem para el símbolo !
##     money      : frecuencia de la palabra "money" como % de todas las palabras
##     n000       : frecuencia de la tira "000", % de todos los caracteres
##     make       : frecuencia de la palabra "make", como % de todas las palabras
##   Variable respuesta: 
##     yesno      : "n" no spam, "y" sí spam.
####################

spam <- read.table("spam.txt", header=TRUE)
names(spam)
summary(spam)
str(spam) 

dim(spam)
spam[1:4,]
####################


##### Para clasificación, Orden rpart( ... , method="class") 

################################################################################# 
##
##   orden: rpart(formula, data, weights, subset, na.action = na.rpart, method,
##                model = FALSE, x = FALSE, y = TRUE, parms, control, cost, ...)
##
##   Para clasificación, Orden rpart( ... , method="class") 
##
##   Argumentos:
##     formula : Una fórmula , con una respuesta , pero sin términos de interacción . 
##              Si es un data.frame se toma como marco de modelo ( ver model.frame ).
##     data    : Marco de datos opcional 
##     weights : pesos opcionales para los casos.
##     subset  : expresión opcional para indicar que sólo un subconjunto de las filas 
##               de los datos se debe utilizar en el ajuste.
##     na.action : la acción predeterminada elimina todas las observaciones que no tienen 
##                 (missing) la variable objetivo y, pero mantiene aquellos en los
##                 que uno o más predictores faltan.
##     method : una de las opciones: "anova", "poisson" , "class" o "exp". 
##              Si method no se indica, la rutina intenta hacer un supuesto inteligente . 
##              - Si y es un objeto de supervivencia, entonces se asume method ="exp" , 
##              - si y tiene 2 columnas, se supone method = "poisson" , 
##              - si y es un factor entonces method = "class", 
##              - en caso contrario method = "anova" 
##              Es mejor especificar el método directamente , sobre todo a medida que 
##              más criterios pueden añadirse a la función en el futuro. 
##              Alternativamente, el método puede ser una lista de funciones denominadas 
##                  "init" , "split" y "eval".
##     model  : (lógica) guardar una copia del marco modelo en el resultado? 
##              Si el valor de entrada para el modelo es un marco (probablemente de una llamada 
##              anterior a la función rpart) modelo , entonces este marco se utiliza en lugar 
##              de construir nuevos datos.
##     x      : conservar una copia de la matriz x en el resultado.
##     y      : conservar una copia de la variable dependiente en el resultado. 
##     parms  : parámetros opcionales para el procedimiento de división.
##              El procedimiento "anova" no tiene parámetros.
##              El procedimiento "poisson" tiene un solo parámetro, el coeficiente de variación 
##              de la distribución a priori sobre las tarifas. El valor por defecto es 1.
##              El procedimiento "exp" tiene el mismo parámetro que de Poisson.
##              Para clasificación, la lista puede contener cualquiera de: 
##                   - vector de probabilidades previas ("prior", deben ser positivas y sumar 1, 
##                                por defecto proporcionales al número de casos ), 
##                   - la matriz de la pérdida ("loss", debe tener ceros en los elementos de la 
##                                diagonal y positivos fuera de la diagonal, por defecto 1)
##                   - el índice de división ("split", puede ser gini o information, 
##                                por defecto gini)
##    control : lista de opciones que controlan el algoritmo rpart. Véase rpart.control.
##    cost    : un vector de costos no negativos, uno para cada variable en el modelo. 
##              El valor predeterminado es 1 para todas las variables. 
##            Son escalas que se aplicarán cuando se consideren las divisiones, por lo que la 
##            mejora con respecto a la división de una variable se divide por su costo en la 
##            decisión de qué división elegir.
####################################################################################
## Orden: rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01,
##                      maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
##                      surrogatestyle = 0, maxdepth = 30, ...)
##
##    minsplit : número mínimo de casos que debe tener un nodo para aplicarle una división.
##   minbucket : número mínimo de casos en cualquier nodo terminal. 
##               Salvo que se especifique los dos parámetros: minsplit= 3*minbucket.  
##          cp : parámetro de complejidad.Cualquier división que no disminuya la perdida global
##               de ajuste en un factor cp no se intenta realizar. 
##               Por ejemplo, con división ANOVA, esto significa que el general R-cuadrado 
##               debe aumentar, al menos, cp a cada paso. Recuérdese que 
##                    R-cuadrado = SC(dentro de los grupos) /SC
##  maxcompete : número de divisiones "competidoras" que proporciona los resultados. Es útil saber, 
##               además de la división elegida, la variable que quedó en segundo lugar, tercero, etc.
##               En el objeto de salida aparece en la tabla "Primary splits" de cada nodo
## maxsurrogate: número de divisiones "sustitutas" que proporciona los resultados. 
##               En el objeto de salida aparece en la tabla "Surrogate splits"
##        xval : número de grupos en la validación cruzada
####################################################################################

spam.rpart <- rpart(yesno ~ crl.tot + dollar + bang + money + n000 + make, 
                    data=spam,method="class")

spam.rpart
# PROPORCIONA datos básicos del arbol inicial obtenido (sin proceso de podas,....)  
# node  : NÚMERO DE NODO
# split : CRITERIO DIVISIÓN
# n     : NÚM.CASOS EN EL NODO 
# loss  : ERRORES EN EL NODO FORMADO, TRAS ASIGNAR A LA MODALIDAD MÁS FRECUENTE)
# yval  : VALOR ASIGNADO PARA LA VARIABLE RESPUESTA: MODALIDAD MÁS FRECUENTE)
# (yprob) : PROBABILIDADES O PROPORCIONES DE LAS MODALIDADES EN EL NODO

names(spam.rpart)

summary(spam.rpart)
# PROPORCIONA datos básicos del arbol inicial obtenido (sin proceso de podas,....)  
# n: número de casos
#
# Una tabla con los valores CP/nsplit/rel error/xerror/xstd
# CP : En el paso 1 (división 0), el índice de Gini asociado al nodo raíz.
#      En el paso 2 (1ª división) y siguientes: la disminución de la función de 
#      impureza (Indice de Gini) después de la correspondiente división.
# nsplit : identificación de la división realizada
# rel error: 1 menos la reducción del error relativo después de cada división.
#            Si se denota la probabilidad de error de clasificación en la 
#            división d (d=0,1,2,...) por PEC(d), entonces es
#               1 - [PEC(d-1)-PED(d)]/PEC(d-1).
#           Así, RelEr x 100% representa el porcentaje de disminución del error
#           de clasificación respecto del paso anterior, es decir, provocado por
#           esta división.
# xerror : Idéntico al anterior pero midiendo la probabilidad de error de 
#          clasificación a través de validación cruzada
# xstd   : error estándar asociado a xerror (a través de validación cruzada)
#          es decir, el valor inidicado para la regla 1-SE.
#
## Relevancia de cada variable en el árbol (%)
# 
# Información para cada nodo: 
#  Número de casos, CP, predicción, error esperado (expected loss), 
#  probabilidad del nodo P(node), número de casos en cada clase y probabilidades
#  asociadas e información sobre divisiones competidoras y divisiones sustitutas
########################################

spam.rpart$cptable #tabla con los valores CP/nsplit/rel error/xerror/xstd
spam.rpart$parms #probabilidades a priori, costes
spam.rpart$control # parámetros  de control del algoritmo
spam.rpart$variable.importance # medida de la importancia relativa de cada variable en las divisiones


################################################################################
# Representación gráfica del árbol de clasificación
################################################################################

plot(spam.rpart,main="CART datos spam",margin=0.01,compress=TRUE)
text(spam.rpart,col="blue")

######## Orden plotcp(objeto_rpart, minline = TRUE, lty = 3, col = 1,
##                    upper = c("size", "splits", "none"), ...))
## Proporciona una representación visual de los resultados de la 
## validación cruzada en un objeto rpart.
##
##      minline : (Lógica; por defecto TRUE). Dibuja una línea horizontal 
##                 1SE por encima del mínimo de la curva). 
##               lty: tipo de lílea 
##               col: color de línea
##                 
##      upper : lo que se traza en el eje superior, es decir, el tamaño 
##                del árbol (número de hojas), el número de divisiones o nada.
## El conjunto de posibles podas basadas en costos de complejidad (cp) de un árbol 
## a partir de un conjunto anidado. 
##
## La "cptable" del ajuste contiene, además de la tabla anteriormente especificada 
## con los valores CP, "rel error", "xerror" y "xstd", indica:
## las variables utilizadas en la construcción del árbol y la estimación de la 
## probabilidad de error del nodo raíz.
## 
## Una buena elección de cp para la poda es a menudo el valor más a la izquierda 
## para los que la media está por debajo de la línea horizontal.
##
## Orden  princp(objeto_rpart) 
## Proporciona "cptable", es decir, una tabla de "podas óptimas" 
## basados en el parámetro de la complejidad. 
#########

plotcp(spam.rpart,col=2)
plotcp(spam.rpart,upper = c("splits"),lty = 10,col=3)

printcp(spam.rpart)

########################################
#matriz de confusión
confu<-table(spam$yesno,predict(spam.rpart,type="class"),deparse.level = 2)
confu
# Porcentaje correcto por grupos
prop.table(confu, 1)
diag(prop.table(confu, 1))

#porcentaje de acierto
tacier<- 100*(confu[1,1]+confu[2,2])/sum(confu)
tacier
# otra forma
100*sum(diag(prop.table(confu)))

# Nota: (Orden "cat" permite concatenar objetos o representaciones)
# ESPECIFICIDAD Y SENSIBILIDAD
# La tabla anterior, 2x2, puede representarse por:
#                -- Predicción --
#  Realidad       N            Y
#     N        Verd.Neg   Fals.Pos
#     Y        Fals.Neg   Verd.Pos
#
# Y medidas de la bondad del diagnóstico son:
#     Sensibilidad  = Verd.Pos / (Verd.Pos + Fals.Neg) : 
#         caracteriza la capacidad de la prueba para detectar la "enfermedad"
#         en sujetos enfermos.
#
#     Especificidad = Verd.Neg / (Verd.Neg + Fals.Pos)
#         caracteriza la capacidad de la prueba para detectar la ausencia de 
#         "enfermedad" en sujetos sanos


cat("Tasa de acierto= ",100*mean(spam$yesno== predict(spam.rpart,type="class")) ,"\n")

cat("Sensitividad= ",100*confu[2,2]/sum(confu[2,]),"\n")
cat("Especificidad = ",100*confu[1,1]/sum(confu[1,]) ,"\n")


######## Orden prune(tree, ...)
##  Determina una secuencia anidada de subárboles del objeto rpart suministrada 
##  recursivamente cortando las divisiones menos importantes, basándose en el 
##  parámetro de complejidad (cp).
##  
##  Argumentos:
##      tree  : objeto rpart previamente ajustado
##       cp   : valor del parámetro complejidad para el cual el objeto rpart 
##              deberá ser truncado o podado
##
##  Resultado: Un nuevo objeto rpart
###########################################################################
spam.prun <- prune(spam.rpart, cp=0.012)

plot(spam.prun,margin=0.01,compress=TRUE, main="CART (árbol podado) datos spam")
text(spam.prun,col="red",cex=0.9)

spam.rpart$variable.importance
spam.prun$variable.importance

spam.rpart$cptable
spam.prun$cptable

spam.prun
########################################
#matriz de confusión en el árbol podado
confuprun<-table(spam$yesno,predict(spam.prun,type="class"),deparse.level = 2)
confuprun
confu
#porcentaje de acierto en el árbol podado
tacierprun<- 100*(confuprun[1,1]+confuprun[2,2])/sum(confuprun)
tacierprun
tacier


cat("Tasa de acierto (prun)= ",100*mean(spam$yesno== predict(spam.prun,type="class")) ,"\n")
cat("Tasa de acierto= ",100*mean(spam$yesno== predict(spam.rpart,type="class")) ,"\n")


cat("Sensitividad (prun)= ",100*confuprun[2,2]/sum(confuprun[2,]),"\n")
cat("Sensitividad= ",100*confu[2,2]/sum(confu[2,]),"\n")
cat("Especificidad (prun)= ",100*confuprun[1,1]/sum(confuprun[1,]) ,"\n")
cat("Especificidad = ",100*confu[1,1]/sum(confu[1,]) ,"\n")


## Predicción del árbol obtenido en el conjunto de datos: 
## Orden: predict(objeto,datos,type="prob")
##      objeto: árbol obtenido a partir del cual se pretende predecir
##      datos : conjunto de datos sobre el que se pretende predecir
##      type  : Si el objeto rpart es un árbol de clasificación, el valor por 
##              defecto es "prob", proporcionando una matriz cuyas filas 
##              corresponden a cada caso del conjunto de datos, y las columnas
##              contienen las probabilidades de pertenecer a cada clase
##              Si es un árbol de regresión, por defecto es "vector" proporcionando 
##              el vector de predicciones
##            type="vector", proporciona el vector de valores ajustados (regresión)
##               o las clases que predice el árbol (clasificación)
##            type="prob" (en clasificación) la matriz de probabilidades.
##     Existen otras posibilidades
##########################################3

prob.fit<- predict(spam.rpart,spam,type="prob")
prob.fit[1:10,]


