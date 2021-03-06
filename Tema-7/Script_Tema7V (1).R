####################################################################
###    ESTAD�STICA COMPUTACIONAL
###    TEMA 7. CLASIFICACI�N Y DISCRIMINACION.
###    
###    GRADO EN INGENIER�A INFORM�TICA-INGENIER�A DEL SOFTWARE
###    GRADO EN INGENIER�A INFORM�TICA- TECNOLOG�AS INFORM�TICAS
###    Juan M. Mu�oz Pichardo - Mar�a Dolores Cubiles de la Vega
###    Departamento de Estad�stica e I.O. Universidad de Sevilla
####################################################################
###           CUARTO SCRIPT DEL DESARROLLO TE�RICO DEL TEMA
###           AARBOLES DE CLASIFICACI�N          
###                    Conjunto de datos: spam.txt   
####################################################################

### DIRECTORIO DE TRABAJO ####################################################

setwd("D:/TRABAJOPERSONAL/DOCENCIA/ECOMPUTACIONAL/Practicas")


###  LECTURA DE DATOS  #######################################################

### cONJUNTO DE DATOS: spam.txt
##   Variables predictoras:
##     crl.total  : Longitud total de las palabras en may�sculas
##     dollar     : frecuencia del s�mbolo $, como % de todos los caracteres
##     bang       : idem para el s�mbolo !
##     money      : frecuencia de la palabra "money" como % de todas las palabras
##     n000       : frecuencia de la tira "000", % de todos los caracteres
##     make       : frecuencia de la palabra "make", como % de todas las palabras
##   Variable respuesta: 
##     yesno      : "n" no spam, "y" s� spam.
####################

spam <- read.table("spam.txt", header=TRUE)
names(spam)
summary(spam)
str(spam) 

dim(spam)
spam[1:4,]
####################


library (randomForest)


### Librer�a randomForest #######################################################################
#
# Implementa el algoritmo "bosque aleatorio" de Breiman para clasificaci�n y regresi�n. 
# Tambi�n se puede utilizar en el modo "sin supervisi�n" para evaluar proximidades entre 
# los puntos o datos
#
#  Orden:
# randomForest(formula, data=NULL, ..., subset, na.action=na.fail)
#
# - data      : marco de datos (opcional) que contiene las variables en el modelo. 
#               Por defecto se toman las variables del entorno activo.
# - subset    : vector de �ndices que indican las filas que se deben utilizar. 
# - na.action : funci�n para especificar la acci�n a tomar si se encuentran valores perdidos.
# - formula   : f�rmula que describe el modelo que se pretende ajustar. 
#               Tambien se puede especificar indicando como primer argumento de la orden
#               el marco de datos o matriz de predictores (x) y a continuaci�n el vector de
#               datos de la variable respuesta (y). Si es un factor se asume un modelo de 
#               clasificaci�n, y en otro caso un modelo de regresi�n.
# - xtest     : marco de datos o matriz (como x) con los predictores para el conjunto test
# - ytest     : datos de la variable respuesta en el conjunto test
# - ntree     : N�mero de �rboles que se deben realizar. No se debe establecer en un n�mero 
#               demasiado peque�o.
# - mtry      : N�mero de variables seleccionadas aleatoriamente para cada divisi�n de cada �rbol 
#               Por defecto, los valores son diferentes para la clasificaci�n, sqrt(p),
#               donde p es el n�mero de variables en x,  y para la regresi�n (p/3) 
#               Si se indica mtry=p entonces se est� aplicando el algoritmo BAGGING
#               Si mtry<p entonces se aplica el algoritmo RANDOM FOREST
# - nodesize  : Tama�o m�nimo de los nodos terminales. Tomando este n�mero grande hace que los 
#               �rboles sean m�s peque�os (menos nodos) y,  por tanto, se necesitar� menos tiempo.
#               Los valores por defecto son para la clasificaci�n (1) y para la regresi�n (5).
# - maxnodes  : N�mero m�ximo de nodos terminales que los �rboles del bosque puede tener. 
#               Si no se indica, los �rboles se realizan hasta el m�ximo posible (sujeto a los 
#               l�mites de nodesize). 
# - importance : �Se debe registrar la "importancia" de los predictores?
# - localImp   : �Se debe regristrar la importancia de los casos? En caso afirmativo, anula "importance"
# - norm.votes : Si TRUE (por defecto), el resultado final de los votos (o frecuencias) se expresan
#               como fracciones. Si es falso, se ofrecen frecuencias absolutas, lo que es �til 
#               para combinar los resultados de diferentes realizaciones (ignorado para regresi�n).
# - classwt    : Probabilidades a priori de las clases (se ignora en regresi�n).
# - sampsize   : Tama�o(s) de muestra. Para la clasificaci�n, si sampsize es un vector de longitud
#                ifual al n�mero de estratos, entonces el muestreo es estratificado por "strata"
#                y los elementos de "sampsize" indican lostama�os muestrales que se deben extraer 
#                en cada uno de los estratos.
#
#  Otras opciones
# - replace    : �El muestreo de los casos debe ser con o sin reemplazamiento?
# - cutoff     : (S�lo Clasificaci�n ) Un vector de longitud igual al n�mero de clases . 
#                La clase "ganadora" para una observaci�n es la que tiene la m�xima raz�n de 
#                proporci�n de votos y mayor que cutoff. El valor predeterminado es 1/k, donde k 
#                es el n�mero de clases (es decir, la case con "mayor�a de votos").
# - strata     : Una variable (factor) que se utiliza para el muestreo estratificado.
# - nPerm      : N�mero de veces que los datos "OOB"(out of bag) se permutan por �rbol para la evaluaci�n de 
#                la importancia variable. N�mero mayor que 1 proporciona una estimaci�n 
#                ligeramente m�s estable, pero no muy eficaz. S�lo implementado para la regresi�n .
# - proximity  : �deben calcularse las proximidades entre casos?
# - oob.prox   : �se calcula la proximidad �nicamente en los datos OOB?
# - norm.votes : Si TRUE (por defecto), el resultado final de los votos (o frecuencias) se expresan
#                como fracciones. Si es falso, se ofrecen frecuencias absolutas, lo que es �til 
#                para combinar los resultados de diferentes realizaciones (ignorado para regresi�n).
# - keep.forest: Si FALSE, el bosque no se guardar� en el objeto de salida . 
# - corr.bias  : �se debe realizar la correcci�n de sesgo de la regresi�n? 
# - keep.inbag : �Deber�a proporcionar una matrix (n x ntree) que realiza un seguimiento 
#                 de cu�les son los casos "in bag" (dentro de las bolsas) en los �rboles 
#                (pero no el n�mero de veces , si el muestreo es con reemplazamiento )
#
# Existen otras opciones
##################################################################################################


set.seed(1) # fija la semila con objeto de poder seleccionar la misma muestra en pr�ximas ocasiones
bag.spam =randomForest(yesno~.,data=spam , mtry=6, importance =TRUE,replace=TRUE,ntree=350, norm.votes="FALSE")
bag.spam
names(bag.spam)

bag.spam$type # Clasificaci�n o regresi�n

# bag.spam$predicted proporciona las clases asignadas seg�n el m�todo out-of-bag
bag.spam$predicted[1:5]

# bag.spam$err.rate proporciona (s�lo para clasificaci�n) el vector de las tasas de error  
#                   de la predicci�n en los datos de entrada, el elemento i-�simo es la
#                   tasa de error OOB acumulado hasta el i-�simo �rbol generado
dim(bag.spam$err.rate)
bag.spam$err.rate[1:5,]

bag.spam$confusion # (s�lo para clasificaci�n) matriz de confusi�n de la predicci�n, 
                   #basado en el m�todo OOB

# bag.spam$votes    (s�lo para clasificaci�n) matriz con una fila para cada caso 
#                    y una columna para cada clase, proporcionando la fracci�n o el n�mero 
#                    de votos (OOB) del bosque aleatorio. Si norm.votes="FALSE", proporciona
#                    los totales
dim(bag.spam$votes)
bag.spam$votes[1:5,]

# bag.spam$oob.times n�mero de veces que los casos son 'OOB' (y por lo tanto se utiliza en el c�lculo
#                    de estimaci�n del error OOB)
bag.spam$oob.times[1:5]

bag.spam$classes   # Clases del estudio

bag.spam$importance # una matriz con NClass+2 (para clasificaci�n) o 2 (para regresi�n) columnas y
                    # tantas filas como variables explicativas
                    # Para la clasificaci�n, las NClass primeras columnas son las medidas espec�ficas
                    # de cada clase calculadas como media del "decrecimiento" de la precisi�n respecto
                    # a las estimacione basadas en permutaciones. 
                    # La columna NClass + primera es la disminuci�n media en la precisi�n en todas 
                    # las clases debido a la variable explicativa asociada. 
                    # La �ltima columna es la disminuci�n media en el �ndice de Gini. 
                    # Para la regresi�n, v�ase el manual

bag.spam$importanceSD # Errores estandar de la medida de importancia basada en permutaciones

bag.spam$ntree      # n�mero de �rboles realizados
bag.spam$mtry       # n�mnero de predictores muestreados utilizados en la construcci�n de cada �rbol

#### Representaci�n gr�fica 
plot(bag.spam,type="l")  # Representa los valores almacenados en "bag.spam$err.rate"
bag.spam$err.rate[1:5,]
bag.spam$err.rate[340:350,]

varImpPlot(bag.spam)  # Representa la importancia de las variables
varImpPlot(bag.spam,sort="TRUE",n.var=6,col=2)


#### Predicci�n para datos nuevos
#
# Orden: predict(object, newdata, type="response",
#               norm.votes=TRUE, predict.all=FALSE, 
#               proximity=FALSE, nodes=FALSE,cutoff, ...)
# object   :  objeto creado por la orden "randomForest"
# newdata  :  conjunto de datos sobre el que se quiere predecir.
#             Si no se indica, se realiza la predicci�n out-of-bag (OOB)
# type     :  Uno de las tres siguientes opciones, para indicar el resultado
#             - response : valores pronosticados
#             - prob.    : matriz de probabilidades de clase
#             - vote     : matriz de voto contabilizados
# norm.votes : �Se deben normalizar (expresados en fracci�n) el n�mero de votos?
# predict.all : �Se debe guardar la predicci�n de todos los �rboles?
# cutoff   : (S�lo para clasificaci�n) Vector de longitud igual al n�mero de 
#             clases. La clase "ganadora" para una observaci�n es la que tiene 
#             la m�xima proporci�n de votos mayor que cutoff. Por defecto se toma 
#             los valores proporcionados, en su caso, en la orden de creaci�n
#             del objeto "randmForest".
##################################################################################

nuevosdat = rbind(c(245,0.13,0.4,0.0,0.0,0.1),c(10,0.2,0.8,0.4,0.3,0.1))
colnames(nuevosdat)=c("crl.tot", "dollar",  "bang", "money", "n000", "make")
nuevosdat
predict(bag.spam,nuevosdat)
predict(bag.spam,nuevosdat,type="vote",norm.votes="F")
predict(bag.spam,nuevosdat,type="prob")

