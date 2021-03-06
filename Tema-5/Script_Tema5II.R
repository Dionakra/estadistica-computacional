####################################################################
###    ESTAD�STICA COMPUTACIONAL
###    TEMA 5. T�CNICAS DE REDUCCI�N DE LA DIMENSI�N.
###    
###    GRADO EN INGENIER�A INFORM�TICA-INGENIER�A DEL SOFTWARE
###    GRADO EN INGENIER�A INFORM�TICA- TECNOLOG�AS INFORM�TICAS
###    Juan M. Mu�oz Pichardo - Mar�a Dolores Cubiles de la Vega
###    Departamento de Estad�stica e I.O. Universidad de Sevilla
####################################################################
###   SEGUNDO SCRIPT DEL DESARROLLO TE�RICO DEL TEMA
###           AN�LISIS DE COMPONENTES PRINCIPALES
####################################################################

### DIRECTORIO DE TRABAJO ####################################################

setwd("L:/TRABAJOPERSONAL/DOCENCIA/ECOMPUTACIONAL/Practicas")


### LECTURA FICHERO DE DATOS ######################################
## Datos: ACPGinecologia.csv 
## Descripci�n: 
##      REVISTA CHILENA DE OBSTRESTICIA Y GINECOLOG�A, 2006; 71(1): 17-25
##      "AN�LISIS DE COMPONENTES PRINCIPALES APLICADO A VARIABLES RESPECTO A LA MUJER GESTANTE 
##      EN LA REGI�N DE LAS AM�RICAS"
##      Hugo Salinas P., Jaime Albornoz V., Alvaro Reyes P., Marcia Erazo B., Rodolfo Ide V.
##
##      Se recopila informaci�n oficial de los pa�ses, correspondiente a variables relacionadas con 
##      la mujer embarazada y los recursos potenciales utilizados en su atenci�n, adem�s de los 
##      resultados obtenidos en dicha atenci�n. 
##      Se estudiaron 9 variables:
##
##    IAR1   Tasa global de fecundidad (hijos/mujer) 2003
##    IAR2   Tasa cruda de natalidad (por 1.000 habitantes)
##    IAR3   Raz�n de mortalidad materna reportada (por 100.000 nv)
##    IAR4   Prevalencia de uso de m�todos anticonceptivos en mujeres en edad f�rtil (%)
##    IAPP1  % poblaci�n gestante atendida por personal capacitado durante embarazo
##    IAPP2  Proporci�n de partos atendidos por personal capacitado (%)
##    IG1    Gasto nacional en salud por a�o per c�pita (US$ corrientes) ($ per capita)
##    IG2    Gasto nacional en salud por a�o como proporci�n del PIB (%)
##    IG3    Gasto p�blico en salud por a�o como % del gasto nacional en salud (%)
##
###################################################################

datos <- read.table("ACPGinecologia.csv", header=TRUE, sep=";",row.names="PAIS")

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
Sigmaestim <- cov(datos)
Sigmaestim
Restim<-cor(datos)
Restim


#An�lisis de Componenetes Principales sobre la matriz de Correlaci�n
cp<-princomp(~.,data=data.frame(datos),cor=TRUE,scores=T)
cp
names(cp)

# Desviacion t�pica, porcentaje de varianza explicativa
summary(cp)
cp$sdev

#Autovalores (recu�rdese que coinciden con la varianza de cada cp)
eigen(Restim)$values
cp$sdev[1]^2 #Coincide con el primer autovalor

#Representaci�n gr�fica de la varianza explicada
plot(cp)

# Autovectores o matriz de cargas
cp$loadings
eigen(Restim)$vectors

# Puntuaciones de las cp en cada uno de los casos
cp$scores
predict(cp)


# Correlaciones entre las variables originales y las C.P.
C=cor(datos,predict(cp))
C

# Centros o Medias muestrales de las variables originales
cp$center
colMeans(datos)

#Escalas o desviaciones t�picas de las variables originales
cp$scale # (divide por n numero de casos)
apply(datos,2,sd) #(divide por (n-1) )


summary(cp)

##########################################################################
# Suponiendo que se consideran s�lo las 2 primeras componentes principales
##########################################################################
# Variabilidad explicada conjuntamente 67.15%
# Variabilidad explicada por CP1: 50.38%
# Variabilidad explicada por CP2: 16,77%

#Matriz de componentes
C=cor(datos,predict(cp)[,1:2])
C

# Comentarios incluidos en el art�culo de referencia
#
# La matriz de correlaciones variables-cp, nos muestra que la primera componente
# se correlaciona, en forma directamente proporcional, con la variable
# IAPP2 (proporci�n de partos atendidos por personal capacitado) y con ISR4
# (prevalencia de uso de m�todos anticonceptivos en mujeres en edad f�rtil);
# se correlaciona en forma inversa con la ISR2 (tasa cruda de natalidad), 
# ISR1 (tasa global de fecundidad) y con ISR3 (raz�n de mortalidad materna).
# Con las restantes, la correlaci�n es positiva pero con valores moderados
#
# La segunda CP se relaciona en forma directa con IG2 (gasto nacional en salud por 
# a�o como proporci�n del PIB) y en forma inversa con IG3 (gasto p�blico en
# salud por a�o como proporci�n del gasto nacional en salud). 
# Asimismo, se relaciona en forma directa, aunque con menos significaci�n estad�stica, 
# con (IG1) el gasto nacional en salud por a�o per c�pita. 
# Con las dem�s variables estudiadas se encontr� una baja correlaci�n.
#################


#Puntuaciones
cp$scores[,1:3]
plot(datos)

#Puntuaciones
cp$scores[,1:2]
plot(datos)


plot(cp$score[,1],cp$score[,2],pch=16,xlab="Primera C.P.",ylab="Segunda C.P.")
text( cp$score[,1],cp$score[,2], row.names(datos), cex=0.7, pos=3, col="blue")
grid()




plot(C[,1],C[,2],pch=16,xlab="Primera C.P.",ylab="Segunda C.P.",xlim=c(-1.0,1.0),ylim=c(-1,1))
text( C[,1],C[,2], row.names(C), cex=0.7, pos=3, col="blue")
grid()


