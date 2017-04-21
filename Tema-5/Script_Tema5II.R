####################################################################
###    ESTADÍSTICA COMPUTACIONAL
###    TEMA 5. TÉCNICAS DE REDUCCIÓN DE LA DIMENSIÓN.
###    
###    GRADO EN INGENIERÍA INFORMÁTICA-INGENIERÍA DEL SOFTWARE
###    GRADO EN INGENIERÍA INFORMÁTICA- TECNOLOGÍAS INFORMÁTICAS
###    Juan M. Muñoz Pichardo - María Dolores Cubiles de la Vega
###    Departamento de Estadística e I.O. Universidad de Sevilla
####################################################################
###   SEGUNDO SCRIPT DEL DESARROLLO TEÓRICO DEL TEMA
###           ANÁLISIS DE COMPONENTES PRINCIPALES
####################################################################

### DIRECTORIO DE TRABAJO ####################################################

setwd("L:/TRABAJOPERSONAL/DOCENCIA/ECOMPUTACIONAL/Practicas")


### LECTURA FICHERO DE DATOS ######################################
## Datos: ACPGinecologia.csv 
## Descripción: 
##      REVISTA CHILENA DE OBSTRESTICIA Y GINECOLOGÍA, 2006; 71(1): 17-25
##      "ANÁLISIS DE COMPONENTES PRINCIPALES APLICADO A VARIABLES RESPECTO A LA MUJER GESTANTE 
##      EN LA REGIÓN DE LAS AMÉRICAS"
##      Hugo Salinas P., Jaime Albornoz V., Alvaro Reyes P., Marcia Erazo B., Rodolfo Ide V.
##
##      Se recopila información oficial de los países, correspondiente a variables relacionadas con 
##      la mujer embarazada y los recursos potenciales utilizados en su atención, además de los 
##      resultados obtenidos en dicha atención. 
##      Se estudiaron 9 variables:
##
##    IAR1   Tasa global de fecundidad (hijos/mujer) 2003
##    IAR2   Tasa cruda de natalidad (por 1.000 habitantes)
##    IAR3   Razón de mortalidad materna reportada (por 100.000 nv)
##    IAR4   Prevalencia de uso de métodos anticonceptivos en mujeres en edad fértil (%)
##    IAPP1  % población gestante atendida por personal capacitado durante embarazo
##    IAPP2  Proporción de partos atendidos por personal capacitado (%)
##    IG1    Gasto nacional en salud por año per cápita (US$ corrientes) ($ per capita)
##    IG2    Gasto nacional en salud por año como proporción del PIB (%)
##    IG3    Gasto público en salud por año como % del gasto nacional en salud (%)
##
###################################################################

datos <- read.table("ACPGinecologia.csv", header=TRUE, sep=";",row.names="PAIS")

names(datos)
datos
class(datos)

##################################################################################
##                               ANÁLISIS DE COMPONENTES PRINCIPALES (ACP)
##
##  
##
##  Orden básica: princomp(...)
##
##    princomp(formula, data = NULL, subset, ...)
##    princomp(x, cor = FALSE, scores = TRUE, covmat = NULL, subset = rep(TRUE, nrow(as.matrix(x))), ...)
##
##    ARGUMENTOS:
##   
##  formula:  formula sin variable respuesta, sólo con variables numéricas
##            Por ejemplo:  ~ varX1 + varX2 + varX3 + varX4
##
##  data  : un marco de datos opcional que contenga las variables de la fórmula
##
##  subset : un vestor opcional para seleccionar las filas (observaciones) de la matriz de datos
##
##  x  : matriz o marco de datos que proporciona los datos que proporciona los datos para el ACP
##
##  cor: Valor lógico (T ó F) indicando si se usa la matriz de correlación (T) o la matriz de covarianzas (F).
##       
## scores : valor lógico (T/F) indicando si las puntuaciones de cada c.p. deben ser calculadas  
##
## 
##  Crea un objeto "princomp" que recoge la siguiente información 
##   sdev : desviaciones estándar de las comp.principales.
##   loadings : matriz de cargas (es decir, matriz de autovectores)
##   center : las medias 
##   scaling : la escala aplicada a cada variable
##   n.obs   : número de observaciones 
##   scores  : Si se ha solicitado, las puntuaciones de los datos en las c.p.
##################################################################################

# Resumen, media y desviación típica.
summary(datos)
colMeans(datos)
apply(datos,2,mean)
apply(datos,2,sd)

# Matriz de covarianzas y matriz de correlación
Sigmaestim <- cov(datos)
Sigmaestim
Restim<-cor(datos)
Restim


#Análisis de Componenetes Principales sobre la matriz de Correlación
cp<-princomp(~.,data=data.frame(datos),cor=TRUE,scores=T)
cp
names(cp)

# Desviacion típica, porcentaje de varianza explicativa
summary(cp)
cp$sdev

#Autovalores (recuérdese que coinciden con la varianza de cada cp)
eigen(Restim)$values
cp$sdev[1]^2 #Coincide con el primer autovalor

#Representación gráfica de la varianza explicada
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

#Escalas o desviaciones típicas de las variables originales
cp$scale # (divide por n numero de casos)
apply(datos,2,sd) #(divide por (n-1) )


summary(cp)

##########################################################################
# Suponiendo que se consideran sólo las 2 primeras componentes principales
##########################################################################
# Variabilidad explicada conjuntamente 67.15%
# Variabilidad explicada por CP1: 50.38%
# Variabilidad explicada por CP2: 16,77%

#Matriz de componentes
C=cor(datos,predict(cp)[,1:2])
C

# Comentarios incluidos en el artículo de referencia
#
# La matriz de correlaciones variables-cp, nos muestra que la primera componente
# se correlaciona, en forma directamente proporcional, con la variable
# IAPP2 (proporción de partos atendidos por personal capacitado) y con ISR4
# (prevalencia de uso de métodos anticonceptivos en mujeres en edad fértil);
# se correlaciona en forma inversa con la ISR2 (tasa cruda de natalidad), 
# ISR1 (tasa global de fecundidad) y con ISR3 (razón de mortalidad materna).
# Con las restantes, la correlación es positiva pero con valores moderados
#
# La segunda CP se relaciona en forma directa con IG2 (gasto nacional en salud por 
# año como proporción del PIB) y en forma inversa con IG3 (gasto público en
# salud por año como proporción del gasto nacional en salud). 
# Asimismo, se relaciona en forma directa, aunque con menos significación estadística, 
# con (IG1) el gasto nacional en salud por año per cápita. 
# Con las demás variables estudiadas se encontró una baja correlación.
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


