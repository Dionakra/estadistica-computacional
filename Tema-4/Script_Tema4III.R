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

### SELECCIÖN DE VARIABLES  ##########################################

### LECTURA FICHERO DE DATOS ######################################
## Datos: airepolu
## Descripción: se desea estudiar los factores socio-culturales que 
## influyen en la tasa de mortalidad de un conjunto de estados. 
## Variable Objetivo:  tasa de mortalidad (tasamor) 
## Variables Explicativas:
##           LLuvia: cantidad de lluvia registrada
##           educa: nivel de educación
##           densidad: densidad de población
##           pormin: Porcentaje de etnias minoritarias
##           nox: nivel de contaminación por nitrógeno
##           s02: nivel de contaminación por azufre
##           lnnox: log neperiano del nivel de contaminación por nitrógeno
##           lns02: log neperiano del nivel de contaminación por azufre
##
###################################################################

## Fichero en formato SPSS
library(foreign)
datos<-data.frame(read.spss("airepolu.sav"))
names(datos)


## REGRESIÓN LINEAL MÚLTIPLE

attach(datos)
summary(datos)

regrelin<-lm(tasamor~lluvia+densidad+educa+pormin+nox+so2+lnox+lnso2,data=datos)
regrelin$coef
summary(regrelin)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 917.311421  94.958807   9.660 4.14e-13 ***
#   lluvia        1.736655   0.703722   2.468   0.0170 *  
#   densidad      0.002483   0.003694   0.672   0.5046    
# educa       -12.323948   6.985877  -1.764   0.0837 .  
# pormin        2.943741   0.669553   4.397 5.60e-05 ***
#   nox          -0.268685   0.158919  -1.691   0.0970 .  
# so2           0.129396   0.124838   1.037   0.3049    
# lnox         13.280076   9.162487   1.449   0.1533    
# lnso2         5.361453   6.230326   0.861   0.3935    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 35.91 on 51 degrees of freedom
# Multiple R-squared:  0.7119,  Adjusted R-squared:  0.6667 
# F-statistic: 15.75 on 8 and 51 DF,  p-value: 2.157e-11


#################################################################################
##   REGRESIÓN POR PASOS: función step
##    AIC = - 2*log L + k * edf
##################################################################################
## Regresion paso a paso: backward
regrepas1<-step(regrelin, direction = c("backward"))
regrepas1$coef
summary(regrepas1)

 ## Regresion paso a paso: forward
regrepas2<-step(regrelin, direction = c("forward"))
regrepas2$coef
summary(regrepas2)

## Regresion paso a paso: both
regrepas3<-step(regrelin, direction = c("both"))
regrepas3$coef
summary(regrepas3)

### Regresión con Variables Cualitativas  #####################################
## Estudiamos si hay diferencia significativa en el saldo de la 
## tarjeta de crédito (balance) por sexo (entre hombres y mujeres)

datos <- read.table("Credit.csv",sep=",",header=T)
names(datos)
attach(datos)
table(Gender)

ajuste = lm(Balance~Gender)
summary(ajuste)
levels(Gender)
contrasts(Gender)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    509.80      33.13  15.389   <2e-16 ***
#   GenderFemale    19.73      46.05   0.429    0.669    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 460.2 on 398 degrees of freedom
# Multiple R-squared:  0.0004611,  Adjusted R-squared:  -0.00205 
# F-statistic: 0.1836 on 1 and 398 DF,  p-value: 0.6685

## Estudiamos si hay diferencia significativa en el saldo de la 
## tarjeta de crédito (Balance) por raza (Ethnicity).

table(Ethnicity)
ajuste = lm(Balance~Ethnicity)
summary(ajuste)
levels(Ethnicity)
contrasts(Ethnicity)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          531.00      46.32  11.464   <2e-16 ***
#   EthnicityAsian       -18.69      65.02  -0.287    0.774    
# EthnicityCaucasian   -12.50      56.68  -0.221    0.826    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 460.9 on 397 degrees of freedom
# Multiple R-squared:  0.0002188,  Adjusted R-squared:  -0.004818 
# F-statistic: 0.04344 on 2 and 397 DF,  p-value: 0.9575


## Regresión Lineal Múltiple
## Variable Cuantitativa (Income) + Cualitativa (Student)

table(Student)
ajuste = lm(Balance~Income+Student)
summary(ajuste)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 211.1430    32.4572   6.505 2.34e-10 ***
#   Income        5.9843     0.5566  10.751  < 2e-16 ***
#   StudentYes  382.6705    65.3108   5.859 9.78e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 391.8 on 397 degrees of freedom
# Multiple R-squared:  0.2775,  Adjusted R-squared:  0.2738 
# F-statistic: 76.22 on 2 and 397 DF,  p-value: < 2.2e-16

levels(Student)
contrasts(Student)




