#ESTADÍSTICA COMPUTACIONAL

##Juan M. Muñoz Pichardo - María Dolores Cubiles de la Vega

##Departamento de Estadística e I.O. Universidad de Sevilla

###TEMA 1. SCRIP INTRODUCCION AL R. (1)



help(plot)
?plot

## Paquetes en R


install.packages("XLConnect")##instalar
require(XLConnect)##cargar 
library(XLConnect)## cargar
detach("package:XLConnect") ##lo descargamos
remove.packages("XLConnect")## lo desinstalamos


## Generar datos


a<- 5.69
vector<-c(9,4,5.8,7)
vectordecaracteres<-c("Lunes","Martes","Miercoles")
vectorenteros<--2:6
vectorsecuencia<-seq(5,10,0.5)

# Al poner el comando entre paréntesis, además de ejecutarlo lo saca por pantalla
# Así podemos verlo sin necesidad del 'debugging'
# (comando)


###Generar tablas


set.seed(10)
Colesterol<-sample(120:260,5,replace=TRUE) # Con reemplazamiento --> Los valores se pueden repetir
Glucemia<-sample(70:110,5,replace=TRUE)
# nos crea la tabla, con dos columnas, Colesterol | Glucemia, 
# pueden ser variables de distinto tipo, es decir, int y char etc..., 
# pero tienen que tener siempre las mismas líneas (obvio)
misdatos<-data.frame(Colesterol,Glucemia)
misdatos<-data.frame(Colesterol,Glucemia,row.names=c("A","B","C","D","E"))##etiquetar a los individuos

## attach(tabla) me devuelve las columnas y puede referenciar las variables sin declararlas


misdatos

set.seed(5)
Notas=sample(1:10,4,replace=TRUE)
Sexo=sample(c("Hombre","Mujer"),4,replace=TRUE)
misdatos<-data.frame(Notas,Sexo,row.names=c("Acosta","Berruezo","Campos","Dominguez"))
misdatos

##Importar datos

titanic<-read.table("http://knuth.uca.es/repos/ebrcmdr/bases_datos/titanic.dat")

# Desde un archivo

mexico<-read.table("mexico.txt",header=T)##En caso de que esté en el directorio de trabajo


require(XLConnect)
wb <- loadWorkbook("sevilla.xls")##Nos carga el archivo
contaminacion<- readWorksheet(wb, sheet="municipios")
rm(wb)


datos = read.csv("datos_ordenador.csv",header=T)

## Exportar datos

save(datos,file="Archivomisdatos.RData")
load(file="Archivomisdatos.RData")

save.image(file="Todo.RData")
load(file="Todo.RData")
