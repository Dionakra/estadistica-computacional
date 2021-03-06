####################################################################
###    ESTAD�STICA COMPUTACIONAL
###    TEMA 2. MODELOS DE DISTRIBUCIONES.
###    
###    GRADO EN INGENIER�A INFORM�TICA-INGENIER�A DEL SOFTWARE
###    GRADO EN INGENIER�A INFORM�TICA- TECNOLOG�AS INFORM�TICAS
###    Juan M. Mu�oz Pichardo - Mar�a Dolores Cubiles de la Vega
###    Departamento de Estad�stica e I.O. Universidad de Sevilla
####################################################################
###  SCRIPT DEL DESARROLLO TE�RICO DEL TEMA
####################################################################


## DISTRIBUCI�N BINOMIAL (par�metros: num (n),prob (p))
## dbinom(x, num, prob) : funci�n de probabilidad Pr[X=x]
## pbinom(x, num, prob, lower.tail = TRUE) : funci�n de distribuci�n F
## qbinom(p, num, prob, lower.tail = TRUE) : funci�n cuantil Q
## rbinom(m, num, prob) : generaci�n de valores aleatorios de una Binomial
##    x - representa el vector de cuantiles
##    p - representa el vector de probabilidades
##    m - representa el n�mero de valores aleatorios deseado
##   lower.tail - l�gico. Por defecto TRUE: Pr[X <= x]; FALSE: Pr[X > x]
##   LA FUNCI�N CUANTIL: Q(p)=x con x valor m�s peque�o : F(x)>=p
#####################################################################


x<-0:30
y<-dbinom(0:30,30,0.16)
data.frame("Prob"=y,row.names=x)
plot(0:30,dbinom(0:30,30,0.15),,type='h',xlab="",ylab="Probabilidad", 
     sub="N�mero de e-mails spam entre los 30 recibidos")


par(mfrow=c(2,2))
plot(0:30,dbinom(0:30,30,0.15),type='h',xlab="",ylab="Prob", sub="p=0.15")
plot(0:30,dbinom(0:30,30,0.20),type='h',xlab="",ylab="Prob", sub="p=0.20")
plot(0:30,dbinom(0:30,30,0.25),type='h',xlab="",ylab="Prob", sub="p=0.25")
plot(0:30,dbinom(0:30,30,0.65),type='h',xlab="",ylab="Prob", sub="p=0.65")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(0:30,pbinom(0:30,30,0.15),type='s',xlab="",ylab="F Distr", sub="p=0.15")
plot(0:30,pbinom(0:30,30,0.20),type='s',xlab="",ylab="F Distr", sub="p=0.20")
plot(0:30,pbinom(0:30,30,0.25),type='s',xlab="",ylab="F Distr", sub="p=0.25")
plot(0:30,pbinom(0:30,30,0.65),type='s',xlab="",ylab="F Distr", sub="p=0.65")
par(mfrow=c(1,1))


### DISTRIBUCI�N DE POISSON (PAR�METRO lambda )
#####################################################################

x<-0:10
y<-dpois(0:10,0.80)
data.frame("Prob"=y,row.names=x)
plot(0:10,dpois(0:10,0.80),,type='h',xlab="",ylab="Probabilidad", sub="No. de trabajos en cola de impresi\U{f3}n")

par(mfrow=c(2,2))
plot(0:10,ppois(0:10,0.10),type='s',xlab="",ylab="F Distr", sub="lambda=0.10")
plot(0:10,ppois(0:10,0.80),type='s',xlab="",ylab="F Distr", sub="lambda=0.80")
plot(0:10,ppois(0:10,1.50),type='s',xlab="",ylab="F Distr", sub="lambda=1.50")
plot(0:10,ppois(0:10,4),type='s',xlab="",ylab="F Distr", sub="lambda=4.00")
par(mfrow=c(1,1))

### DISTRIBUCI�N NORMAL (PAR�METROS (mean,sd) )
##
## An�logamente a los casos anteriore
## dnorm(x, mean = 0, sd = 1)
## pnorm(q, mean = 0, sd = 1, lower.tail = TRUE)
## qnorm(p, mean = 0, sd = 1, lower.tail = TRUE)
##rnorm(n, mean = 0, sd = 1)
#####################################################################


x<-seq(-10,10,length=100)

plot(x,dnorm(x,0,1),xlab="x", ylab="f(x)", type='l', main="F. Densidad N(0,1)")

par(mfrow=c(2,2))
plot(x,dnorm(x,0,1),xlab="x",ylab="f(x)", type='l',main="F. Densidad N(0,1)")
plot(x,pnorm(x,0,1),xlab="x",ylab="F(x)", type='l',main="F. Dist N(0,1)")
plot(x,dnorm(x,0,2),xlab="x",ylab="f(x)", type='l',main="F. Densidad N(0,2)")
plot(x,pnorm(x,0,2),xlab="x",ylab="F(x)", type='l',main="F. Dist N(0,2)")
par(mfrow=c(1,1))


par(mfrow=c(2,2))
plot(x,dnorm(x,0,1),xlab="x",ylab="f(x)", type='l',main="F. Densidad N(0,1)")
plot(x,pnorm(x,0,1),xlab="x",ylab="F(x)", type='l',main="F. Dist N(0,1)")
plot(x,dnorm(x,3,1),xlab="x",ylab="f(x)", type='l',main="F. Densidad N(3,1)")
plot(x,pnorm(x,3,1),xlab="x",ylab="F(x)", type='l',main="F. Dist N(3,1)")
par(mfrow=c(1,1))

### DISTRIBUCI�N EXPONENCIAL (PAR�METRO lambda )
#####################################################################

par(mfrow=c(2,2))
plot(x,dexp(x,0.5),xlim=c(0,5),xlab="x",ylab="f(x)", type='l',main="F. Densidad Exp(0.5)")
plot(x,dexp(x,1),xlim=c(0,5),xlab="x",ylab="F(x)", type='l',main="F. Densidad Exp(1)")
plot(x,dexp(x,2),xlim=c(0,5),xlab="x",ylab="f(x)", type='l',main="F. Densidad Exp(2)")
plot(x,dexp(x,5),xlim=c(0,5),xlab="x",ylab="F(x)", type='l',main="F. Densidad Exp(5)")
par(mfrow=c(1,1))



colores<-c("black","red","blue","orange")
valores<-c(0.1,0.2,0.5,1)

plot(valores,valores,type="n",main="Funci�n de densidad",xlab="x",ylab="f(x)",
      ylim=c(0,1),xlim=c(0,5)) #Gr�fico inicial
curve(dexp(x,0.1),0,5,1000,xlim=c(0,5),xlab="x",ylab="f(x)",lwd=3, lty=4, add=T,type='l',col="black")
curve(dexp(x,0.2),0,5,1000,xlim=c(0,5),xlab="x",ylab="f(x)", lwd=3, lty=3, add=T,type='l',col="red")
curve(dexp(x,0.5),0,5,1000,xlim=c(0,5),xlab="x",ylab="f(x)", lwd=3, lty=3, add=T,type='l',col="blue")
curve(dexp(x,1),0,5,1000,xlim=c(0,5),xlab="x",ylab="f(x)", lwd=3, lty=1, add=T,type='l',col="orange")
grid()
legend("topright",lty=4:1,col=colores,lwd=3,legend=paste("lambda=",valores))

plot(valores,valores,type="n",main="Funci�n de Distribuci�n",xlab="x",ylab="F(x)",
     ylim=c(0,1),xlim=c(0,10)) #Gr�fico inicial
curve(pexp(x,0.1),0,10,1000,xlim=c(0,5),xlab="x",ylab="F(x)",lwd=3, lty=4, add=T,type='l',col="black")
curve(pexp(x,0.2),0,10,1000,xlim=c(0,5),xlab="x",ylab="F(x)", lwd=3, lty=3, add=T,type='l',col="red")
curve(pexp(x,0.5),0,10,1000,xlim=c(0,5),xlab="x",ylab="F(x)", lwd=3, lty=3, add=T,type='l',col="blue")
curve(pexp(x,1),0,10,1000,xlim=c(0,5),xlab="x",ylab="F(x)", lwd=3, lty=1, add=T,type='l',col="orange")
grid()
legend("bottomright",lty=4:1,col=colores,lwd=3,legend=paste("lambda=",valores))

