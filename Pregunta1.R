#EJERCICIO 1 PRACTICA 3:

#APARTADO A --> ENSAYO BERNOULLI 
x<- c(0,1)
f<-c(0.68,0.32) 

plot(x,f,type='h', ylim=c(0,1), col='red')
points(x,f,pch=16, col='red')

n<- 43
muestra <- sample(x,n,f, replace=TRUE) #0 --> FALSE y 1 --> TRUE
table(muestra) #hace una muestra aeleatoria de lo que podria suceder 
pie(table(muestra))

table(muestra)/n
mean(muestra)

bar <- barplot(table(muestra)/n, ylim=c(0,1)) #los rectangulos(barras)iran variando porq son muestras aleatorias y van cambiando los valores 
lines(bar,f,type='h', col='red') 
points(bar,f,pch=16, col='red') #la funcion no cambiara (linea roja y punto) ya que la hemos definido como nos decia el enucniado

muestra <- sample(x,n,f, replace=TRUE)
Y <- function(i){sum(sample(x,n,f, replace=TRUE))}#cada vez que hacemos la muestra, diferentes personas me diran que si es decir ira cambiando (ya que es aleatoria) pero la probabilidad será la misma 

m <- 400
sapply(1:m, Y) #REPETIR LA FUNCION Y de 1 hasta m=40
#cada posicion nos dice el numero de personas que han dicho que si (tienen minimo 2 teles)

m <- 400
barplot(table(sapply(1:m, Y))) #el grafico de barras siempre es sobre la tabla 
barplot

set.seed(123) #si le ponemos la funcion setseed en el mismo punto nos dara a todos los mismos numeros en los experimentos 
m<- 400000
encuestas <- sapply(1:m, Y)              
fr <-table(encuestas)/m
fr["13"] #a) =0.1283625
#---------------------------------------------------------------------------

#APARTADO B)
dbinom(13, 43, 0.32) #--> dbinom(y, n, p) siendo y= resultado que quieres, n=43 y p =0.32 prob de q digan q si


xx <- names(fr) # numero de personas que respondieron que si en una encuesta de 43 es decir ni 1 ni 0 personas dijeron que si ni mas de 29 dijeron que si 
xx
br <- barplot(table(encuestas)/m)
lines(br,dbinom(2:29, 43, 0.32),type='h', col='red') #lo ponemos de 2 a 29 ya que en una encuesta no hay menos de 2 personas que hayan dicho que si ni tampoco mas de 29
points(br,dbinom(2:29, 43, 0.32),pch=16, col='red')

#IMPORTANTE --> diferenciar dbinom y pbinom!!!!!
dbinom(17, 44, 0.32) #la probabilidad de que salga 17 
plot(0:43, dbinom(0:43, 44, 0.32), type="h", col='red')

pbinom(16, 44, 0.32)#probabilidad de que salga menos de 17 es decir HACE UN SUMATORIO HASTA 16 
#b) = 0.7850429

#---------------------------------------------------------------------------------------------

#APARTADO C)
#ahora nos piden lo contrario por tanto ahora la prob = 0.68 

n <- 24
x <- c(0,1)
f <- c(0.32, 0.68)
Xstar <- function(i){sum(sample(x,n,f, replace=TRUE))}
set.seed(123)
m <- 400000  
encuestas <- sapply(1:m, Xstar)
mean(encuestas)
# a) = 16.32063
n*0.68 #lo mismo 

var(encuestas)
#b) = 5.203776
n*0.68*0.32

qbinom(0.25, 24, 0.68) #qbinom para el cuarto ya que si pusieramos pbinom nos daria la suma de valores hasta que x=0.25 
#c) = 15
plot(0:24, dbinom(0:24, 24, 0.68), type="h", col='red' )
#mirar el gráfico para entenderlo 
