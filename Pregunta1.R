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


#----------------------------------------------------------------------------------------------------------------------------------
#PROBLEMA 1:
#1
dbinom(10, 25, 60/150)
#2
#p(x>=15) = 1- P(X<15) por tanto 14
pbinom(14, 40, 60/150)
#3
n <- 30
p <- 80/200
var <- n*p*(1-p) #formula binominal 
var

qbinom(0.25, n, p)

#4
x <- c(0, 1)
f <- c(0.6, 0.4)
n <- 35
Xstar <- function(i){sum(sample(x,n,f, replace=TRUE))}
set.seed(789)
m <- 10000 
encuestas <- sapply(1:m, Xstar)
table(encuestas)
mean(encuestas)


n4 <- 35               
p4 <- 48 / 120         
simulaciones <- 10000  

set.seed(789)          
resultados <- rbinom(simulaciones, size = n4, prob = p4)
table(resultados)
promedio_resultados <- mean(resultados)

#-------------------------------------------------------------
#EJ MERY
#en un anàlisis medioambiental, s'estima que cada mostra d'1 centimetre cubic 
# té un 15% de prob de contenir una molecula rara. ASUMINT QUE LES MOSTRES SON INDEPENDENTS:

#1: si analitzem mostres d'aire de forma secuencial fins obtenir tres mostres que continguin una molecula rara, quina es la porbabilitat que necessitem analitzar exactament 8 mostres per aconseguir-ho?
dnbinom((8-3), 3, 0.15) #CUIDADOO
#ES LA BINOMINAL NEGATIVA 

#2 prob que necessitem analitzar 
#al menys 10 mostres per trobar cinc mostres que continguin la molec rara
#P(X>=10) = 1- P(x<10)
# POR LO TANTO PONEMOS UN 9 
1 - pnbinom(9-5, 5, 0.15) #CUIDADO AQUÍ 


#3 Suposem que cada mostra té un 20% de prob de obtenir molec rara
#si X es el nombre de mostres que necessitem analitzar per trobar 4 mostres que continguin molec rara. 

# varainza en binominal negativa:
p <- 0.2
n <- 4
var <- n*(1-p)/(p^2) # CAMBIA --> memoria 
var

#segundo cuartil
qnbinom(0.50, 4, 0.2)

#4 
prob <- 0.12
n <- 10000
set.seed(789)
rnbinom(n, 6, 0.12) + 6 #SUMARLE LA N 
mean(rnbinom(n, 6, 0.12)+6)

#5 seleccionem 15 mostres cadascuna amb una prob del 10%
#de contenir la molec rara, quien es la prob de q exactament 4 mostres continguinn molecula rara 
#YA NO ES NEGATIVA CUIDADO 
dbinom(4, 15, 0.1)

#-----------------------------------------------------------------------------
# se sabe que cada radar funciona de manera independiente de los demás 
#y que la probabilidad de que un radar detecte un misil es de 0.8 


#1
# si se instalan 3 radares en la base, cual es la prob de que un misil 
#sea detectado por al menos dos de los radares
#P(X>=2) = 1 - P(x<2)
p<- 0.8
n <- 3 
1- pbinom(1, 3, 0.8)

#2 cuantos radares serian necessarios para asegurar 
#que la prob de que un misil no sea detectado por ningun radar sea menor a 0.0001?

#0.2^n<0.0001
n <- log(0.001)/log(0.2)
n 
# como nos da 4.29 vamos a poner 5 radares

#3 si se pueden instalart unicamente 3 radares, pero es possible mejorar la tecnologia, 
#cuál debería ser la probabilidad de detecccion de cada radar para asegurar que la prob
#de que un misil no sea detectado por ningun radar sea menor a 0.0001
#(1-p)^n < 0.0001

p <- log(1-0.0001)^(1/3)

#para aislar p 
f <- function(x) (1-x)^3-0.0001
sol <- uniroot(f, c(0,1))$root
sol

#4
set.seed(456)
rbinom(10000, 5, 0.75) # rbinom(n, numradares, prob)
mean(rbinom(10000, 5, 0.75))

#5 ahora se revisan los radares secuencialmente hasta que uno de ellos detecte un missil 
#si cada radar tiene una prob de deteccion de 0.6, prob de que necesitemos revisar exactamente 4 radares para q el misisl sea detectado por el primer radar?
p <- 0.6
n <- 4
dgeom(n-1, p)

#formula teorica ((1-0.6)^n-1)*0.6


