#estima que els drons són detectats 
#a una taxa mitjana de 1.2 vegades per hora

#1 calcula la prob de que exactamente 2 drons 
#activin el radar en una hora determinada
lambda <- 1.2
dpois(2, lambda)

#2 calcula la prob que el radar s'activi almenys 
#una vegada en un periode de dues hores --> lambda *2
#P(X>=1)=1-P(X<1)
lambda <- lambda*2
lambda
1-ppois(0, lambda)

#3 el període s'esten a 3 hores, X representa el nombre de detecciones
#valor esperat = lambda
#var = lambda
lambda <- 1.2
valor_Esperado <- lambda*3
valor_Esperado
varianza <- lambda*3
varianza

#4
lambda_2 <- 1.5
rpois(10000, lambda_2)
set.seed(789)
mean(rpois(10000, lambda_2))


lambda_2 <- 1.5
set.seed(789)
mean(rpois(10000, lambda_2))

#5 calcula el nombre minims de radars per a que la prob que no hi hagi 
#deteccions en una hora sigui menor a 0.01 assumint que la mitja es de 0.3
p <- 0.01
lambda <- 0.3
(-log(p) / lambda)
#---------------------------------------------------------------
# els pesos de caniches es distribueixen de forma normal amb 
media <- 7.4
sd <- 1.42
#1 seleccioneu un grafic: 
x <- seq(0,18, by=0.4)
densidad <- dnorm(x,media,sd)
plot(x, densidad, type = "l", col = "red", lwd=2)

#2 P(X>6.6)
1-pnorm(6.6, media, sd)

#3 prob de que pese més de 5.6 però menys de 8.4
pnorm(8.4,media,sd)-pnorm(5.6,media,sd)
#F(B)-F(A)

#4 percentil 0.94
qnorm(0.94,media,sd)

#5 un grup de 10 caniches alatzar, prob que el pes total sigui menus que 70P(X<70)
n <- 10
k <- 70
media_total <- n*media
sd_total <- sqrt(n)*sd
pnorm(k,media_total,sd_total)

#---------------------------------------------------
#3
mu <- 897

#valor esperado de la suma
n <- 49
n*mu

#varinza de la media
mu <- 897
n <- 49
prob <- 0.983
x <- 919

z <- qnorm(prob)
sigma <- (x - mu) * sqrt(n) / z
sigma
varianza <- (sigma^2) / n;varianza

#prob de la suma dels continuguts no sigui menor 44169
sum_crit <- 44169
mean <- n * mu
std<- sigma * sqrt(n)
prob <- 1 - pnorm(44168, mean = mean, sd = std);prob

#mida minima de la muestra per a obtenir una prob
#de menos del 3.4% que la media no pase los 878 
prob <- 0.034
x <- 878
z_tg <- qnorm(prob)
n_min <-((sigma * z_tg / (mu - x))^2);n_min

#
n_nueva <- 25
nueva_tg <- 977
prob_tg <- 0.10
z.2 <- qnorm(prob_tg, lower.tail = FALSE)
crecimiento.2 <- (nueva_tg - mu) + z.2* (sigma / sqrt(n_nueva));crecimiento.2
#-------------------------------------------------------------------------------------------------------------------------------
# SOlucion Ejercicio

x1 = 0:5   # Posibles resultados
f1 = c(1,2,2,3,4,3)/15 # Probabilidad de ocurrencia de cada resultado
# Creación de las líneas verticales
plot(x1, f1, type="h", col="red", lwd=3, main="Función de probabilidad", xlab="X", ylab="f(x)",
     xlim=c(-0.5,5.5), ylim=c(0,0.3)) 
# Se crean los puntos y se guarda la gráfica completa en un objeto para su uso posterior
points(x1, f1, col="red", lwd=10)




F1 = cumsum(f1) # Se genera un vector con la suma acumulada
plot(c(-1,x1,6), c(0,F1,1), type="s", col="red", lwd=3, main="Función de distribución", xlab="X",
     ylab="F(x)")
points(x1, F1, col="red", lwd=8)




#f(2)=2/15
#F(2)=1/3
#f(3.5)=0
#F(3.5)=8/15
#f(6)=0
#F(6)=1


miu.X <- sum(x1*f1); miu.X


Q2.X <- max(x1[F1<=0.5]); Q2.X


var.X <- sum((x1-miu.X)^2*f1); var.X


set.seed(12)
sim.venta <- sample(x1,30,replace=T,prob=f1)


fi <- table(sim.venta)/length(sim.venta) 
xb <- barplot(fi)
lines(xb, f1, type="h", col="red", lwd=3)  
points(xb, f1, col="red", lwd=10)

mean.sim <- mean(sim.venta); mean.sim

var.sim <- var(sim.venta); var.sim


set.seed(12)
sim.venta <- sample(x1,10000,replace=T,prob=f1)
fi <- table(sim.venta)/length(sim.venta) 
xb <- barplot(fi)
lines(xb, f1, type="h", col="red", lwd=3)  
points(xb, f1, col="red", lwd=10)


mean.sim <- mean(sim.venta); mean.sim

var.sim <- var(sim.venta); var.sim


miu.Y <- 0.75*miu.X-1.5; miu.Y
var.Y <- 0.75^2*var.X; var.Y

set.seed(12)
y1 <- 0.75*x1-1.5
sim.ben <- sample(y1,10000,replace=T,prob=f1)
fi <- table(sim.ben)/length(sim.ben) 
xb <- barplot(fi)
lines(xb, f1, type="h", col="red", lwd=3)  
points(xb, f1, col="red", lwd=10)


mean.sim.ben <- mean(sim.ben); mean.sim.ben

var.sim.ben <- var(sim.ben); var.sim.ben




