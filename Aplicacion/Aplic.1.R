# EJERCICIO 1:

### clustering2
rm(list=ls())
distancias <- matrix(0, ncol=5, nrow=5);
distancias[upper.tri(distancias)] <- c(3.1,2.4,3.4,3.3,2.1,3.1,2.6,3.6,.4,3.1);
distancias <- t(distancias);

# Metodo average
cluster <- hclust(as.dist(distancias), method="average");
# La matriz de distancias que le cargo es una matriz triangular inferior, igual que la que devuelve dist()

#"ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).

# a) Sucesion de agrupaciones con el metodo del agrupamiento promedio:
cluster$merge; # orden cuando se unieron
cluster$height; # alturas a las cuales se unieron

cbind(cluster$merge, cluster$height)

# b) Construya un diagrama de arbol jerarquico para la agrupaciones obtenidas 
dendograma <- plot(cluster,main="Cluster Dendrogram",
                   xlab="Observation Number in Data Set",sub="Method=Average;Distance=Euclidian")

# Calcular distancias con los distintos links:
# solo para link promedio
distanciaCalc <- function(dis, g1, g2, enlace="promedio") {
	if(enlace=="promedio"){
		aa <- sum(unlist(lapply(g1, function(x) lapply(g2, function(y) max(distancias[x,y],distancias[y,x])))))/(length(g1)*length(g2));
	} else if(enlace=="min") {
		aa <- min(unlist(lapply(g1, function(x) lapply(g2, function(y) max(distancias[x,y],distancias[y,x])))));
	} else if(enlace=="max"){
		aa <- max(unlist(lapply(g1, function(x) lapply(g2, function(y) max(distancias[x,y],distancias[y,x])))));
	}
	return(aa);	
}

# distancias entre grupos
distanciaCalc(distancias, list(1,2), list(3,4,5), "promedio"); # se usa asi
# distanciaCalc(distancias, list(1), list(3,5), "promedio"); # funciona perfecto

# x = 3
# y = 5
# b = max(distancias[x,y],distancias[y,x])/(length(x)*length(y))
# El maximo de las distiancias al principio tiene que ver con como cargue la matriz de distancias, si es matriz triangular superior o inferior.

# a) a traves coeficiente cofenetico
# Necesito la matriz de distancias:
dist1<- as.dist(distancias)
dist2<- cophenetic(cluster)
cor(dist1,dist2)
# 0.985 posee un coeficiente cofenetico muy elevado por lo que se asume que presenta una baja distoricion en las relaciones originales existentes entre los elementos en estudio.
# Se eljige el metodo que mayor coeficiente presenta.

# EJERCICIO 2:

### clustering
rm(list=ls())
# a) a traves de F criterio de Hartigan.
# F = (SCDG(g) - SCDG(g+1)) / (SCDG(g+1)/(n-g-1))
#calculamos el valor F =(SCDT(k grupos) - SCDT(k+1 grupos))/(SCDT(k+1 grupos) / n-k-1)
calcularF <- function(SCg1, SCg2, numeroGruposDelg1, ntotal) {
    resF <- (SCg1-SCg2) / (SCg2/(ntotal-numeroGruposDelg1-1));
    return(resF);
}
# si es > 10 entonces deberia quedarme con g+1 grupos
# Los parametros que ingresan a la funcion son = SCDT(k grupos); SCDT(k+1 grupos); k ; n.

# Resolucion
# n
n <- 26;

# Sumas de cuadrados dentro totales W para cada k distinto de grupos:
SCG2 <- 63.35+79.49;SCG2
SCG3 <- 63.35+43.94+26.99;SCG3
SCG4 <- 9.36+29.95+27.46+29.07;SCG4

# F entre 2 grupos y 3 grupos: k=2:
calcularF(SCG2, SCG3, 2, n);
(SCG2 - SCG3)/(SCG3/(n-2-1))

# F entre 3 grupos y 4 grupos: k=3:
calcularF(SCG3, SCG4, 3, n);
(SCG3 - SCG4)/(SCG4/(n-3-1))

# Cuando F>10 sugiere quedarnos con un grupo mas, en este caso nos quedariamos con 2 grupos como consecuencia de que F<10 y no tiene sentido dividir en grupos.

# a) a traves de R^2
# R^2 = 1- W/T
# Para este metodo necesito los datos para poder calcular la matriz de varianzas y covarianzas.
# Para 2 grupos:
# Matriz de varainzas y covarianzas:
S=cov(datos)
n = 26
T=(n-1)*S
#Spooled
c1=cov(datos1)
c2=cov(datos2)
Sp=((n1-1)*c1+(n2-1)*c1)/(n-2)
W=(n-2)*Sp
B=T-W
# Para 3 grupos:
# Para 4 grupos:

# a) a traves coeficiente cofenetico
# Necesito la matriz de distancias:
distancia<- dist(datos)
dist2<- cophenetic(cluster)
cor(distancias,dist2)

# EJERCICIO 3:

# a) Expresion analitica de los coeficientes y vector de coeficientes estimados LDA:

### Discriminante lineal a mano
rm(list=ls());
# Matriz de varianzas y covrianzas combinada:
Sp <- matrix(c(4.764,1.001,1.001,3.259), ncol=2, byrow=T);
iSpt <- solve(Sp); #inversa

# Numero de observaciones por grupo:
n <- 70;
nG1 <- 35; nG2 <- 35;

# Medias por grupo
m1t <- c(5, 4.8); # GRUPO 1 es el Cumplidor
m2t <- c(9, 3);

# Proporcion en cada grupos:
pg1=nG1/n
pg2=nG2/n

# Termino constante
alin1 <- -0.5*m1t %*% iSpt %*% m1t + (log(pg1)); # Constante del L1
alin2 <- -0.5*m2t %*% iSpt %*% m2t + (log(pg2));
# Para la profe va sin el ln(prop)
alin1 <- -0.5*m1t %*% iSpt %*% m1t;alin1 # Constante del L1
alin2 <- -0.5*m2t %*% iSpt %*% m2t;alin2
# Valores de dato:
alin1 <- -5.876;
alin2 <- -9.396;

# Pendiente
blin1 <- m1t %*% iSpt; # Pendiente del L1
blin2 <- m2t %*% iSpt;
# Valores de dato:
blin1 <- c(0.777, 1.296);
blin2 <- c(1.813, 0.364);

# Coeficientes de la expresion analitica:
# PREGUNTAR O VER SI LOS COEFICIENTES QUE ME DA ELLA EN LA CONSTANTE TIENEN INCLUIDO EL LOG PROP. Y DEBO RESTARLO
alin <- (alin1-log(pg1))-(alin2-log(pg2)) # -((m1t-m2t) %*% iSpt %*% (m1t+m2t)) / 2
alin <- (alin1-alin2)
blin <- (blin1-blin2) # (m1t-m2t) %*% iSpt
alin; blin; # coeficientes finales.

# Discriminante lineal para cada sujeto
solicitante1 <- c(6,5);
solicitante2 <- c(8,4);

pi1 <- nG1 / n; pi2 <- nG2 / n;
# Con quien comparo para decidir:
critico <- log(pi2/pi1); critico;
# Aplico regla de desicion a los X (solicitante 1 y 2):
alin + blin %*% solicitante1; # como esto es mayor a critico entonces pertenece a G1
alin + blin %*% solicitante2; # como esto es menor a critico entonces pertenece a G2

# Decido:
(alin + blin %*% solicitante1)>critico; # como esto es mayor a critico entonces pertenece a G1. G1= cumplidor.
(alin + blin %*% solicitante2)>critico; # como esto es menor a critico entonces pertenece a G2. G2= no cumplidor.

# b) Prob a priori de los solicitantes
# Invento solicitante 3 
#solicitante3<-c(15,7)
d <- rbind(solicitante1, solicitante2);
DM1 <- mahalanobis(d, center=m1t, cov=Sp, inverted = FALSE); DM1 # Distancia de mahalanobis al vector de medias grupo 1.
DM2 <- mahalanobis(d, center=m2t, cov=Sp, inverted = FALSE); DM2 # Distancia de mahalanobis al vector de medias grupo 2.

# Para mi es asi pq mahalanobis ya devuelve distancia al cuadrado
prY1_x <- pg1*exp(-0.5*DM1)/(pg1*exp(-0.5*DM1)+pg2*exp(-0.5*DM2));prY1_x
# Probabilidad de que los solicitantes pertenezcan al grupo 1 dado los valores de x

prY2_x <- pg2*exp(-0.5*DM2)/(pg1*exp(-0.5*DM1)+pg2*exp(-0.5*DM2));prY2_x
# Probabilidad de que los solicitantes pertenezcan al grupo 2 dado los valores de x


#prY1x <- pi1*exp(-0.5*DM1^2)/(pi1*exp(-0.5*DM1^2)+pi2*exp(-0.5*DM2^2)); # esto es lo que pide
#probGroups <- as.numeric(prY1x < 0.5)+1;


# a) Expresion analitica de los coeficientes y vector de coeficientes estimados QDA:

# Discriminante cuadratico a mano:
# Matrices de varianzas y covrianzas:

S1 <- matrix(c(4.764,1.001,1.001,3.259), ncol=2, byrow=T);
S2 <- matrix(c(3.5,2.4,3.2,2.2), ncol=2, byrow=T);
iS1 <- solve(S1); #inversa
iS2 <- solve(S2); #inversa

# Numero de observaciones por grupo:
n <- 70;
nG1 <- 35; nG2 <- 35;

# Medias por grupo
m1t <- c(5, 4.8); # GRUPO 1 es el Cumplidor
m2t <- c(9, 3);

# Proporcion en cada grupos:
pg1=nG1/n
pg2=nG2/n

# Termino constante 1:
cte1= 0.5*log(det(S1)/det(S2));cte1

# Termino constante 2:
cte2= -0.5*((t(m1t)%*%iS1%*%m1t)-(t(m2t)%*%iS2%*%m2t));cte2

# Termino lineal:
b1= -2*t(x)%*%(iS1%*%m1t-iS2%*%m2t)

# Termino cuadratico:
b2= -0.5*t(x)%*%(iS1-iS2)%*%(x-mit)%*%x

# Se clasifica al grupo que mas grande da:
sol1 <- matrix(c(6,5),ncol=1,byrow=T);
sol2 <- c(8,4);

# La otra forma que parece es la correcta, segun filimina para dos grupos:
# Se clasifica al grupo que mas grande da:
### sol1
Q1=log(pi1)-0.5*log(det(S1))-(0.5*t(sol1-m1t)%*%(iS1)%*%(sol1-m1t));Q1
Q2=log(pi2)-0.5*log(det(S2))-(0.5*t(sol1-m2t)%*%(iS2)%*%(sol1-m2t));Q2
max(Q1,Q2) # El maximo es Q1 entonces asigno al grupo 1.
### sol2
Q1=log(pi1)-0.5*log(det(S1))-(0.5*t(sol2-m1t)%*%(iS1)%*%(sol2-m1t));Q1
Q2=log(pi2)-0.5*log(det(S2))-(0.5*t(sol2-m2t)%*%(iS2)%*%(sol2-m2t));Q2
max(Q1,Q2) # El maximo es Q1 entonces asigno al grupo 1.


# Segun el libro Gareth:
Q1=log(pi1)-(0.5*t(sol1-m1t)%*%(iS1)%*%(sol1-m1t));Q1
Q2=log(pi2)-(0.5*t(sol1-m2t)%*%(iS2)%*%(sol1-m2t));Q2
max(Q1,Q2) # El maximo es Q1 entonces asigno al grupo 1.

Q1=log(pi1)-(0.5*t(sol2-m1t)%*%(iS1)%*%(sol2-m1t));Q1
Q2=log(pi2)-(0.5*t(sol2-m2t)%*%(iS2)%*%(sol2-m2t));Q2
max(Q1,Q2) # El maximo es Q1 entonces asigno al grupo 1.


# Forma en que resta los Q (filmina):
# Debo reemplazar x y mit
### solicitante 1
Q11= (-0.5*t(sol1)%*%(iS1-iS2)%*%((sol1-m1t)*sol1))+(-2*t(sol1)%*%(iS1%*%m1t-iS2%*%m2t))+cte1+cte2;Q11
# No me cierra dimensionalmente voy a hacer de forma separada
Q21= (-0.5*t(sol1)%*%(iS1-iS2)%*%((sol1-m2t)*sol1))+(-2*t(sol1)%*%(iS1%*%m1t-iS2%*%m2t))+cte1+cte2;Q21
Q21>Q11 # asigno al grupo 2
critico <- log(pi2/pi1); critico;

Q11>critico
Q21>critico

### solicitante 2
Q11= (-0.5*t(sol2)%*%(iS1-iS2)%*%((sol2-m1t)*sol2))+(-2*t(sol2)%*%(iS1%*%m1t-iS2%*%m2t))+cte1+cte2;Q11
# No me cierra dimensionalmente voy a hacer de forma separada
Q21= (-0.5*t(sol2)%*%(iS1-iS2)%*%((sol2-m2t)*sol2))+(-2*t(sol2)%*%(iS1%*%m1t-iS2%*%m2t))+cte1+cte2;Q21
Q21>Q11 # asigno al grupo 1


# a) FDCL

# Funcion discriminante canonica lineal:
#MATRIZ DE COVARIANZAS COMBINADA#
n=26;n1=12;n2=14;g=2
m1=m1t
m2=m2t

c1 <- matrix(c(4.764,1.001,1.001,3.259), ncol=2, byrow=T);
c2 <- matrix(c(3.5,2.4,3.2,2.2), ncol=2, byrow=T);
Sp=((n1-1)*c1+(n2-1)*c2)/(n-2);Sp
W=(n-g)* Sp;W
B =T-W;B

#DISCRIMINANTE CANÓNICO utilizando la función eigen#
e=eigen(solve(W)%*%B);e

e1=as.matrix(e$vectors[,1]);e1
fc=(t(e1)%*%B%*%e1)/(t(e1)%*%W%*%e1);fc

###Para determinar el punto de corte
D1=t(e1)%*%m1;D1

D2=t(e1)%*%m2;D2
C=(D1+D2)/2;C
Di=as.matrix(datos)%*%e1

# VER LA RELACION DE LAS MEDIAS Y PONER EL CRITERIO COMO DEBE ESTAR.
yfit=c()
for (i in 1:length(datos$CONDICIO)) 
  if (Di[i]>C) yfit[i]=1 else yfit[i]=2 
####OJO el criterio esta al revés del teórico porque la media del grupo 2 es menor a la del grupo 1
table(yfit,datos$CONDICIO)
cbind(datos$CONDICIO,yfit)



# Punto 4(20 puntos)
# Para un conjunto de 26 observaciones y 6 variables se realizaron agrupaciones jerárquicas
# utilizando distintos métodos, representadas en los siguientes dendongramas.
# Distancia euclidea, enlace(simple, completo, promedio,centroide) y ward
# 1.¿Con qué método se obtuvo el mejor agrupamiento de los datos?
# 2.Describa brevemente el método seleccionado.
# 3.Utilizando el dendograma ¿Cuántos grupos seleccionaría?¿Por qué?
