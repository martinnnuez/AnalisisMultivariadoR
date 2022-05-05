### clustering2
rm(list=ls())
distancias <- matrix(0, ncol=5, nrow=5);
distancias[upper.tri(distancias)] <- c(3.1,2.4,3.4,3.3,2.1,3.1,2.6,3.6,.4,3.1);
distancias <- t(distancias);

cluster <- hclust(as.dist(distancias), method="average");

cluster$merge; # orden cuando se unieron
cluster$height; # alturas a las cuales se unieron

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

### clustering
rm(list=ls())
# F = (SCDG(g) - SCDG(g+1)) / (SCDG(g+1)/(n-g-1))
calcularF <- function(SCg1, SCg2, numeroGruposDelg1, ntotal) {
    resF <- (SCg1-SCg2) / (SCg2/(ntotal-numeroGruposDelg1-1));
    return(resF);
}
# si es > 10 entonces deberia quedarme con g+1 grupos

n <- 26;

SCG2 <- 63.35+79.49;SCG2
SCG3 <- 63.35+43.94+26.99;SCG3
SCG4 <- 9.36+29.95+27.46+29.07;SCG4

calcularF(SCG2, SCG3, 2, n);
(SCG2 - SCG3)/(SCG3/23)
calcularF(SCG3, SCG4, 3, n);
(SCG3 - SCG4)/(SCG4/22)

### Discriminante lineal a mano
rm(list=ls());
Sp <- matrix(c(4.764,1.001,1.001,3.259), ncol=2, byrow=!F);

n <- 70;
nG1 <- 35; nG2 <- 35;

m1t <- c(5, 4.8); # GRUPO 1 es el Cumplidor
m2t <- c(9, 3);
Sp <- matrix(c(4.764,1.001,1.001,3.259), ncol=2, byrow=!F);
iSpt <- solve(Sp);

alin1 <- -0.5*m1t %*% iSpt %*% m1t; # Constante del L1
alin2 <- -0.5*m2t %*% iSpt %*% m2t;

alin1 <- -5.876;
alin2 <- -9.396;

blin1 <- m1t %*% iSpt; # Pendiente del L1
blin2 <- m2t %*% iSpt;

blin1 <- c(0.777, 1.296);
blin2 <- c(1.813, .364);

alin <- alin1-alin2 # -((m1t-m2t) %*% iSpt %*% (m1t+m2t)) / 2
blin <- blin1-blin2 # (m1t-m2t) %*% iSpt
alin; blin; # coeficientes finales.

# discriminante lineal para cada sujeto
solicitante1 <- c(6,5);
solicitante2 <- c(8,4);

pi1 <- nG1 / n; pi2 <- nG2 / n;
critico <- log(pi2/pi1); critico;
alin + blin %*% solicitante1; # como esto es mayor a critico entonces pertenece a G1
alin + blin %*% solicitante2; # como esto es menor a critico entonces pertenece a G2

# prob a priori de los solicitantes
d <- rbind(solicitante1, solicitante2);
DM1 <- mahalanobis(d, center=m1t, cov=Sp, inverted = FALSE); DM1
DM2 <- mahalanobis(d, center=m2t, cov=Sp, inverted = FALSE); DM2

prY1x <- pi1*exp(-0.5*DM1^2)/(pi1*exp(-0.5*DM1^2)+pi2*exp(-0.5*DM2^2)); # esto es lo que pide
# probGroups <- as.numeric(prY1x < 0.5)+1;











