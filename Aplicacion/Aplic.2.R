### clustering
# F = (SCDG(g) - SCDG(g+1)) / (SCDG(g+1)/(n-g-1))
calcularF <- function(SCg1, SCg2, numeroGruposDelg1, ntotal) {
    resF <- (SCg1-SCg2) / (SCg2/(ntotal-numeroGruposDelg1-1));
    return(resF);
}
# si es > 10 entonces deberia quedarme con g+1 grupos

### Discriminante lineal a mano
m1t <- c(5,4.8);
m2t <- c(9,3);
Sp <- matrix(c(4.764,1.001,1.001,3.259), ncol=2, byrow=!F);
iSpt <- solve(Sp);

alin1 <- -0.5*m1t %*% iSpt %*% m1t; # Constante del L1
alin2 <- -0.5*m2t %*% iSpt %*% m2t;

blin1 <- m1t %*% iSpt; # Pendiente del L1
blin2 <- m2t %*% iSpt;

alin <- alin1-alin2 # -((m1t-m2t) %*% iSpt %*% (m1t+m2t)) / 2
blin <- blin1-blin2 # (m1t-m2t) %*% iSpt
alin; blin; # coeficientes finales.

# discriminante lineal para cada sujeto
solicitante1 <- c(6,5);
solicitante2 <- c(8,4);

n <- 70;
nG1 <- 35; nG2 <- 35;
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

### clustering2
distancias <- matrix(0, ncol=5, nrow=5);
distancias[lower.tri(distancias)] <- c(32,180,241,841,68,121,1105,13,1369,1314);

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


