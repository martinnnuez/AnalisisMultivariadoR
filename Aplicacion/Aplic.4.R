# Sumas de cuadrados

# suma de cuadrados dentro de 2 grupos:
G1=as.matrix(subset(Datos[,2:8],Datos$h2==1))#Datos grupo 1 
G2=as.matrix(subset(Datos[,2:8],Datos$h2==2))#Datos grupo 2

SCD1= sum(diag(var(G1))*(nrow(G1)-1))#SCDgrupo1
SCD2= sum(diag(var(G2))*(nrow(G2)-1))#SCDgrupo2
SCDG2=SCD1+SCD2;SCDG2#SCDtotal W

# suma de cauadrados total:
S=cov(datos)
SCT= sum(diag(var(datos))*(nrow(datos)-1))

# Suma de cuadrados entre:
SCB1=SCT-SCD1
SCB2=SCT-SCD2


# Criterio Calsinli y Harabaz:
#MATRIZ DE COVARIANZAS#
S=cov(d)
T=(n-1)*S
T

#SEPARACION DE LOS DOS GRUPOS#
d1=d[1:(n1),1:p]
d2=d[((n2)+1):n,1:p]
m1=colMeans(d1)

#MATRIZ DE COVARIANZAS COMBINADA#
c1=cov(d1)
c2=cov(d2)
Sp=((n1-1)*c1+(n2-1)*c2)/(n-2)
Sp

# SCD
W=(n-g)* Sp
W

# SCE
B =T-W
B

# Coeficiete:
n= 26 # cantidad de datos
g=2 # cantidad de grupos
CH=(sum(diag(B)))*(n-g)/((sum(diag(W)))*(g-1))

# Criterio R2
1-(W/T)
1-(sum(diag(W))/sum(diag(T)))


# COEFICIENTE SILUETA:
# ÍNDICES SILUETA KMEANS
# ==============================================================================

silhouette_kmeans <- function(n_clusters, datos, iter.max=1000, nstart=50){
  # Esta función aplica el algoritmo kmeans y devuelve la media del índice silueta
  if (n_clusters == 1) {
    # Para n_clusters = 1, el indice silueta es 0
    media_silhouette <- 0
  }else {
    cluster_kmeans <- kmeans(centers = n_clusters, x = datos, iter.max = iter.max,
                             nstart = nstart)
    valores_silhouette <- cluster::silhouette(cluster_kmeans$cluster,
                                              get_dist(x = datos, method = "euclidean"))
    media_silhouette <- summary(valores_silhouette)[[4]]
    return(media_silhouette)
  }
  
}

datos <- scale(USArrests)
valores_medios_silhouette <- map_dbl(.x = 1:15,
                                     .f = silhouette_kmeans,
                                     datos = datos)

data.frame(n_clusters = 1:15, media_silhouette = valores_medios_silhouette) %>%
  ggplot(aes(x = n_clusters, y = media_silhouette)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2:15) +
  theme_bw()


# ÍNDICES SILUETA HIERARCHICAL CLUSTERING
# ==============================================================================

custom_silhouette <- function(n_clusters, dendograma, distancia, datos){
  # Esta función calcula el indice silueta medio de un dendograma
  # para un determinado número de clusters.
  set.seed(123)
  valores_silhouette <- cluster::silhouette(stats::cutree(dendograma,
                                                          k = n_clusters),
                                            get_dist(x = datos, method = distancia))
  media_silhouette <- summary(valores_silhouette)[[4]]
  return(media_silhouette)
}

datos <- scale(USArrests)
hc_euclidea_completo <- hclust(d = dist(x = datos, method = "euclidean"),
                               method = "complete")
valores_medios_silhouette <- map_dbl(.x = 2:15,
                                     .f = custom_silhouette,
                                     dendograma = hc_euclidea_completo,
                                     distancia = "euclidean",
                                     datos = datos)

data.frame(n_clusters = 2:15, media_silhouette = valores_medios_silhouette) %>%
  ggplot(aes(x = n_clusters, y = media_silhouette)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2:15) +
  theme_bw()


#FUNCION LINEAL, CUADRATICA Y REGULARIZADA#
p=7
dt=dat_fun[,2:8]
d1t=dt[1:(n/2),1:p]
d2t=dt[((n/2)+1):n,1:p]
m1t=colMeans(d1t)
m2t=colMeans(d2t)
dmt=m1t-m2t
c1t=cov(d1t)
c2t=cov(d2t)
Spt=((n/2-1)*c1t+(n/2-1)*c2t)/(n-2)
iSpt=solve(Spt)

#FUNCIONES DISCRIMINANTES CON DATOS TRANSFORMADOS#
#DISCRIMINANTE LINEAL#
#CONSTANTES#
alin1=-0.5*m1t%*%iSpt%*%m1t
alin2=-0.5*m2t%*%iSpt%*%m2t
#COEFICIENTES#
blin1=m1t%*%iSpt
blin2=m2t%*%iSpt
alin=alin1-alin2
blin=blin1-blin2
alin;blin

#FUNCIONES CUADRATICAS#
i1t=solve(c1t)
i2t=solve(c2t)
det1t=det(i1t)
det2t=det(i2t)
#CONSTANTES#
acua1=-0.5*((m1t%*%i1t%*%m1t)+log(det1t))
acua2=-0.5*((m2t%*%i2t%*%m2t)+log(det2t))
acua=acua1-acua2
#TERMINOS LINEALES #
bcua1=m1t%*%i1t
bcua2=m2t%*%i2t
bcua=bcua1-bcua2
#TERMINOS CUADRATICOS #
ccua1=-0.5*i1t
ccua2=-0.5*i2t
ccua=ccua1-ccua2
acua;bcua;ccua

# DISCRIMINANTE REGULARIZADO: FIJANDO VALORES PARA LOS PAR?METROS#
library(klaR)
disl.rda=rda(CONDICIO~.,dat_fun, lambda=1, gamma=0, crossval = TRUE)
names(disl.rda)
disl.rda$regularization
disl.rda$error.rate


######################## SELECCION METODO JERARQUICO PARA AGRUPAR:
#detalle del agrupamiento
detalle=cbind(HClust.1$merge, HClust.1$height); detalle[1:20,]
d1<- dist(Datos[,2:8])

d2 <-cophenetic(HClust.1)
(as.matrix(d2))[1:10,1:10]
cor(d1,d2) # Mayor da mejor metodo

apply (Datos[,2:8],2,mean)

#Resumen de los 2 grupos
summary(as.factor(cutree(HClust.1, k = 2)))# Cluster Sizes

####################### SELECCION NUMERO DE GRUPOS:
## Generar una variable grupo(con dos grupos)  h2
Datos$h2 <- c(as.factor(cutree(HClust.1, k = 2)))
biplot(princomp(model.matrix(~-1+ANTEMP+ANTIG+EDAD+EDUCAC+HORASEM+INGRFLIA+INGRPER,Datos)),
       xlabs=as.character(cutree(HClust.1,k=2)))

# suma de cuadrados dentro de 2 grupos
G1=as.matrix(subset(Datos[,2:8],Datos$h2==1))
apply(G1,2,mean);dim(G1)
G2=as.matrix(subset(Datos[,2:8],Datos$h2==2))
apply(G2,2,mean);dim(G2)
SCD1= sum(diag(var(G1))*(nrow(G1)-1))
SCD2= sum(diag(var(G2))*(nrow(G2)-1))
SCDG2=SCD1+SCD2;SCDG2

#Resumen de los 3 grupos
#Generar las variable de los grupos 3 (h3) y 4 (h4) variables
summary(as.factor(cutree(HClust.1, k = 3)))# Cluster Sizes

## Generar una variable grupo(con tres grupos)  h3
Datos$h3 <- c(as.factor(cutree(HClust.1, k = 3)))
biplot(princomp(model.matrix(~-1+ANTEMP+ANTIG+EDAD+EDUCAC+HORASEM+INGRFLIA+INGRPER,Datos)),
       xlabs=as.character(cutree(HClust.1,k=3)))

# suma de cuadrados dentro de 3 grupos
G1=as.matrix(subset(Datos[,2:8],Datos$h3==1))
apply(G1,2,mean)
dim(G1)
G2=as.matrix(subset(Datos[,2:8],Datos$h3==2))
apply(G2,2,mean)
dim(G2)
G3=as.matrix(subset(Datos[,2:8],Datos$h3==3))
apply(G3,2,mean);dim(G3)
SCD1= sum(diag(var(G1))*(nrow(G1)-1))
SCD2= sum(diag(var(G2))*(nrow(G2)-1))
SCD3= sum(diag(var(G3))*(nrow(G3)-1))
SCD1;SCD2;SCD3
SCDG3=SCD1+SCD2+SCD3; SCDG3


#Resumen de los 4 grupos
summary(as.factor(cutree(HClust.1, k = 4)))# Cluster Sizes

## Generar una variable grupo(con 4 grupos)  h4
Datos$h4 <- c(as.factor(cutree(HClust.1, k = 4)))
biplot(princomp(model.matrix(~-1+ANTEMP+ANTIG+EDAD+EDUCAC+HORASEM+INGRFLIA+INGRPER,Datos)),
       xlabs=as.character(cutree(HClust.1,k=4)))


# suma de cuadrados dentre de 4 grupos
G1=as.matrix(subset(Datos[,2:8],Datos$h4==1))
dim(G1)
G2=as.matrix(subset(Datos[,2:8],Datos$h4==2))
dim(G2)
G3=as.matrix(subset(Datos[,2:8],Datos$h4==3))
dim(G3)
G4=as.matrix(subset(Datos[,2:8],Datos$h4==4))
dim(G4)
SCD1= sum(diag(var(G1))*(nrow(G1))-1)
SCD2= sum(diag(var(G2))*(nrow(G2))-1)
SCD3= sum(diag(var(G3))*(nrow(G3))-1)
SCD4= sum(diag(var(G4))*(nrow(G4))-1)
SCD1;SCD2;SCD3;SCD4
SCDG4=SCD1+SCD2+SCD3+SCD4; SCDG4

#PARA COMPARAR SUMAS DE CUADRADOS

#calculamos el valor F =(SCDT(k grupos)-SCDT (k+1 grupos))/(SCDT(k+1 grupos)/n-k-1)
# de 2 a 3 grupos
F=(SCDG2-SCDG3)/(SCDG3/(100-2-1))
F

# de 3 a 4 grupos
F=(SCDG3-SCDG4)/(SCDG4/(100-3-1))
F

###actividad 
#seleccionar la cantidad de conglomerados con otra medida diferente a la de Hartigan.

# Otros metodos:
# R2
S= cov(Datos[,2:8])
SCT=sum(diag(S))*(nrow(Datos)-1);SCT

# suma de cuadrados dentro de 2 grupos
G1=as.matrix(subset(Datos[,2:8],Datos$h2==1))
G2=as.matrix(subset(Datos[,2:8],Datos$h2==2))
SCD1= sum(diag(var(G1))*(nrow(G1)-1))
SCD2= sum(diag(var(G2))*(nrow(G2)-1))
SCDG2=SCD1+SCD2;SCDG2
R2=1-(SCDG2/SCT);R2

# suma de cuadrados dentro de 3 grupos
G1=as.matrix(subset(Datos[,2:8],Datos$h3==1))
G2=as.matrix(subset(Datos[,2:8],Datos$h3==2))
G3=as.matrix(subset(Datos[,2:8],Datos$h3==3))
SCD1= sum(diag(var(G1))*(nrow(G1)-1))
SCD2= sum(diag(var(G2))*(nrow(G2)-1))
SCD3= sum(diag(var(G3))*(nrow(G3)-1))
SCD1;SCD2;SCD3
SCDG3=SCD1+SCD2+SCD3; SCDG3
R3=1-(SCDG3/SCT);R3

# suma de cuadrados dentro de 3 grupos
G1=as.matrix(subset(Datos[,2:8],Datos$h4==1))
G2=as.matrix(subset(Datos[,2:8],Datos$h4==2))
G3=as.matrix(subset(Datos[,2:8],Datos$h4==3))
G4=as.matrix(subset(Datos[,2:8],Datos$h4==4))
SCDG4=SCD1+SCD2+SCD3+SCD4; SCDG4
R4=1-(SCDG4/SCT);R4

## Analisis resultados
R2;R3;R4
# Optamos quedarnos con 3 grupos.

# Metodo CH
n=nrow(Datos)
CH2=((SCDG2-SCT)/SCDG2)*((n-2)/(2-1))
CH3=((SCDG3-SCT)/SCDG3)*((n-3)/(3-1))
CH4=((SCDG4-SCT)/SCDG4)*((n-4)/(4-1))
CH2;CH3;CH4
# El mayor es el de 4 grupos.