##### QUEST?O 2 #####
### Alternativa C ###

## AGRUPAMENTO - INDIVIDUOS ##

tabela <- read.table("dados_prova1.txt", header = T)
tabela

dados <- tabela[,2:10]   

row.names(dados) = tabela[,1]
dados

attach(dados)
names(dados)

disteuclid <- dist(dados)
disteuclid
print(disteuclid, digits = 3)

## C?lculo dos Coeficientes Cor. Cofen?ticos para escolha dos m?todos ##

## LIGA??O SIMPLES (Single Linkage) - VIZINHO MAIS PR?XIMO ##
cluster_single <- hclust(disteuclid, "single")
cluster_single
help(hclust)
d1 <- cophenetic(cluster_single)
cor1 <- cor(disteuclid,d1)              ## coeficiente de correla??o cofen?tica ##

## LIGA??O COMPLETA (Complete Linkage) - VIZINHO MAIS DISTANTE ##
cluster_complete <- hclust(disteuclid, "complete")
cluster_complete
d2 <- cophenetic(cluster_complete)
cor2 <- cor(disteuclid,d2)              ## coeficiente de correla??o cofen?tica ##

## LIGA??O M?DIA (Average Linkage) ##
cluster_average <- hclust(disteuclid, "average")
cluster_average
d3 <- cophenetic(cluster_average)
cor3 <- cor(disteuclid,d3)              ## coeficiente de correla??o cofen?tica ##

## CENTR?IDE ##
cluster_centroid <- hclust(disteuclid, "centroid")
cluster_centroid
d4 <- cophenetic(cluster_centroid)
cor4 <- cor(disteuclid,d4)              ## coeficiente de correla??o cofen?tica ##

## WARD ##
cluster_ward <- hclust(disteuclid, "ward")
cluster_ward
d5 <- cophenetic(cluster_ward)
cor5 <- cor(disteuclid,d5)              ## coeficiente de correla??o cofen?tica ##

ccc <- c(cor1, cor2, cor3, cor4, cor5)
cbind(ccc)		## M?todos escolhidos: Liga??o M?dia e Centr?ide ##


	## DENDOGRAMAS ##

## LIGA??O M?DIA ##

cluster_average$height       ## valores de jun??o no dendograma (maior salto)##
## definir numero de clusters ##
## h>17,61
## k=4
plot(cluster_average, xlab = "Parcelas", ylab = "Dist?ncia Euclidiana", main = "", 
hang = -1)
rect.hclust(cluster_average, k=4)

## classifica os elementos em cada grupo ##
c <- cutree(cluster_average, k = 4)     
## diagrama de dispers?o a cada 2 vari?veis, identificando os grupos ##             
plot(dados, col = c)  


## CENTR?IDE ##

cluster_centroid$height       ## valores de jun??o no dendograma (maior salto)##
## definir numero de clusters ##
## h>10,86
## k= 3
plot(cluster_centroid, xlab = "Parcelas", ylab = "Dist?ncia Euclidiana", main = "", 
hang = -1)
rect.hclust(cluster_centroid, k=3)

  
## M?TODO K-M?DIAS(K-Means) - N?o Hier?rquico ##

## N?mero de grupos ##
help(fviz_nbclust)
library("factoextra")
library("NbClust")
## Hubert and D index graphical method ##
nb <- NbClust(dados, distance = "euclidean", min.nc = 2,
max.nc = 8, method = "kmeans")
fviz_nbclust(nb)

# Elbow method for kmeans
fviz_nbclust(dados, kmeans, method = "wss") +
geom_vline(xintercept = 2, linetype = 2) +
geom_vline(xintercept = 3, linetype = 2) 

## k=2 ##

kmedias <- kmeans(dados,2)        ## m?todo kmeans ##
kmedias
kmedias$size
kmedias$cluster
cbind(kmedias$cluster)

require(cluster)
clusplot(dados, kmedias$cluster, color=T, shade=T, labels=2, 
lines=0,cex.txt=0.8, main="")
help(clusplot)
