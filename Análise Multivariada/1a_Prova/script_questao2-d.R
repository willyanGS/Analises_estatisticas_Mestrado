##### QUEST?O 2 #####
### Alternativa D ###

## AGRUPAMENTO - VARI?VEIS ##

tabela <- read.table("dados_prova1.txt", header = T)
tabela

dados <- tabela[,2:10]   
dados

row.names(dados) = tabela[,1]
dados

attach(dados)
names(dados)

## matriz de correla??o Pearson ##
R <- cor(dados)
R                              

## matriz de distancia Rencher ##
r <- as.dist((1-(R^2)))  ## SUGERIDA POR RENCHER (2002) ##
r

## C?lculo dos Coeficientes Cor. Cofen?ticos para escolha dos m?todos ##

## LIGA??O SIMPLES (Single Linkage) - VIZINHO MAIS PR?XIMO ##
cluster_single <- hclust(r, "single")
cluster_single
help(hclust)
d1 <- cophenetic(cluster_single)
cor1 <- cor(r,d1)              ## coeficiente de correla??o cofen?tica ##

## LIGA??O COMPLETA (Complete Linkage) - VIZINHO MAIS DISTANTE ##
cluster_complete <- hclust(r, "complete")
cluster_complete
d2 <- cophenetic(cluster_complete)
cor2 <- cor(r,d2)              ## coeficiente de correla??o cofen?tica ##

## LIGA??O M?DIA (Average Linkage) ##
cluster_average <- hclust(r, "average")
cluster_average
d3 <- cophenetic(cluster_average)
cor3 <- cor(r,d3)              ## coeficiente de correla??o cofen?tica ##

## CENTR?IDE ##
cluster_centroid <- hclust(r, "centroid")
cluster_centroid
d4 <- cophenetic(cluster_centroid)
cor4 <- cor(r,d4)              ## coeficiente de correla??o cofen?tica ##

## WARD ##
cluster_ward <- hclust(r, "ward")
cluster_ward
d5 <- cophenetic(cluster_ward)
cor5 <- cor(r,d5)              ## coeficiente de correla??o cofen?tica ##

ccc <- c(cor1, cor2, cor3, cor4, cor5)
cbind(ccc)		## M?todos escolhidos: Liga??o M?dia e Ward ##


	## DENDOGRAMAS ##

## LIGA??O M?DIA ##

cluster_average$height       ## valores de jun??o no dendograma (maior salto)##
## definir numero de clusters ##
## h>0,64
## k=6
plot(cluster_average, xlab = "Parcelas", ylab = "Dist?ncia Euclidiana", main = "", 
hang = -1)
rect.hclust(cluster_average, k=6)

## classifica os elementos em cada grupo ##
c <- cutree(cluster_average, k = 6)     
## diagrama de dispers?o a cada 2 vari?veis, identificando os grupos ##             
plot(dados, col = c)  

## LIGA??O SIMPLES ##

cluster_single$height       ## valores de jun??o no dendograma (maior salto)##
## definir numero de clusters ##
## h>0,72
## k= 2
plot(cluster_single, xlab = "Parcelas", ylab = "Dist?ncia Euclidiana", main = "", 
hang = -1)
rect.hclust(cluster_single, k=2)


## WARD ##

cluster_ward$height       ## valores de jun??o no dendograma (maior salto)##
## definir numero de clusters ##
## h>0,99
## k= 2
plot(cluster_ward, xlab = "Parcelas", ylab = "Dist?ncia Euclidiana", main = "", 
hang = -1)
rect.hclust(cluster_ward, k=2)
plot(cluster_ward, xlab = "Parcelas", ylab = "Dist?ncia Euclidiana", main = "", 
hang = -1)
rect.hclust(cluster_ward, k=7)