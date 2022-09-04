##### QUESTÃO 2 #####
### Alternativa D ###

## AGRUPAMENTO - VARIÁVEIS ##

tabela <- read.table("dados_prova1.txt", header = T)
tabela

dados <- tabela[,2:10]   
dados

row.names(dados) = tabela[,1]
dados

attach(dados)
names(dados)

## matriz de correlação Pearson ##
R <- cor(dados)
R                              

## matriz de distancia Rencher ##
r <- as.dist((1-(R^2)))  ## SUGERIDA POR RENCHER (2002) ##
r

## Cálculo dos Coeficientes Cor. Cofenéticos para escolha dos métodos ##

## LIGAÇÃO SIMPLES (Single Linkage) - VIZINHO MAIS PRÓXIMO ##
cluster_single <- hclust(r, "single")
cluster_single
help(hclust)
d1 <- cophenetic(cluster_single)
cor1 <- cor(r,d1)              ## coeficiente de correlação cofenética ##

## LIGAÇÃO COMPLETA (Complete Linkage) - VIZINHO MAIS DISTANTE ##
cluster_complete <- hclust(r, "complete")
cluster_complete
d2 <- cophenetic(cluster_complete)
cor2 <- cor(r,d2)              ## coeficiente de correlação cofenética ##

## LIGAÇÃO MÉDIA (Average Linkage) ##
cluster_average <- hclust(r, "average")
cluster_average
d3 <- cophenetic(cluster_average)
cor3 <- cor(r,d3)              ## coeficiente de correlação cofenética ##

## CENTRÓIDE ##
cluster_centroid <- hclust(r, "centroid")
cluster_centroid
d4 <- cophenetic(cluster_centroid)
cor4 <- cor(r,d4)              ## coeficiente de correlação cofenética ##

## WARD ##
cluster_ward <- hclust(r, "ward")
cluster_ward
d5 <- cophenetic(cluster_ward)
cor5 <- cor(r,d5)              ## coeficiente de correlação cofenética ##

ccc <- c(cor1, cor2, cor3, cor4, cor5)
cbind(ccc)		## Métodos escolhidos: Ligação Média e Ward ##


	## DENDOGRAMAS ##

## LIGAÇÃO MÉDIA ##

cluster_average$height       ## valores de junção no dendograma (maior salto)##
## definir numero de clusters ##
## h>0,64
## k=6
plot(cluster_average, xlab = "Parcelas", ylab = "Distância Euclidiana", main = "", 
hang = -1)
rect.hclust(cluster_average, k=6)

## classifica os elementos em cada grupo ##
c <- cutree(cluster_average, k = 6)     
## diagrama de dispersão a cada 2 variáveis, identificando os grupos ##             
plot(dados, col = c)  

## LIGAÇÃO SIMPLES ##

cluster_single$height       ## valores de junção no dendograma (maior salto)##
## definir numero de clusters ##
## h>0,72
## k= 2
plot(cluster_single, xlab = "Parcelas", ylab = "Distância Euclidiana", main = "", 
hang = -1)
rect.hclust(cluster_single, k=2)


## WARD ##

cluster_ward$height       ## valores de junção no dendograma (maior salto)##
## definir numero de clusters ##
## h>0,99
## k= 2
plot(cluster_ward, xlab = "Parcelas", ylab = "Distância Euclidiana", main = "", 
hang = -1)
rect.hclust(cluster_ward, k=2)
plot(cluster_ward, xlab = "Parcelas", ylab = "Distância Euclidiana", main = "", 
hang = -1)
rect.hclust(cluster_ward, k=7)