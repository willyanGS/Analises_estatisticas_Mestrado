##### QUESTÃO 2 #####

tabela <- read.table("dados_prova1.txt", header = T)
tabela

dados <- tabela[,2:10]   
dados

row.names(dados) = tabela[,1]
dados

attach(dados)
names(dados)

is.data.frame(dados)

dim(dados)

colMeans(dados)

summary(dados)

## BOX PLOTs ##
par(mfrow = c(1,3))
boxplot(X1, main = "X1", ylab = "Frequência")
boxplot(X2, main = "X2", ylab = "Frequência")
boxplot(X3, main = "X3", ylab = "Frequência")
par(mfrow = c(1,3))
boxplot(X4, main = "X4", ylab = "Frequência")
boxplot(X5, main = "X5", ylab = "Frequência")
boxplot(X6, main = "X6", ylab = "Frequência")
par(mfrow = c(1,3))
boxplot(X7, main = "X7", ylab = "Frequência")
boxplot(X8, main = "X8", ylab = "Frequência")
boxplot(X9, main = "X9", ylab = "Frequência")


### Alternativa A ###

## COMPONENTES PRINCIAPAIS ##

S <- cov(dados,dados)
S

R <- cor(dados,dados)
R

## teste de esfericidade de Bartlett ##
n <- nrow(dados)
require(psych)
cortest.bartlett(R,n)  ## teste de esfericidade de bartlett

KMO(R)  ## índice KMO pelo pacote psych ##



## COMANDO ESPECÍFICO DE COMPONENTES PRINCIPAIS ##


## USANDO A MATRIZ S padronizada = MATRIZ R ##

eigen(R)

cpR <- prcomp(dados, scale = T)   ## cria os componentes principais usando R ##
cpR

par(mfrow = c(1,2))
screeplot(cpR)
screeplot(cpR, type = "lines")     ## gráfico de cotovelo ##

require(factoextra)
fviz_eig(cpR)

names(cpR)
summary(cpR)           ## desvio padrão, proporção e proporção acumulada ##

cpR$sdev               ## desvio padrão dos CP's: raíz quadrada autovalores ##
cpR$rotation           ## coeficientes cada componente principal: autovetores ##
cpR$center             ## coordenada central: média amostral ##

cpR$rotation[,1]       ## coeficientes do 1º CP ##
score1 <- t(cpR$rotation[,1]) %*% t(dados)
t(score1)                  ## score para cada indivíduo no CP1 ##

cpR$rotation[,2]       ## coeficientes do 2º CP ##
score2 <- t(cpR$rotation[,2]) %*% t(dados)
t(score2)                  ## score para cada indivíduo no CP2 ##

cpR$rotation[,3]       ## coeficientes do 3º CP ##
score3 <- t(cpR$rotation[,3]) %*% t(dados)
t(score3)                  ## score para cada indivíduo no CP3 ##

cpR$rotation[,4]       ## coeficientes do 4º CP ##
score4 <- t(cpR$rotation[,4]) %*% t(dados)
t(score4)                  ## score para cada indivíduo no CP4 ##


cpR$x                  ## scores com variáveis centradas ##


biplot(cpR, choices=c(1,2))            ## gráfico biplot CP1,CP2##

biplot(cpR, choices=c(3,4))            ## gráfico biplot CP3,CP4##