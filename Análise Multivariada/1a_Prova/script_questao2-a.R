##### QUEST?O 2 #####

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
boxplot(X1, main = "X1", ylab = "Frequ?ncia")
boxplot(X2, main = "X2", ylab = "Frequ?ncia")
boxplot(X3, main = "X3", ylab = "Frequ?ncia")
par(mfrow = c(1,3))
boxplot(X4, main = "X4", ylab = "Frequ?ncia")
boxplot(X5, main = "X5", ylab = "Frequ?ncia")
boxplot(X6, main = "X6", ylab = "Frequ?ncia")
par(mfrow = c(1,3))
boxplot(X7, main = "X7", ylab = "Frequ?ncia")
boxplot(X8, main = "X8", ylab = "Frequ?ncia")
boxplot(X9, main = "X9", ylab = "Frequ?ncia")


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

KMO(R)  ## ?ndice KMO pelo pacote psych ##



## COMANDO ESPEC?FICO DE COMPONENTES PRINCIPAIS ##


## USANDO A MATRIZ S padronizada = MATRIZ R ##

eigen(R)

cpR <- prcomp(dados, scale = T)   ## cria os componentes principais usando R ##
cpR

par(mfrow = c(1,2))
screeplot(cpR)
screeplot(cpR, type = "lines")     ## gr?fico de cotovelo ##

require(factoextra)
fviz_eig(cpR)

names(cpR)
summary(cpR)           ## desvio padr?o, propor??o e propor??o acumulada ##

cpR$sdev               ## desvio padr?o dos CP's: ra?z quadrada autovalores ##
cpR$rotation           ## coeficientes cada componente principal: autovetores ##
cpR$center             ## coordenada central: m?dia amostral ##

cpR$rotation[,1]       ## coeficientes do 1? CP ##
score1 <- t(cpR$rotation[,1]) %*% t(dados)
t(score1)                  ## score para cada indiv?duo no CP1 ##

cpR$rotation[,2]       ## coeficientes do 2? CP ##
score2 <- t(cpR$rotation[,2]) %*% t(dados)
t(score2)                  ## score para cada indiv?duo no CP2 ##

cpR$rotation[,3]       ## coeficientes do 3? CP ##
score3 <- t(cpR$rotation[,3]) %*% t(dados)
t(score3)                  ## score para cada indiv?duo no CP3 ##

cpR$rotation[,4]       ## coeficientes do 4? CP ##
score4 <- t(cpR$rotation[,4]) %*% t(dados)
t(score4)                  ## score para cada indiv?duo no CP4 ##


cpR$x                  ## scores com vari?veis centradas ##


biplot(cpR, choices=c(1,2))            ## gr?fico biplot CP1,CP2##

biplot(cpR, choices=c(3,4))            ## gr?fico biplot CP3,CP4##