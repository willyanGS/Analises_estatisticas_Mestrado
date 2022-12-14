##########################################################
### 1a LISTA DE EXERC?CIOS - AN?LISE MULTIVARIADA 2018 ###
##########################################################



	##### Questao 3 #####


dados <- read.table("questao3_dados.txt", header = T)   
dados

names(dados)
is.data.frame(dados)
c <- as.matrix(dados)
c

attach(dados)
dim(dados)



	### Alternativa (a) ###


summary(dados)

u <-colMeans(dados) ## vetor de m?dias ##
u
S <- cov(dados,dados) ## matriz de variancia e covariancia ##
S
R <- cor(dados,dados) ## matriz de correla??o ##
R

## gr?fico das correla??es amostrais ##

require(corrplot)
corrplot(R, method="pie", type = c("upper"), diag = F, tl.cex = 1.2, 
tl.col = "black", addCoef.col = "black")

## teste de hip?teses das correla??es cor.test ##

library(readr)
library(psych)
corr.test(dados)

cor.test(x1,x2)
cor.test(x1,x3)
cor.test(x1,x4)
cor.test(x1,x5)
cor.test(x2,x3)
cor.test(x2,x4)
cor.test(x2,x5)
cor.test(x3,x4)
cor.test(x3,x5)
cor.test(x4,x5)


	### Alternativa (b) ###

## vari?vel padronizada ##
dadosPad <- scale(dados)
dadosPad
cov(dadosPad)
cor(dados)
## s?o iguais ##


	### Alternativa (c) ###

## Diagrama de dispers?o - vari?veis duas a duas ##

pairs(dados, c("X1", "X2", "X3", "X4", "X5"))

par(mfrow = c(2,2))
diagrama1 <- plot(x1~x2, main = "x1-x2")
diagrama2 <-plot(x1~x3, main = "x1-x3")
diagrama3 <-plot(x1~x4, main = "x1-x4")
diagrama4 <-plot(x1~x5, main = "x1-x5")

par(mfrow = c(2,2))
diagrama5 <-plot(x2~x3, main = "x2-x3")
diagrama6 <-plot(x2~x4, main = "x2-x4")
diagrama7 <-plot(x2~x5, main = "x2-x5")

par(mfrow = c(2,2))
diagrama8 <-plot(x3~x4, main = "x3-x4")
diagrama9 <-plot(x3~x5, main = "x3-x5")
diagrama10 <-plot(x4~x5, main = "x4-x5")


## gr?ficos de dispers?o simult?neos com diagonal = histograma das vari?veis ##
##  fun??o que cria um histograma ##

panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

pairs(dados, c("X1", "X2", "X3", "X4", "X5"), diag.panel=panel.hist)



	### Alternativa (d) ###

## X1 ##
par(mfrow = c(1,2))
hist(x1, col="gray", main= "X1 - Precipita??o total em 
Novembro e Dezembro",
xlab = "mm Nov/Dez", ylab = "Frequ?ncia", prob=F) 	## Histograma
qqnorm(x1, main = "QQ-plot para X1")
qqline(x1, col = "blue")           ## qq-plot

## X2 ##
par(mfrow = c(1,2))
hist(x2, main = "X2 - Temperatura m?dia 
em Julho", col="gray",xlab = "?C", 
ylab = "Frequ?ncia", prob=F) 		## Histograma
qqnorm(x2, main = "QQ-plot para X2")
qqline(x2, col = "blue")           ## qq-plot

## X3 ##
par(mfrow = c(1,2))
hist(x3, main = "X3 - Precipita??o total 
em Julho", col="gray",
xlab = "mm Jul", ylab = "Frequ?ncia", prob=F)	## Histograma
qqnorm(x3, main = "QQ-plot para X3")
qqline(x3, col = "blue")           ## qq-plot

## X4 ##
par(mfrow = c(1,2))
hist(x4, main = "X4 - Radia??o 
em Julho", col="gray",xlab = "mm ?lcool", 
ylab = "Frequ?ncia", prob=T) 		## Histograma
qqnorm(x4, main = "QQ-plot para X4")
qqline(x4, col = "blue")           ## qq-plot

## X5 ##
par(mfrow = c(1,2))
hist(x5, main = "X5 - Rendimento m?dio 
de colheita", col="gray",
xlab = "quintais/hectares", ylab = "Frequ?ncia", prob=T)	## Histograma
qqnorm(x5, main = "QQ-plot para X5")
qqline(x5, col = "blue")           ## qq-plot

## Boxplot - Vari?veis ##
par(mfrow = c(2,3))
boxplot(x1, xlab = "X1 - Precipita??o total em 
Novembro e Dezembro",
ylab = "mm Nov/Dez", col=c("lightgray"))
points(mean(x1), pch='x', cex=1.5, col='darkblue') 

boxplot(x2, xlab = "X2 - Temperatura m?dia em Julho",
ylab = "?C", col=c("lightgray"))
points(mean(x2), pch='x', cex=1.5, col='darkblue')
 
boxplot(x3, xlab = "X3 - Precipita??o total em Julho",
ylab = "mm Jul", col=c("lightgray"))
points(mean(x3), pch='x', cex=1.5, col='darkblue') 

boxplot(x4, xlab = "X4 - Radia??o em Julho",
ylab = "mm ?lcool", col=c("lightgray"))
points(mean(x4), pch='x', cex=1.5, col='darkblue')
 
boxplot(x5, xlab = "X5 - Rendimento m?dio de colheita",
ylab = "quintais/hectares", col=c("lightgray"))
points(mean(x5), pch='x', cex=1.5, col='darkblue') 

## TESTE DE SHAPIRO-WILKS UNIVARIADO ##
shapiro.test(x1)
shapiro.test(x2)
shapiro.test(x3)
shapiro.test(x4)
shapiro.test(x5)



	### Alternativa (e) ###

## GR?FICO QQ-PLOT MULTIVARIADO ##

S=var(c)
S
m=apply(c,2,mean)   ## indica que ? p/ fazer os c?lculos para cada coluna ##
m
invS=solve(S)
invS
d1_2 <- t(c[5,]-m) %*% invS %*% (c[5,]-m)
d1_2
(dim(c)[1]-1+0.5)/dim(c)[1]
1-((1-0.5)/dim(c)[1])
d = q = NULL
n= nrow(c)    ## n? de indiv?duos ##
p= ncol(c)    ## n? de vari?veis ##
for (i in 1:n){
  d = c(d, t(c[i,]-m) %*% invS %*% (c[i,]-m))
  prob = (i-0.5)/n
  q = c(q, qchisq(prob, df=p))
}
d=sort(d)
d
q

plot(d,q, xlab = "Dist?ncias Ordenadas - di^2", ylab = "Quantil Qui-Quadrado")


## TESTE DE SHAPIRO WILKS MULTIVARIDO ##

require(mvnormtest)
mshapiro.test(t(c))

## TESTE DE NORMALIDADE MULTIVARIADO DE SHAPIRO-FRANCIA ##
require(mvsf)
mvsf(t(c))            

