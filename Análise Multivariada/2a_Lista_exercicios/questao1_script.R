### QUESTÃO 1 ###

## Análise de correlação canônica

## Dados ##

require(MVar.pt)
help(DataMix)
data(DataMix)
data.frame(DataMix)

dados <- data.frame(cbind(DataMix[,2,],DataMix[,3,],DataMix[,6,],DataMix[,7,]))
dados

attach(dados)
dim(dados)
names(dados)


cor <- cor(dados)
cor

G1 <- as.matrix(cbind(DataMix[,2,],DataMix[,3,]))
G2 <- as.matrix(cbind(DataMix[,6,],DataMix[,7,]))

corca <- cancor(G1,G2)
corca

## Manualmente ##

r11 <- cor[1:2,1:2]
r11

r22 <- cor[3:4,3:4]
r22

r12 <- cor[1:2,3:4]
r12

A <- (solve(r11)) %*% r12 %*% (solve(r22)) %*% (t(r12))
A

B <- (solve(r22)) %*% (t(r12)) %*% (solve(r11)) %*% r12
B

eigen1 <- eigen(A)
eigen2 <- eigen(B)

eigen1
eigen2

sqrt(eigen1$values)      ## correlação canônica ##
sqrt(eigen2$values)      ## correlação canônica ##

## estimativas das variáveis canônicas ##
## G1: médias, anos

G1m <- (X1 - mean(X1))/(sd(X1))
G1m

G1a <- (X2 - mean(X2))/(sd(X2))
G1a

z1 <- as.matrix(cbind(G1m,G1a))
z1

a1 <- as.matrix(eigen1$vectors[,1]) 
a1

u1 <- z1 %*% a1
u1

a2 <- as.matrix(eigen1$vectors[,2]) 
a2

u2 <- z1 %*% a2
u2

cbind(u1,u2)


## G2: especiais, comerciais

G2e <- (X3 - mean(X3))/(sd(X3))
G2e

G2c <- (X4 - mean(X4))/(sd(X4))
G2c

z2 <- as.matrix(cbind(G2e,G2c))
z2

b1 <- as.matrix(eigen2$vectors[,1]) 
b1

v1 <- z2 %*% b1
v1

b2 <- as.matrix(eigen2$vectors[,2]) 
b2

v2 <- z2 %*% b2
v2

cbind(v1,v2)

## Correlação entre U e V

cor(u1,v1)    ## correlação canônica ##
cor(u1,v2)    ## correlação canônica ##
cor(u2,v1)    ## correlação canônica ##
cor(u2,v2)    ## correlação canônica ##


par(mfrow = c(2,2))
plot(u1,v1)
plot(u1,v2)
plot(u2,v1)
plot(u2,v2)


## correlação das variáveis canônicas U com as originais ##

cor(u1,X1); cor(u1,X2)
cor(u1,G1m); cor(u1,G1a) #Igual a anterior

cor(u2,X1); cor(u2,X2)
cor(u2,G1m); cor(u2,G1a) #Igual a anterior

cor(u1,X3); cor(u1,X4) 
cor(u1,G2e); cor(u1,G2c) #Igual a anterior

cor(u2,X3); cor(u2,X4)
cor(u2,G2e); cor(u2,G2c) #Igual a anterior

rbind(cor(u1,G1m), cor(u1,G1a), cor(u2,G1m), cor(u2,G1a),
cor(u1,G2e), cor(u1,G2c), cor(u2,G2e), cor(u2,G2c))


## correlação das variáveis canônicas V com as originais ##

cor(v1,X3); cor(v1,X4) 
cor(v1,G2e); cor(v1,G2c) #Igual a anterior

cor(v2,X1); cor(v2,X2)
cor(v2,G1m); cor(v2,G1a) #Igual a anterior

cor(v1,X1); cor(v1,X2)
cor(v1,G1m); cor(v1,G1a) #Igual a anterior

cor(v2,X3); cor(v2,X4)
cor(v2,G2e); cor(v2,G2c) #Igual a anterior

rbind(cor(v1,G2e), cor(v1,G2c), cor(v2,G1m), cor(v2,G1a),
cor(v1,G1m), cor(v1,G1a), cor(v2,G2e), cor(v2,G2c))


## medida de qualidade do modelo ##

p <- 2  ## nº variáveis no 1º grupo ##
q <- 2  ## nº variáveis no 2º grupo ##


cor_u1 <- (cor(u1,G1m)^2) + (cor(u1,G1a)^2)
prop_u1 <- 100 * (cor_u1/p)
prop_u1

cor_v1 <- (cor(v1,G2e)^2) + (cor(v1,G2c)^2)
prop_v1 <- 100 * (cor_v1/p)
prop_v1
