##### QUESTÃO 2 #####
### Alternativa B ###

## FATORIAL ##

tabela <- read.table("dados_prova1.txt", header = T)
tabela

dados <- tabela[,2:10]   
dados

row.names(dados) = tabela[,1]
dados

attach(dados)
names(dados)

## Matriz de Correlação ##
R <- cor(dados)
R

## Autovalores e Autovetores ##
autov <- eigen(R)
autov

## Da análise anterior (ACP), adotam-se os 4 primeiros CPs ##
## Proporção acumulada ##
prop_acu <- sum(autov$values[1:4]) / sum(autov$values)
prop_acu*100 #(%)


## Cargas fatoriais pelo método de CP ##

carga1 <- sqrt(autov$values[1]) * autov$vectors[,1]
carga2 <- sqrt(autov$values[2]) * autov$vectors[,2]
carga3 <- sqrt(autov$values[3]) * autov$vectors[,3]
carga4 <- sqrt(autov$values[4]) * autov$vectors[,4]

carga1
carga2
carga3
carga4

L <- cbind(carga1,carga2,carga3,carga4)
L

##Comunalidade: qualidade da análise##
com <- carga1^2 + carga2^2 + carga3^2 + carga4^2
com

## Matriz psi ##
psi <- R - L %*% t(L)
diag(psi)

# grafico de dispersao entre as cargas
par(mfrow = c(1,2))
plot(carga1,carga2, pch = 20)
text(carga1,carga2, adj=1.5)

plot(carga3,carga4, pch = 20)
text(carga3,carga4, adj=1.5)


## Transformação Varimax ##
V <- varimax(L, normalize = F)
V

par(mfrow = c(1,2))
plot(V$loading[,c(1,2)], pch = 20)
text(V$loadings[,c(1,2)], adj=1.5)

plot(V$loading[,c(3,4)], pch = 20)
text(V$loadings[,c(3,4)], adj=1.5)
