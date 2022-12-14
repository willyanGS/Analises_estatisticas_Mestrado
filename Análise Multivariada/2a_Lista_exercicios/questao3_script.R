### Quest?o 3 ###

## An?lise de regress?o linear multivariada

## Dados ##

dados <- read.table("dados_questao3.txt", header = T)
dados
attach(dados)
names(dados)

summary(dados)

x <- cbind(dados$P,dados$K,dados$Ca) #variaveis preditoras
x

y1 <- dados$Altura #primeira variavel resposta = Altura
y1
y2 <- dados$Prod #segunda variavel resposta = Produ??o
y2

## Correla??o entre as vari?veis respostas ##

cor(y1,y2)
plot(y1,y2) 


# Correla??o entre as vari?veis respostas e independentes #
## Y1
cor(P,y1)
cor(K,y1)
cor(Ca,y1)

par(mfrow = c(1,3))
plot(P,y1,xlab = "F?sforo", ylab = "Altura")
plot(K,y1,xlab = "Pot?ssio", ylab = "Altura")
plot(Ca,y1,xlab = "C?lcio", ylab = "Altura")

## Y2
cor(P,y2)
cor(K,y2)
cor(Ca,y2)

par(mfrow = c(1,3))
plot(P,y2,xlab = "F?sforo", ylab = "Produ??o")
plot(K,y2,xlab = "Pot?ssio", ylab = "Produ??o")
plot(Ca,y2,xlab = "C?lcio", ylab = "Produ??o")


# Correla??o entre as vari?veis independentes #

cor(P,K)
cor(P,Ca)
cor(K,Ca)

## Teste de normalidade dos dados - vari?veis independentes ##

# TESTE DE SHAPIRO-WILKS UNIVARIADO #

shapiro.test(P)
shapiro.test(K)
shapiro.test(Ca)

# TESTE DE Anderson-Darling #

require(mvsf)
ad.test(P)
ad.test(K)
ad.test(Ca)


## Coeficientes da regress?o linear ##

require(systemfit)

eq1 <- y1 ~ x 	## altura em fun??o das vari?veis independentes
eq2 <- y2 ~ x	## produ??o em fun??o das vari?veis independentes

eqSystem <- list(Altura = eq1, Prod = eq2)

fit_ols <- systemfit(eqSystem)

fit_ols             ## estimativas dos coeficientes ##

model <- lm(cbind(y1,y2) ~ x) # lm-linear model, variavel resposta em fun??o das preditoras
model					# apresenta os coeficiente associadas as variaveis preditoras

summary(model)
summary(fit_ols)

names(fit_ols)
fit_ols$coefCov       ## matriz de vari?ncias e covari?ncias das estimativas dos coeficientes ##

fitted(fit_ols)       ## valores estimados ##

cbind(y1,y2,fitted(fit_ols)) #tabela com os valores reais e estimados#

e <- as.matrix(residuals(fit_ols))     ## res?duos ##
e
ee <- t(e) %*% e
ee/(length(y1)-1-1)                ## estimativa matriz covari?ncias res?duos ##

fit_ols$residCov                   ## estimativa matriz covari?ncias res?duos ##


X <- cbind(rep(1,length(x)),x)
X

invXX <- solve(t(X) %*% X)
invXX

fit_ols$residCov[1,1] * invXX
fit_ols$residCov[2,2] * invXX
fit_ols$coefCov               ## matriz covari?ncias estimativas dos coeficientes ##


confint(fit_ols)   ## intervalos de confian?a para as estimativas coeficientes ##


## TESTE SE x ? SIGNIFICATIVO NO MODELO ##

fit_ols             ## estimativas dos coeficientes com x no modelo##

eq12 <- y1 ~ 1
eq22 <- y2 ~ 1

eq12
eq22

eq2System <- list(Altura = eq12, Prod = eq22)

fit_ols2 <- systemfit(eq2System)

fit_ols2             ## estimativas dos coeficientes ##
mean(y1)
mean(y2)
summary(fit_ols2)

## teste da razao de verossimilhanca comparando os dois modelos ##

lrtest(fit_ols, fit_ols2) # teste de Lambda de Wilks #
# p-valor significativo 5% = variaveis significativas para explicar produtividade