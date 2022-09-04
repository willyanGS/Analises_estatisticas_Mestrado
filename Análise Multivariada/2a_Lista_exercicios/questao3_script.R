### Questão 3 ###

## Análise de regressão linear multivariada

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
y2 <- dados$Prod #segunda variavel resposta = Produção
y2

## Correlação entre as variáveis respostas ##

cor(y1,y2)
plot(y1,y2) 


# Correlação entre as variáveis respostas e independentes #
## Y1
cor(P,y1)
cor(K,y1)
cor(Ca,y1)

par(mfrow = c(1,3))
plot(P,y1,xlab = "Fósforo", ylab = "Altura")
plot(K,y1,xlab = "Potássio", ylab = "Altura")
plot(Ca,y1,xlab = "Cálcio", ylab = "Altura")

## Y2
cor(P,y2)
cor(K,y2)
cor(Ca,y2)

par(mfrow = c(1,3))
plot(P,y2,xlab = "Fósforo", ylab = "Produção")
plot(K,y2,xlab = "Potássio", ylab = "Produção")
plot(Ca,y2,xlab = "Cálcio", ylab = "Produção")


# Correlação entre as variáveis independentes #

cor(P,K)
cor(P,Ca)
cor(K,Ca)

## Teste de normalidade dos dados - variáveis independentes ##

# TESTE DE SHAPIRO-WILKS UNIVARIADO #

shapiro.test(P)
shapiro.test(K)
shapiro.test(Ca)

# TESTE DE Anderson-Darling #

require(mvsf)
ad.test(P)
ad.test(K)
ad.test(Ca)


## Coeficientes da regressão linear ##

require(systemfit)

eq1 <- y1 ~ x 	## altura em função das variáveis independentes
eq2 <- y2 ~ x	## produção em função das variáveis independentes

eqSystem <- list(Altura = eq1, Prod = eq2)

fit_ols <- systemfit(eqSystem)

fit_ols             ## estimativas dos coeficientes ##

model <- lm(cbind(y1,y2) ~ x) # lm-linear model, variavel resposta em função das preditoras
model					# apresenta os coeficiente associadas as variaveis preditoras

summary(model)
summary(fit_ols)

names(fit_ols)
fit_ols$coefCov       ## matriz de variâncias e covariâncias das estimativas dos coeficientes ##

fitted(fit_ols)       ## valores estimados ##

cbind(y1,y2,fitted(fit_ols)) #tabela com os valores reais e estimados#

e <- as.matrix(residuals(fit_ols))     ## resíduos ##
e
ee <- t(e) %*% e
ee/(length(y1)-1-1)                ## estimativa matriz covariâncias resíduos ##

fit_ols$residCov                   ## estimativa matriz covariâncias resíduos ##


X <- cbind(rep(1,length(x)),x)
X

invXX <- solve(t(X) %*% X)
invXX

fit_ols$residCov[1,1] * invXX
fit_ols$residCov[2,2] * invXX
fit_ols$coefCov               ## matriz covariâncias estimativas dos coeficientes ##


confint(fit_ols)   ## intervalos de confiança para as estimativas coeficientes ##


## TESTE SE x É SIGNIFICATIVO NO MODELO ##

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