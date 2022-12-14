dados <- read.table("dados-split.txt", header=T)
dados

attach(dados)

adub <- as.factor(dados$adub)
aplic <- as.factor(dados$aplic)
bloco <- as.factor(dados$bloco)

##### AN?LISE DESCRITIVA #####

### GERAL ###

summary(prod)
var(prod)
sd(prod)
(sd(prod)/mean(prod))*100   #coeficiente de varia??o#
sum(prod)
max(prod)-min(prod)  #amplitude#

require(moments)

skewness(prod)   #assimetria#
kurtosis(prod)   #curtose#

# HISTOGRAMA #

x <- seq(10, 80, length.out = 100)
d <- dnorm(x, mean=mean(prod), sd=sd(prod))
hist(prod, main = "Histograma", xlab = "Produ??o (100kg/ha)", ylab = "Frequ?ncia",
col=c("lightgray"), prob=T)
lines (x, d, col = "darkblue")

# BOX PLOT #

m?dia = mean(prod)
boxplot(prod, main = "Boxplot", ylab = "Produ??o (100kg/ha)", col=c("lightgray"))
points(m?dia, pch='x', cex=1.5, col='darkblue') 


### FATOR PRINCIPAL: DOSES DE ADUBA??O ###

tapply(prod, adub, summary)
tapply(prod, adub, var)
tapply(prod, adub, sd)
(tapply(prod, adub, sd)/tapply(prod, adub, mean))*100   #coeficiente de varia??o#
tapply(prod, adub, sum)
tapply(prod, adub, max)-tapply(prod, adub, min)  #amplitude#

require(moments)

tapply(prod, adub, skewness)   #assimetria#
tapply(prod, adub, kurtosis)   #curtose#

# BOX PLOT #

m?diasAdub = tapply(prod, adub, mean)
boxplot(prod~adub, main = "Boxplot - Aduba??o", xlab = "Dose de Aduba??o", 
ylab = "Produ??o (100kg/ha)", col=c("lightgray"), names = c("A1","A2"))
points(m?diasAdub, pch='x', cex=1.5, col='darkblue') 


### FATOR SECUND?RIO: TIPOS DE APLICA??O ###

tapply(prod, aplic, summary)
tapply(prod, aplic, var)
tapply(prod, aplic, sd)
(tapply(prod, aplic, sd)/tapply(prod, aplic, mean))*100   #coeficiente de varia??o#
tapply(prod, aplic, sum)
tapply(prod, aplic, max)-tapply(prod, aplic, min)  #amplitude#

require(moments)

tapply(prod, aplic, skewness)   #assimetria#
tapply(prod, aplic, kurtosis)   #curtose#

# BOX PLOT #

m?diasAplic = tapply(prod, aplic, mean)
boxplot(prod~aplic, main = "Boxplot - Aplica??o", xlab = "Tipo de Aplica??o", 
ylab = "Produ??o (100kg/ha)", col=c("lightgray"), 
names = c("B1=cova","B2=sulco","B3=lan?o"))
points(m?diasAplic, pch='x', cex=1.5, col='darkblue')


### POR BLOCO ###

tapply(prod, bloco, summary)
tapply(prod, bloco, var)
tapply(prod, bloco, sd)
(tapply(prod, bloco, sd)/tapply(prod, bloco, mean))*100   #coeficiente de varia??o#
tapply(prod, bloco, sum)
tapply(prod, bloco, max)-tapply(prod, bloco, min)  #amplitude#

require(moments)

tapply(prod, bloco, skewness)   #assimetria#
tapply(prod, bloco, kurtosis)   #curtose#

# BOX PLOT #

m?diasBloco = tapply(prod, bloco, mean)
boxplot(prod~bloco, main = "Boxplot - Blocos", xlab = "Bloco", 
ylab = "Produ??o (100kg/ha)", col=c("lightgray"), 
names = c("1","2","3","4"))
points(m?diasBloco, pch='x', cex=1.5, col='darkblue')


### POR TRATAMENTO ###

tapply(prod, list(adub,aplic), mean)
tapply(prod, list(adub,aplic), min)
tapply(prod, list(adub,aplic), max)
tapply(prod, list(adub,aplic), median)
tapply(prod, list(adub,aplic), sum)
tapply(prod, list(adub,aplic), var)
tapply(prod, list(adub,aplic), sd)
tapply(prod, list(adub,aplic), max)-tapply(prod, list(adub,aplic), min)  #amplitude#
(tapply(prod, list(adub,aplic), sd)/tapply(prod, list(adub,aplic), mean))*100   #coeficiente de varia??o#


require(moments)

tapply(prod, list(adub,aplic), skewness)   #assimetria#
tapply(prod, list(adub,aplic), kurtosis)   #curtose#


# BOX PLOT #

m?diasTrat = c(tapply(prod, list(adub,aplic), mean))
boxplot(prod~adub+aplic, main = "Boxplot - Tratamentos", xlab = "Tratamento", 
ylab = "Produ??o (100kg/ha)", col=c("lightgray"))
points(m?diasTrat, pch='x', cex=1.5, col='darkblue')


##### AN?LISE DE INTERA??O #####

### gr?fico em colunas ###

par(mfrow = c(1,2))
barplot(m?diasAdub, beside=T, leg = T, ylim = c(0,45), xlab = "Aduba??o")
barplot(m?diasAplic, beside=T, leg = T, ylim = c(0,45), xlab = "Aplica??o")

### gr?ficos de intera??o ###

Aduba??o <- as.factor(dados$adub)
Aplica??o <- as.factor(dados$aplic)
Produ??o <- (dados$prod)

interaction.plot(Aduba??o, Aplica??o, Produ??o, ylab = "Produ??o m?dia (100kg/ha)")
interaction.plot(Aplica??o, Aduba??o, Produ??o, ylab = "Produ??o m?dia (100kg/ha)")



##### AN?LISE DAS SUPOSI??ES DO MODELO #####

dadostrat <- read.table("dados-split-tratamento.txt", header=T)
dadostrat

attach(dadostrat)

trat <- as.factor(dadostrat$tratamento)

anvres <- aov(prod ~ trat)

### Normalidade ###

# Histograma com curva normal #

par(mfrow = c(1,2))

x <- seq(10, 80, length.out = 100)
d <- dnorm(x, mean=mean(prod), sd=sd(prod))
hist(prod, main = "Histograma - Produ??o", xlab = "Produ??o (100kg/ha)", ylab = "Frequ?ncia",
col=c("lightgray"), prob=T)
lines (x, d, col = "darkblue")

xres <- seq(-30, 30, length.out = 100)
dres <- dnorm(xres, mean=mean(anvres$residuals), sd=sd(anvres$residuals))
hist(anvres$residuals, main = "Histograma - Res?duos", xlab = "Produ??o (100kg/ha)", ylab = "Frequ?ncia",
col=c("lightgray"), prob=T)
lines (xres, dres, col = "darkblue")

# Gr?ficos QQ-plot #

par(mfrow = c(1,2))

qqnorm(prod, main = "QQ-plot Produ??o", 
ylab="Quantis da amostra", xlab="Quantis te?ricos")
qqline(prod, col = "darkblue")

qqnorm(anvres$residuals, main = "QQ-plot Res?duo",
ylab="Quantis da amostra", xlab="Quantis te?ricos")
qqline(anvres$residuals, col = "darkblue")

# teste de Shapiro-Wilk #

shapiro.test(prod)
shapiro.test(anvres$residuals)


### homogeneidade das vari?ncias ###

# gr?fico de dispers?o res?duos x m?dias #

n <- length(prod)                         ## n? total de parcelas
I <- length(levels(trat))                 ## n? de tratamentos
r <- n/I                                  ## n? de repeti??es

repmean <- rep(tapply(prod, trat, mean), each = r)
plot(repmean,anvres$residuals, xlab = "M?dias dos tratamentos", ylab = "Res?duos")

# boxplot para residuos/tratamento #

boxplot(anvres$residuals ~ trat, main = "Boxplot - res?duos", 
xlab = "Tratamento", ylab = "Res?duos", col=c("lightgray"))

# testes estat?sticos #

bartlett.test(prod ~ adub)
bartlett.test(prod ~ aplic)
bartlett.test(prod ~ bloco)
bartlett.test(prod ~ trat)


### independ?ncia ###

plot(1:n, anvres$residuals, xlab = "Ordem da coleta", ylab = "Res?duos")


### res?duos padronizados ###

res <- resid(anvres) 
res

respad <- res/sqrt(sum(res^2)/anvres$df.res)   ## res?duos padronizados ##
respad

par(mfrow = c(1,2))
boxplot(respad, ylab = "Res?duos padronizados", 
main = "Boxplot dos Res. padronizados") 

hist(respad, main = "Histograma dos Res. padronizados", ylab = "Frequ?ncia",
xlab = "Res?duos padronizados")


##### AN?LISE DE VARI?NCIA #####
##### DESDOBRAMENTO ANOVA E TESTE DE COMPARA??O DE M?DIAS #####

require(ExpDes)

split2.rbd(adub, aplic, bloco, prod, quali = c(TRUE, TRUE), mcomp = "tukey", fac.names = c("Aduba??o", "Aplica??o"), sigT = 0.05, sigF = 0.05)
