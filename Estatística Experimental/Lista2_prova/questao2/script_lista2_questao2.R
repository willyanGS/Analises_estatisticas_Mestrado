dados <- read.table("dados-split.txt", header=T)
dados

attach(dados)

adub <- as.factor(dados$adub)
aplic <- as.factor(dados$aplic)
bloco <- as.factor(dados$bloco)

##### ANÁLISE DESCRITIVA #####

### GERAL ###

summary(prod)
var(prod)
sd(prod)
(sd(prod)/mean(prod))*100   #coeficiente de variação#
sum(prod)
max(prod)-min(prod)  #amplitude#

require(moments)

skewness(prod)   #assimetria#
kurtosis(prod)   #curtose#

# HISTOGRAMA #

x <- seq(10, 80, length.out = 100)
d <- dnorm(x, mean=mean(prod), sd=sd(prod))
hist(prod, main = "Histograma", xlab = "Produção (100kg/ha)", ylab = "Frequência",
col=c("lightgray"), prob=T)
lines (x, d, col = "darkblue")

# BOX PLOT #

média = mean(prod)
boxplot(prod, main = "Boxplot", ylab = "Produção (100kg/ha)", col=c("lightgray"))
points(média, pch='x', cex=1.5, col='darkblue') 


### FATOR PRINCIPAL: DOSES DE ADUBAÇÃO ###

tapply(prod, adub, summary)
tapply(prod, adub, var)
tapply(prod, adub, sd)
(tapply(prod, adub, sd)/tapply(prod, adub, mean))*100   #coeficiente de variação#
tapply(prod, adub, sum)
tapply(prod, adub, max)-tapply(prod, adub, min)  #amplitude#

require(moments)

tapply(prod, adub, skewness)   #assimetria#
tapply(prod, adub, kurtosis)   #curtose#

# BOX PLOT #

médiasAdub = tapply(prod, adub, mean)
boxplot(prod~adub, main = "Boxplot - Adubação", xlab = "Dose de Adubação", 
ylab = "Produção (100kg/ha)", col=c("lightgray"), names = c("A1","A2"))
points(médiasAdub, pch='x', cex=1.5, col='darkblue') 


### FATOR SECUNDÁRIO: TIPOS DE APLICAÇÃO ###

tapply(prod, aplic, summary)
tapply(prod, aplic, var)
tapply(prod, aplic, sd)
(tapply(prod, aplic, sd)/tapply(prod, aplic, mean))*100   #coeficiente de variação#
tapply(prod, aplic, sum)
tapply(prod, aplic, max)-tapply(prod, aplic, min)  #amplitude#

require(moments)

tapply(prod, aplic, skewness)   #assimetria#
tapply(prod, aplic, kurtosis)   #curtose#

# BOX PLOT #

médiasAplic = tapply(prod, aplic, mean)
boxplot(prod~aplic, main = "Boxplot - Aplicação", xlab = "Tipo de Aplicação", 
ylab = "Produção (100kg/ha)", col=c("lightgray"), 
names = c("B1=cova","B2=sulco","B3=lanço"))
points(médiasAplic, pch='x', cex=1.5, col='darkblue')


### POR BLOCO ###

tapply(prod, bloco, summary)
tapply(prod, bloco, var)
tapply(prod, bloco, sd)
(tapply(prod, bloco, sd)/tapply(prod, bloco, mean))*100   #coeficiente de variação#
tapply(prod, bloco, sum)
tapply(prod, bloco, max)-tapply(prod, bloco, min)  #amplitude#

require(moments)

tapply(prod, bloco, skewness)   #assimetria#
tapply(prod, bloco, kurtosis)   #curtose#

# BOX PLOT #

médiasBloco = tapply(prod, bloco, mean)
boxplot(prod~bloco, main = "Boxplot - Blocos", xlab = "Bloco", 
ylab = "Produção (100kg/ha)", col=c("lightgray"), 
names = c("1","2","3","4"))
points(médiasBloco, pch='x', cex=1.5, col='darkblue')


### POR TRATAMENTO ###

tapply(prod, list(adub,aplic), mean)
tapply(prod, list(adub,aplic), min)
tapply(prod, list(adub,aplic), max)
tapply(prod, list(adub,aplic), median)
tapply(prod, list(adub,aplic), sum)
tapply(prod, list(adub,aplic), var)
tapply(prod, list(adub,aplic), sd)
tapply(prod, list(adub,aplic), max)-tapply(prod, list(adub,aplic), min)  #amplitude#
(tapply(prod, list(adub,aplic), sd)/tapply(prod, list(adub,aplic), mean))*100   #coeficiente de variação#


require(moments)

tapply(prod, list(adub,aplic), skewness)   #assimetria#
tapply(prod, list(adub,aplic), kurtosis)   #curtose#


# BOX PLOT #

médiasTrat = c(tapply(prod, list(adub,aplic), mean))
boxplot(prod~adub+aplic, main = "Boxplot - Tratamentos", xlab = "Tratamento", 
ylab = "Produção (100kg/ha)", col=c("lightgray"))
points(médiasTrat, pch='x', cex=1.5, col='darkblue')


##### ANÁLISE DE INTERAÇÃO #####

### gráfico em colunas ###

par(mfrow = c(1,2))
barplot(médiasAdub, beside=T, leg = T, ylim = c(0,45), xlab = "Adubação")
barplot(médiasAplic, beside=T, leg = T, ylim = c(0,45), xlab = "Aplicação")

### gráficos de interação ###

Adubação <- as.factor(dados$adub)
Aplicação <- as.factor(dados$aplic)
Produção <- (dados$prod)

interaction.plot(Adubação, Aplicação, Produção, ylab = "Produção média (100kg/ha)")
interaction.plot(Aplicação, Adubação, Produção, ylab = "Produção média (100kg/ha)")



##### ANÁLISE DAS SUPOSIÇÕES DO MODELO #####

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
hist(prod, main = "Histograma - Produção", xlab = "Produção (100kg/ha)", ylab = "Frequência",
col=c("lightgray"), prob=T)
lines (x, d, col = "darkblue")

xres <- seq(-30, 30, length.out = 100)
dres <- dnorm(xres, mean=mean(anvres$residuals), sd=sd(anvres$residuals))
hist(anvres$residuals, main = "Histograma - Resíduos", xlab = "Produção (100kg/ha)", ylab = "Frequência",
col=c("lightgray"), prob=T)
lines (xres, dres, col = "darkblue")

# Gráficos QQ-plot #

par(mfrow = c(1,2))

qqnorm(prod, main = "QQ-plot Produção", 
ylab="Quantis da amostra", xlab="Quantis teóricos")
qqline(prod, col = "darkblue")

qqnorm(anvres$residuals, main = "QQ-plot Resíduo",
ylab="Quantis da amostra", xlab="Quantis teóricos")
qqline(anvres$residuals, col = "darkblue")

# teste de Shapiro-Wilk #

shapiro.test(prod)
shapiro.test(anvres$residuals)


### homogeneidade das variâncias ###

# gráfico de dispersão resíduos x médias #

n <- length(prod)                         ## nº total de parcelas
I <- length(levels(trat))                 ## nº de tratamentos
r <- n/I                                  ## nº de repetições

repmean <- rep(tapply(prod, trat, mean), each = r)
plot(repmean,anvres$residuals, xlab = "Médias dos tratamentos", ylab = "Resíduos")

# boxplot para residuos/tratamento #

boxplot(anvres$residuals ~ trat, main = "Boxplot - resíduos", 
xlab = "Tratamento", ylab = "Resíduos", col=c("lightgray"))

# testes estatísticos #

bartlett.test(prod ~ adub)
bartlett.test(prod ~ aplic)
bartlett.test(prod ~ bloco)
bartlett.test(prod ~ trat)


### independência ###

plot(1:n, anvres$residuals, xlab = "Ordem da coleta", ylab = "Resíduos")


### resíduos padronizados ###

res <- resid(anvres) 
res

respad <- res/sqrt(sum(res^2)/anvres$df.res)   ## resíduos padronizados ##
respad

par(mfrow = c(1,2))
boxplot(respad, ylab = "Resíduos padronizados", 
main = "Boxplot dos Res. padronizados") 

hist(respad, main = "Histograma dos Res. padronizados", ylab = "Frequência",
xlab = "Resíduos padronizados")


##### ANÁLISE DE VARIÂNCIA #####
##### DESDOBRAMENTO ANOVA E TESTE DE COMPARAÇÃO DE MÉDIAS #####

require(ExpDes)

split2.rbd(adub, aplic, bloco, prod, quali = c(TRUE, TRUE), mcomp = "tukey", fac.names = c("Adubação", "Aplicação"), sigT = 0.05, sigF = 0.05)
