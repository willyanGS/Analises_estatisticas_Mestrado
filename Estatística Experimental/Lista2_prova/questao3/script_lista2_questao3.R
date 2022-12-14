dados <- read.table("dados-regress?o.txt", header=T)
dados

attach(dados)

##### AN?LISE DESCRITIVA #####

### GERAL ###

summary(razao)
var(razao)
sd(razao)
(sd(razao)/mean(razao))*100   #coeficiente de varia??o#
sum(razao)
max(razao)-min(razao)  #amplitude#

require(moments)

skewness(razao)   #assimetria#
kurtosis(razao)   #curtose#

# HISTOGRAMA #

hist(razao, main = "Histograma", xlab = "Raz?o Efici?ncia (g dieta/g ganho de peso)", 
ylab = "Frequ?ncia", col=c("lightgray"))


# BOX PLOT #

m?dia = mean(razao)
boxplot(razao, main = "Boxplot", ylab = "Raz?o Efici?ncia (g dieta/g ganho de peso)",
col=c("lightgray"))
points(m?dia, pch='x', cex=1.5, col='darkblue') 


### POR TRATAMENTO ###

tapply(razao, cobre, summary)
tapply(razao, cobre, var)
tapply(razao, cobre, sd)
(tapply(razao, cobre, sd)/tapply(razao, cobre, mean))*100   #coeficiente de varia??o#
tapply(razao, cobre, sum)
tapply(razao, cobre, max)-tapply(razao, cobre, min)  #amplitude#

require(moments)

tapply(razao, cobre, skewness)   #assimetria#
tapply(razao, cobre, kurtosis)   #curtose#

# BOX PLOT #

m?dias = tapply(razao, cobre, mean)
boxplot(razao~cobre, main = "Boxplot - N?veis de Cobre", xlab = "N?vel de cobre (ppm)", 
ylab = "Raz?o Efici?ncia (g dieta/g ganho de peso)", col=c("lightgray"))
points(m?dias, pch='x', cex=1.5, col='darkblue') 



##### AN?LISE DAS SUPOSI??ES DO MODELO #####

cobreF <- as.factor(dados$cobre)
anvres <- aov(razao ~ cobreF)
anova(anvres)

### Normalidade ###

# Histograma com curva normal #

par(mfrow = c(1,2))

x <- seq(1.5, 2.2, length.out = 30)
d <- dnorm(x, mean=mean(razao), sd=sd(razao))
hist(razao, main = "Histograma - Raz?o Efici?ncia Dieta", xlab = "Raz?o Efici?ncia (g dieta/g ganho de peso)", 
ylab = "Frequ?ncia", col=c("lightgray"), prob=T)
lines (x, d, col = "darkblue")

xres <- seq(-0.3, 0.4, length.out = 30)
dres <- dnorm(xres, mean=mean(anvres$residuals), sd=sd(anvres$residuals))
hist(anvres$residuals, main = "Histograma - Res?duos", xlab = "Produ??o (100kg/ha)", ylab = "Frequ?ncia",
col=c("lightgray"), prob=T)
lines (xres, dres, col = "darkblue")

# Gr?ficos QQ-plot #

par(mfrow = c(1,2))

qqnorm(razao, main = "QQ-plot Raz?o Efici?ncia
(g dieta/g ganho de peso)", 
ylab="Quantis da amostra", xlab="Quantis te?ricos")
qqline(razao, col = "darkblue")

qqnorm(anvres$residuals, main = "QQ-plot Res?duo",
ylab="Quantis da amostra", xlab="Quantis te?ricos")
qqline(anvres$residuals, col = "darkblue")

# teste de Shapiro-Wilk #

shapiro.test(razao)
shapiro.test(anvres$residuals)

### homogeneidade das vari?ncias ###

# teste estat?stico #

require(car)
leveneTest(razao~cobreF)


##### AN?LISE DE VARI?NCIA #####
##### AN?LISE DE REGRESS?O #####

require(ExpDes)

## considerando N?VEL DE COBRE como vari?vel qualitativa ##
crd(cobre, razao, quali = T, sigF = 0.05, sigT = 0.05)  

## considerando x como vari?vel quantitativa ##
## R2 = SQReg/SQTrat ##
## SQ lack of fit(falta de ajuste) = SQTrat - SQReg ##

crd(cobre, razao, quali = F, sigF = 0.05, sigT = 0.05)  ## s? est? implementada at? grau 3 ##


## LINEAR ##

## estimativa do modelo
lin <- lm(razao ~ cobre)
lin

## curva do modelo estimado
plot(cobre,razao, xlab = "N?vel de Cobre (ppm)", 
ylab = "Raz?o Efici?ncia (g dieta/g ganho de peso)")
lines(cobre,lin$fi, col="darkblue")