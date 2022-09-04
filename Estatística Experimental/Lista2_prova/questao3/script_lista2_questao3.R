dados <- read.table("dados-regressão.txt", header=T)
dados

attach(dados)

##### ANÁLISE DESCRITIVA #####

### GERAL ###

summary(razao)
var(razao)
sd(razao)
(sd(razao)/mean(razao))*100   #coeficiente de variação#
sum(razao)
max(razao)-min(razao)  #amplitude#

require(moments)

skewness(razao)   #assimetria#
kurtosis(razao)   #curtose#

# HISTOGRAMA #

hist(razao, main = "Histograma", xlab = "Razão Eficiência (g dieta/g ganho de peso)", 
ylab = "Frequência", col=c("lightgray"))


# BOX PLOT #

média = mean(razao)
boxplot(razao, main = "Boxplot", ylab = "Razão Eficiência (g dieta/g ganho de peso)",
col=c("lightgray"))
points(média, pch='x', cex=1.5, col='darkblue') 


### POR TRATAMENTO ###

tapply(razao, cobre, summary)
tapply(razao, cobre, var)
tapply(razao, cobre, sd)
(tapply(razao, cobre, sd)/tapply(razao, cobre, mean))*100   #coeficiente de variação#
tapply(razao, cobre, sum)
tapply(razao, cobre, max)-tapply(razao, cobre, min)  #amplitude#

require(moments)

tapply(razao, cobre, skewness)   #assimetria#
tapply(razao, cobre, kurtosis)   #curtose#

# BOX PLOT #

médias = tapply(razao, cobre, mean)
boxplot(razao~cobre, main = "Boxplot - Níveis de Cobre", xlab = "Nível de cobre (ppm)", 
ylab = "Razão Eficiência (g dieta/g ganho de peso)", col=c("lightgray"))
points(médias, pch='x', cex=1.5, col='darkblue') 



##### ANÁLISE DAS SUPOSIÇÕES DO MODELO #####

cobreF <- as.factor(dados$cobre)
anvres <- aov(razao ~ cobreF)
anova(anvres)

### Normalidade ###

# Histograma com curva normal #

par(mfrow = c(1,2))

x <- seq(1.5, 2.2, length.out = 30)
d <- dnorm(x, mean=mean(razao), sd=sd(razao))
hist(razao, main = "Histograma - Razão Eficiência Dieta", xlab = "Razão Eficiência (g dieta/g ganho de peso)", 
ylab = "Frequência", col=c("lightgray"), prob=T)
lines (x, d, col = "darkblue")

xres <- seq(-0.3, 0.4, length.out = 30)
dres <- dnorm(xres, mean=mean(anvres$residuals), sd=sd(anvres$residuals))
hist(anvres$residuals, main = "Histograma - Resíduos", xlab = "Produção (100kg/ha)", ylab = "Frequência",
col=c("lightgray"), prob=T)
lines (xres, dres, col = "darkblue")

# Gráficos QQ-plot #

par(mfrow = c(1,2))

qqnorm(razao, main = "QQ-plot Razão Eficiência
(g dieta/g ganho de peso)", 
ylab="Quantis da amostra", xlab="Quantis teóricos")
qqline(razao, col = "darkblue")

qqnorm(anvres$residuals, main = "QQ-plot Resíduo",
ylab="Quantis da amostra", xlab="Quantis teóricos")
qqline(anvres$residuals, col = "darkblue")

# teste de Shapiro-Wilk #

shapiro.test(razao)
shapiro.test(anvres$residuals)

### homogeneidade das variâncias ###

# teste estatístico #

require(car)
leveneTest(razao~cobreF)


##### ANÁLISE DE VARIÂNCIA #####
##### ANÁLISE DE REGRESSÃO #####

require(ExpDes)

## considerando NÍVEL DE COBRE como variável qualitativa ##
crd(cobre, razao, quali = T, sigF = 0.05, sigT = 0.05)  

## considerando x como variável quantitativa ##
## R2 = SQReg/SQTrat ##
## SQ lack of fit(falta de ajuste) = SQTrat - SQReg ##

crd(cobre, razao, quali = F, sigF = 0.05, sigT = 0.05)  ## só está implementada até grau 3 ##


## LINEAR ##

## estimativa do modelo
lin <- lm(razao ~ cobre)
lin

## curva do modelo estimado
plot(cobre,razao, xlab = "Nível de Cobre (ppm)", 
ylab = "Razão Eficiência (g dieta/g ganho de peso)")
lines(cobre,lin$fi, col="darkblue")