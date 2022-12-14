## ___________________________ QUEST?O 2 ___________________________ ##


##----------------------------------------------##
## ____ ALTERNATIVA C ____ ##
##----------------------------------------------##

dados <- read.table("dados_lista1_produ??o_batata_doce.txt", header = T)   ## lendo um conjunto de dados em txt, header = T tem nome das colunas na 1? linha ##
dados

attach(dados)      

## __ AN?LISE DESCRITIVA GERAL __ ##

summary(prod)                               ## resumo descritivo geral ##
var(prod)                                       ## vari?ncia geral ## 
sd(prod)                                    ## desvio padr?o geral ##                               
((sd(prod))/(mean(prod)))*100             ## coeficiente de varia??o ##
sum(prod)                                 ## soma geral ##
max(prod)-min(prod)                       ## amplitude dos dados ##

require(moments)

## Assimetria (AS) ##
## AS = 0 distribui??o sim?trica; 
## AS > 0 distribui??o assim?trica positiva; 
## AS < 0 distribui??o assim?trica negativa. 

skewness(prod)

## Curtose (CUR) ##
## CUR = 3 distribui??o com caudas neutras (normais - mesoc?rtica); 
## CUR > 3 distribui??o com caudas longas ou pesadas (leptoc?rtica); 
## CUR < 3 distribui??o com caudas curtas ou leves (platic?rtica). 

kurtosis(prod) #curtose#

## histograma ##

x <- seq(20, 140, length.out = 120)
d <- dnorm(x, mean=mean(prod), sd=sd(prod))
hist(prod, main = "Histograma", xlab = "Produ??o (kg/6m?)", ylab = "Frequ?ncia",
col=c("lightgray"), prob=T)
lines (x, d, col = "darkblue")

## boxplot ##

(m?dia = mean(prod))
boxplot(prod, main = "Boxplot", ylab = "Produ??o (kg/6m?)", col=c("lightgray"))
points(m?dia, pch='x', cex=1.5, col='darkblue') 
                 

## __ AN?LISE DESCRITIVA POR TRATAMENTO __ ##

tapply(prod, esp, sum)
tapply(prod, esp, summary)
tapply(prod, esp, max)-tapply(prod, esp, min) ## amplitude(esp) = (max-min) ##
tapply(prod, esp, mean)
tapply(prod, esp, var)
tapply(prod, esp, sd)
(tapply(prod, esp, sd)/tapply(prod, esp, mean))*100  ## coeficiente de varia??o = sd/mean*100 ##
tapply(prod, esp, skewness)
tapply(prod, esp, kurtosis)

## histograma de cada tratamento ##

dados2 <- read.table("dados_lista1_produ??o_batata_doce_histograma_especies.txt", header = T)   ## lendo um conjunto de dados em txt, header = T tem nome das colunas na 1? linha ##
dados2

attach(dados2)


par(mfrow=c(2,3))

xB <- seq(20, 140, length.out=120)
dB <- dnorm(x, mean=mean(dados2$B), sd=sd(dados2$B)) 
hist(dados2$B, main = "Brazl?ndia", xlab = "Produ??o (kg/6m?)", 
ylab = "Frequ?ncia",
col=c("lightgray"), prob=T)
lines (xB, dB, col = "darkblue")

xJ <- seq(20, 140, length.out=120)
dJ <- dnorm(x, mean=mean(dados2$J), sd=sd(dados2$J)) 
hist(dados2$J, main = "Jacare?", xlab = "Produ??o (kg/6m?)", 
ylab = "Frequ?ncia",
col=c("lightgray"), prob=T)
lines (xJ, dJ, col = "darkblue")

xP <- seq(20, 140, length.out=120)
dP <- dnorm(x, mean=mean(dados2$P), sd=sd(dados2$P)) 
hist(dados2$P, main = "Paulistinha", xlab = "Produ??o (kg/6m?)", 
ylab = "Frequ?ncia",
col=c("lightgray"), prob=T)
lines (xP, dP, col = "darkblue")

xR <- seq(20, 140, length.out=120)
dR <- dnorm(x, mean=mean(dados2$R), sd=sd(dados2$R)) 
hist(dados2$R, main = "Rainha", xlab = "Produ??o (kg/6m?)", 
ylab = "Frequ?ncia",
col=c("lightgray"), prob=T)
lines (xR, dR, col = "darkblue")

xY <- seq(20, 140, length.out=120)
dY <- dnorm(x, mean=mean(dados2$Y), sd=sd(dados2$Y)) 
hist(dados2$Y, main = "Yellow Yam", xlab = "Produ??o (kg/6m?)", 
ylab = "Frequ?ncia",
col=c("lightgray"), prob=T)
lines (xY, dY, col = "darkblue")


## boxplot separado por tratamento ##

(m?dias = tapply(prod, esp, mean))
boxplot(prod ~ esp, main = "Boxplot - tratamentos", 
xlab = "Variedade de batata doce", ylab = "Produ??o (kg/6m?)", names = 
c("Brazl?ndia","Jacare?","Paulistinha","Rainha","Yellow Yam"), 
col=c("lightgray"))
points(m?dias, pch='x', cex=1.5, col='darkblue')



##----------------------------------------------##
## ____ ALTERNATIVA D ____ ##
##----------------------------------------------##

## AN?LISE DE VARI?NCIA ##

dados <- read.table("dados_lista1_produ??o_batata_doce.txt", header = T)   ## lendo um conjunto de dados em txt, header = T tem nome das colunas na 1? linha ##
dados

attach(dados)

esp <- as.factor(esp)       ## informa??o de que a esp ? um fator ##
anv <- aov(prod ~ esp)  ## c?lculo da an?lise de vari?ncia ##
anova(anv)                                ## demonstrativo da Tabela ANOVA ##



##----------------------------------------------##
## ____ ALTERNATIVA E ____ ##
##----------------------------------------------##

anv$residuals                   ## res?duos ##
cbind(dados,anv$residuals)

## AN?LISE DAS PRESSUPOSI??ES DO MODELO ##

## ____ normalidade _____ ##

# histogramas com a curva normal #
par(mfrow = c(1,2))
x <- seq(20, 140, length.out = 120)           ## o intervalo de valores foi verificado no histograma ##
d <- dnorm(x, mean = mean(prod), sd = sd(prod))
hist(prod, main = "Histograma vari?vel resposta", xlab = "Produ??o (kg/6m?)", ylab = "Frequ?ncia", prob = T, col=c('lightgrey'))
lines(x,d, col = "darkblue")

xres <- seq(-20, 30, length.out = 50)           ## o intervalo de valores foi verificado no histograma ##
dres <- dnorm(xres, mean = mean(anv$residuals), sd = sd(anv$residuals))
hist(anv$residuals, main = "Histograma res?duos", xlab = "Produ??o (kg/6m?)", ylab = "Frequ?ncia", prob = T, col=c('lightgrey'))
lines(xres,dres, col = "darkblue")

# Gr?ficos QQ-plot #

par(mfrow = c(1,2))     ## abre uma janela gr?fica com espa?o para 2 gr?ficos ##

qqnorm(prod, main = "QQ-plot Produ??o", 
ylab="Quantis da amostra", xlab="Quantis te?ricos")
qqline(prod, col = "darkblue")

qqnorm(anv$residuals, main = "QQ-plot Res?duo",
ylab="Quantis da amostra", xlab="Quantis te?ricos")
qqline(anv$residuals, col = "darkblue")

# teste de Shapiro-Wilk #

shapiro.test(prod)    ## teste de normalidade de Shapiro Wilk ##
shapiro.test(anv$residuals)    ## teste de normalidade de Shapiro Wilk ##


## __ homogeneidade das vari?ncias __ ##

# gr?fico de dispers?o res?duos x m?dias #

n <- length(prod)                         ## n? total de parcelas
I <- length(levels(esp))                  ## n? de tratamentos
r <- n/I                                  ## n? de repeti??es

repmean <- rep(tapply(prod, esp, mean), each = r)
plot(repmean,anv$residuals, xlab = "M?dias dos tratamentos", ylab = "Res?duos")

# boxplot para residuos/tratamento #

boxplot(anv$residuals ~ esp, main = "Boxplot - res?duos", 
xlab = "Variedade de batata doce", ylab = "Res?duos", names = 
c("Brazl?ndia","Jacare?","Paulistinha","Rainha","Yellow Yam"), 
col=c("lightgray"))


# testes estat?sticos #

bartlett.test(prod ~ esp)   ## teste de Bartlett: homogeneidade das vari?ncias ##


## __ independ?ncia __ ##

plot(1:n, anv$residuals, xlab = "Ordem da coleta", ylab = "Res?duos")

## an?lise dos res?duos padronizados: avalia??o de outliers ##

res <- resid(anv) 
res

respad <- res/sqrt(sum(res^2)/anv$df.res)   ## res?duos padronizados ##
respad

par(mfrow = c(1,2))
boxplot(respad, ylab = "Res?duos padronizados", main = "Boxplot dos Res. padronizados") 
hist(respad, main = "Histograma dos Res. padronizados")



##----------------------------------------------##
## ____ ALTERNATIVA F ____ ##
##----------------------------------------------##

## Teste Tukey ##

qtukey(0.95,5,15)                           ## valor da tabela de Tukey ##
qtabtukey <- qtukey(0.95,5,15)

dms <- (qtabtukey*(sqrt(108.97/4)))       ## valor do dms ##
dms

TukeyHSD(anv)                               ## teste Tukey, sem pacote "laercio" ##

## gr?fico do intervalo de confian?a para as diferen?as m?dias ##
plot(TukeyHSD(anv,wich='esp',ordered=TRUE,conf.level=0.95))

require(laercio)                            ## pacote laercio ##
LTukey(anv,"esp")                         ## teste Tukey, default = 5%, com pacote "laercio" ##

## gr?fico em colunas com a compara??o de m?dias ##

espe <- c("Brazl?ndia","Jacare?","Paulistinha","Rainha","Yellow Yam")
espe <- as.factor(espe)
medi <- c(45,68.4,49.4,99.975,74.1)
medi

let <- c("d","bc","cd","a","b")
let <- as.vector(let)
max(medi)

die <- data.frame(espe, medi, let)

require(lattice)

barchart(medi~espe, data=die, horiz=FALSE,
         ylab="M?dia da Produ??o", ylim=c(0,110),
         xlab="Variedade de Batata Doce",col = "lightgray",
         panel=function(x, y, subscripts, ...){
           panel.barchart(x, y, subscripts=subscripts, ...)
           panel.text(x, y, label=die[subscripts,"let"], pos=3, cex = 1)
})



##----------------------------------------------##
## ____ ALTERNATIVA G ____ ##
##----------------------------------------------##

# TESTE DUNNETT: COMPARA??O DOS TRATAMENTOS COM O CONTROLE #
# SEMPRE CONSIDERA QUE O 1? TRATAMENTO DO BANCO DE DADOS ? O CONTROLE #

require(multcomp)

esp <- as.factor(esp)
anv <- aov(prod ~ esp)

ht <- glht(anv, linfct = mcp(esp = "Dunnett"))  ## o trat 1 no banco de dados sempre ser? a testemunha ##
summary(ht)
confint(ht, level = 0.95)

plot(ht, xlim=c(-30,90), xlab="Intervalos de Confian?a",
main="Compara??o com Grupo Controle
(95% de n?vel de confian?a)")                   ## gr?fico do intervalo de confian?a para a diferen?a m?dia ##
# obs.: intervalo que cont?m o zero, ent?o o tratamento e o controle s?o iguais #



##----------------------------------------------##
## ____ ALTERNATIVA H ____ ##
##----------------------------------------------##

## Contrastes Ortogonais ##

# C1 = 2B + 2J + 2P - 3R - 3Y
# C2 = 1B + 1J - 2P + 0R + 0Y
# C3 = 1B - 1J + 0J + 0R + 0Y
# C4 = 0B + 0J + 0P + 1R - 1Y

dados <- read.table("dados_lista1_produ??o_batata_doce.txt", header = T)   ## lendo um conjunto de dados em txt, header = T tem nome das colunas na 1? linha ##
dados

attach(dados)

con1 <- matrix(c(2,2,2,-3,-3,1,1,-2,0,0,1,-1,0,0,0,0,0,0,1,-1),
nrow = 5, ncol = 4)
con1

esp <- as.factor(esp)       ## informa??o de que a esp?cie ? um fator #

contrasts(esp) <- con1
contrasts(esp)

anv <- aov(prod ~ esp)  ## c?lculo da an?lise de vari?ncia ##
anova(anv) 

summary.aov(anv, split=list("esp"=list("c1" = 1, "c2" = 2, "c3" = 3, "c4" = 4)))

##----------------------------------------------##
##----------------------------------------------##
##--------------------Fim-----------------------##