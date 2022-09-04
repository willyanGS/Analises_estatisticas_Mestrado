##########################################################
### 1a LISTA DE EXERCÍCIOS - ANÁLISE MULTIVARIADA 2018 ###
##########################################################


	##### Questao 5 #####

x <- read.table("questao5_dados.txt", header = T)
x

attach(x)

## ANOVA ##

SEXO <- as.factor(SEXO)
anvC <- aov(C ~ SEXO)
anvL <- aov(L ~ SEXO)
anvP <- aov(P ~ SEXO)
summary(anvC)
summary(anvL)
summary(anvP)

# Resumo Anova #
summary.aov(fit)


## MANOVA ##
# test = c("Pillai" (defualt), "Wilks", "Hotelling-Lawley", "Roy") #

var <- as.matrix(x[,2:4])
var
fit <- manova(var ~ SEXO)
fit

# Resumo Manova #
summary.manova(fit)
summary.manova(fit, test = "Wilks")
cor(var)

    

