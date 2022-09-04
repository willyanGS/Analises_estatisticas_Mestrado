##########################################################
### 1a LISTA DE EXERCÍCIOS - ANÁLISE MULTIVARIADA 2018 ###
##########################################################


	##### Questao 2 #####

A <-  matrix(c(9,-5,-5,6), ncol = 2, nrow = 2) 
A

## Alternativa (a) ##
a <- t(A)
a

## xt * A * x 



## Alternativa (b) ##

eigen(A)


## Alternativa (c) ##

Avet <- matrix(c(-0.8023, 0.5969, -0.5969, -0.8023), ncol=2, nrow=2) 
Avet

Aval <- matrix(c(12.7202, 0, 0, 2.2798), ncol=2, nrow=2)
Aval

Adecomp <- Avet %*% Aval %*% t(Avet)
Adecomp

## Alternativa (d) ##

solve(A)
Ainv <- solve(A)
Ainv
eigen(Ainv)