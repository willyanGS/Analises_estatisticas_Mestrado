##########################################################
### 1a LISTA DE EXERCÍCIOS - ANÁLISE MULTIVARIADA 2018 ###
##########################################################



	##### Questao 1 #####

A <- matrix(c(1,1,0,2,1,1,3,2,2), ncol = 3, nrow = 3)
B <- matrix(c(1,0,1,2,2,2,3,3,4), ncol = 3, nrow = 3)
A
B

### Alternativa a) ###
A-3*B


### Alternativa b) ###
A%*%B
det(A%*%B)

### Alternativa c) ###
kronecker(A,B)

### Alternativa d) ###
C <- matrix (c(1,1,0,0,0,0,2,1,1,0,0,0,3,2,2,0,0,0,0,0,0,1,0,1,0,0,0,2,2,2,0,0,0,3,3,4), ncol = 6, nrow= 6)
C

### Alternativa e) ###
D <- solve(B)
D
B%*%D

### Alternativa f) ###
KhatriRao(A,B)
