# Grupo 7: Gabriel Almeida, Lucas Capris, Victor Azevedo
#Sessão 2 Conceitos de Probabilidade e estat´ıstica
#===================================[ 01 ]===================================
moedas <- c("cara", "coroa")

resultado_10 <- sample(moedas, size = 10, replace = TRUE)
resultado_100 <- sample(moedas, size = 100, replace = TRUE)
resultado_1000 <- sample(moedas, size = 1000, replace = TRUE)
resultado_10000 <- sample(moedas, size = 10000, replace = TRUE)

qnt_cara_10 <- sum(resultado_10 == "cara")
qnt_coroa_10 <- sum(resultado_10 == "coroa")
relacao_10 <- c(qnt_cara_10, qnt_coroa_10)

qnt_cara_100 <- sum(resultado_100 == "cara")
qnt_coroa_100 <- sum(resultado_100 == "coroa")
relacao_100 <- c(qnt_cara_100, qnt_coroa_100)

qnt_cara_1000 <- sum(resultado_1000 == "cara")
qnt_coroa_1000 <- sum(resultado_1000 == "coroa")
relacao_1000 <- c(qnt_cara_1000, qnt_coroa_1000)

qnt_cara_10000 <- sum(resultado_10000 == "cara")
qnt_coroa_10000 <- sum(resultado_10000 == "coroa")
relacao_10000 <- c(qnt_cara_10000, qnt_coroa_10000)

lancamentos.df <- data.frame("Moedas"=moedas,"Lançamentos 10"=relacao_10, "Lançamentos 100"=relacao_100, "Lançamentos 1000"=relacao_1000, "Lançamentos 10000"=relacao_10000)

#===================================[ 02 ]===================================
qnt_variaveis_aleatorias <- 12
gerador_de_variaveis <- runif(qnt_variaveis_aleatorias)
media_variaveis <- mean(gerador_de_variaveis)
variaveis.df <- data.frame(gerador_de_variaveis, media_variaveis)

#===================================[ 02A ]===================================
media <- media_variaveis*10 # ou media = 6
variancia <- 12 * (media_variaveis ^ 2 / 12) # ou variancia = 1
valores_grafico <- seq(0, 12, length = 1000)
pdf <- dnorm(x, mean = media, sd = sqrt(variancia))

media_distribuicao <- mean(pdf)
variancia_distribuicao <- var(pdf)
cat("Média da distribuição normal:", media_distribuicao, "\n")
cat("Variância da distribuição normal:", variancia_distribuicao, "\n")

plot(valores_grafico, pdf, type = "l", main = "Distribuição Normal Aproximada da Soma", xlab = "Valor da Soma", ylab = "Densidade de Probabilidade")

#===================================[ 02B ]===================================
num_simulacoes <- 10000
n <- 12
somas <- numeric(num_simulacoes)

for (i in 1:num_simulacoes) {
  soma <- sum(runif(n))
  somas[i] <- soma
}

media_simulada <- mean(somas)
variancia_simulada <- var(somas)
hist(somas, breaks = 30, main = "Distribuição da Soma (Monte Carlo)", xlab = "Valor da Soma", ylab = "Frequência")
cat("Média simulada:", media_simulada, "\n")
cat("Variância simulada:", variancia_simulada, "\n")

#===================================[ 05 ]===================================
variaveis <- c(2, 5, 10)
num_simulacoes <- 100000
valores_maximo <- matrix(0, nrow = num_simulacoes, ncol = length(variaveis))

for (j in 1:length(variaveis)) { # nolint
  i <- variaveis[j]
  for (k in 1:num_simulacoes) {
    simulacoes_aleatorias <- rnorm(i)
    valor_maximo <- max(simulacoes_aleatorias)
    valores_maximo[k, j] <- valor_maximo
  }
}

par(mfrow=c(1, length(variaveis)))
for (j in 1:length(variaveis)) {
  hist(valores_maximo[, j], main = paste("i =", variaveis[j]), xlab = "Máximo", prob = TRUE)
  lines(density(valores_maximo[, j]), col = "blue")
}

#===================================[ 06 ]===================================
n_max <- 10 #n arbitrário
resultados <- matrix(0, nrow = qnt_simulacoes, ncol = n_max)

for (i in 1:qnt_simulacoes) {
  X <- matrix(rnorm(n_max), ncol = n_max)
  x_squared <- cumsum(X^2)
  resultados[i, ] <- x_squared
}

par(mfrow = c(2, 5))
for (n in 1:n_max) {
  hist(resultados[, n], breaks = 30, main = paste("n =", n),
       xlab = "Valor de x^2(n)", ylab = "Frequência")
}

#===================================[ 07 ]===================================
lambda <- 0.5
num_amostras <- 1000
N <- rexp(num_amostras, rate = lambda)
Z <- exp(N)

hist(Z, breaks = 30, main = "Aproximação Empírica da Função de Probabilidade Z = e^N", xlab = "Valor de Z", ylab = "Frequência")
media_empirica <- mean(Z)
desvio_padrao_empirico <- sd(Z)
cat("Média Empírica:", media_empirica, "\n")
cat("Desvio Padrão Empírico:", desvio_padrao_empirico, "\n")