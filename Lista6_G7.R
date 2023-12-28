# Grupo 7: Gabriel Almeida, Lucas Capris, Victor Azevedo
# Sessão 6 Modelagem da correlação
q1eq4 <- function() {
  atividades <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  pred <- c(2, 5, 4, 8, 44, 30, 9, 24, 28, 10)
  DMin <- c(4, 9, 10, 13, 60, 40, 20, 30, 29, 10)
  DMp <- c(18, 19, 28, 36, 100, 74, 43, 48, 96, 12)
  DMax <- c(300, 480, 3750, 8400, 300000, 37650, 10500, 36000, 48750, 450)
  CMin <- c(300, 480, 3750, 8400, 300000, 37650, 10500, 36000, 48750, 360)
  CMp <- c(450, 600, 4500, 9600, 312000, 39600, 11550, 38400, 52500, 450)
  CMax <- c(600, 720, 5250, 10800, 322500, 41400, 12600, 40800, 56250, 540)

  coef_correlacao <- matrix(c(
    1.00, 0.90, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    0.90, 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    0.00, 0.00, 1.00, 0.85, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    0.00, 0.00, 0.85, 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    0.00, 0.00, 0.00, 0.00, 1.00, 0.90, 0.00, 0.00, 0.00, 0.00,
    0.00, 0.00, 0.00, 0.00, 0.90, 1.00, 0.00, 0.00, 0.00, 0.00,
    0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 0.85, 0.00, 0.00,
    0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.85, 1.00, 0.00, 0.00,
    0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 0.85,
    0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.85, 1.00
  ), nrow = 10, byrow = TRUE)

  n_simulacoes <- 10000

  # Crie vetores para armazenar os resultados da simulação
  custos <- numeric(n_simulacoes)
  prazos <- numeric(n_simulacoes)

  # Monte Carlo
  for (i in 1:n_simulacoes) {
    valores_aleatorios <- matrix(rnorm(10), ncol = 1)
    valores <- pred + (DMp - DMin) * valores_aleatorios  # Calcule os valores das atividades
    custo_total <- sum(CMin + (CMp - CMin) * valores_aleatorios)
    prazo_total <- max(valores)
    custos[i] <- custo_total
    prazos[i] <- prazo_total
  }

  # Calcule as aproximações empíricas para o risco de custo e prazo
  risco_custo <- quantile(custos, c(0.1, 0.5, 0.9))
  risco_prazo <- quantile(prazos, c(0.1, 0.5, 0.9))

  print("Aproximações Empíricas para o Risco de Custo:")
  print(risco_custo)
  print("Aproximações Empíricas para o Risco de Prazo:")
  print(risco_prazo)
}

q2 <- function() {
  atividades <- data.frame(
    Atividade = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
    DMin = c(2, 5, 4, 8, 44, 30, 9, 24, 28, 10),
    DMp = c(4, 9, 10, 13, 60, 40, 20, 30, 29, 10),
    DMax = c(18, 19, 28, 36, 100, 74, 43, 48, 96, 12)
  )

  # Cálculo do tempo médio e variância para cada atividade
  atividades$TE <- (atividades$DMin + 4 * atividades$DMp + atividades$DMax) / 6
  atividades$Var <- ((atividades$DMax - atividades$DMin) / 6) ^ 2

  coef_correlacao <- matrix(1, nrow = nrow(atividades), ncol = nrow(atividades))
  diag(coef_correlacao) <- 1
  coef_correlacao[1, 2] <- 0.9
  coef_correlacao[4, 5] <- 0.85
  coef_correlacao[6, 7] <- 0.90
  coef_correlacao[c(8, 9, 10), c(8, 9, 10)] <- 0.85

  probabilidades_caminho_critico <- rep(0, nrow(atividades))
  probabilidades_caminho_critico[1] <- 1

  for (i in 2:nrow(atividades)) {
    prob_caminho_i <- 0
    for (j in 1:(i - 1)) {
      prob_caminho_i <- prob_caminho_i + probabilidades_caminho_critico[j] * coef_correlacao[j, i]
    }
    probabilidades_caminho_critico[i] <- prob_caminho_i
  }

  print(probabilidades_caminho_critico)
}

q3 <- function() {
  atividades <- data.frame(
    Atividade = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
    DMin = c(4, 9, 10, 13, 60, 40, 20, 30, 29, 10),
    DMax = c(18, 19, 28, 36, 100, 74, 43, 48, 96, 12),
    CMin = c(300, 480, 3750, 8400, 300000, 37650, 10500, 36000, 48750, 360),
    CMax = c(600, 720, 5250, 10800, 322500, 41400, 12600, 40800, 56250, 540)
  )

  # Calcular valores médios de prazo e custo para cada atividade
  atividades$DMedio <- (atividades$DMin + atividades$DMax) / 2
  atividades$CMedio <- (atividades$CMin + atividades$CMax) / 2

  plot(atividades$DMedio, atividades$CMedio, 
      main = "Correlação entre Prazo e Custo da Obra",
      xlab = "Prazo Médio",
      ylab = "Custo Médio",
      pch = 19, col = "blue")

  # Adicionar rótulos das atividades ao gráfico
  text(atividades$DMedio, atividades$CMedio, labels = atividades$Atividade, pos = 3)

  # Adicionar linha de tendência (correlação)
  correlation <- A(atividades$DMedio, atividades$CMedio)
  abline(lm(atividades$CMedio ~ atividades$DMedio), col = "red")
  legend("topright", legend = paste("Correlação =", round(correlation, 2)), col = "red", bty = "n")
}
 