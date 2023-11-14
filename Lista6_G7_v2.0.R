# Grupo 7: Gabriel Almeida, Lucas Capris, Victor Azevedo
# Sessão 6 Modelagem da correlação
q1 <- function() {
  atividades <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  descricao <- c("Obter materiais", "Obter mão de obra", "Escavar A", "Colocar fundação C", "Construir estrutura B,D", "Instalação Hidráulica E", "Instalação Elétrica E", "Acabamento interior F,G", "Acabamento Exterior F", "Limpeza Local H,I")
  pred <- c(2, 5, 4, 8, 44, 30, 9, 24, 28, 10)
  DMin <- c(4, 9, 10, 13, 60, 40, 20, 30, 29, 10)
  DMp <- c(18, 19, 28, 36, 100, 74, 43, 48, 96, 12)
  DMax <- c(300, 480, 3750, 8400, 300000, 37650, 10500, 36000, 48750, 540)
  CMin <- c(450, 600, 3750, 8400, 300000, 37650, 10500, 36000, 48750, 360)
  CMp <- c(600, 720, 4500, 9600, 312000, 39600, 11550, 38400, 52500, 450)
  CMax <- c(600, 720, 5250, 10800, 322500, 41400, 12600, 40800, 56250, 540)
  
  correlation_matrix <- matrix(c(
    1.0, 0.9, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.9, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 1.0, 0.85, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.85, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.9, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.9, 1.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.85, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.85, 1.0, 0.85,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.85, 1.0
  ), ncol = 10, byrow = TRUE)
  
  # Função de simulação Monte Carlo para o risco de custo
  simular_risco_custo <- function(iterations) {
    custo_simulado <- numeric(iterations)
    for (i in 1:iterations) {
      tempo_simulado <- numeric(length(atividades))
      for (j in 1:length(atividades)) {
        if (j == 1) {
          tempo_simulado[j] <- rnorm(1, mean = pred[j], sd = (DMax[j] - pred[j]) / 3)
        } else {
          tempo_simulado[j] <- rnorm(1, mean = pred[j], sd = (DMax[j] - pred[j]) / 3) +
            correlation_matrix[j, 1:(j - 1)] %*% (tempo_simulado[1:(j - 1)] - pred[1:(j - 1)])
        }
      }
      custo_simulado[i] <- sum(CMin + (CMp - CMin) * (tempo_simulado - DMin) / (DMp - DMin))
    }
    return(custo_simulado)
  }
  
  # Função de simulação Monte Carlo para o risco de prazo
  simular_risco_prazo <- function(iterations) {
    prazo_simulado <- numeric(iterations)
    for (i in 1:iterations) {
      tempo_simulado <- numeric(length(atividades))
      for (j in 1:length(atividades)) {
        if (j == 1) {
          tempo_simulado[j] <- rnorm(1, mean = pred[j], sd = (DMax[j] - pred[j]) / 3)
        } else {
          tempo_simulado[j] <- rnorm(1, mean = pred[j], sd = (DMax[j] - pred[j]) / 3) +
            correlation_matrix[j, 1:(j - 1)] %*% (tempo_simulado[1:(j - 1)] - pred[1:(j - 1)])
        }
      }
      prazo_simulado[i] <- max(tempo_simulado)
    }
    return(prazo_simulado)
  }
  
  set.seed(123)  # Definir uma semente para reproduzibilidade
  num_iterations <- 10000  
  custo_simulado <- simular_risco_custo(num_iterations)
  prazo_simulado <- simular_risco_prazo(num_iterations)
  
  summary(custo_simulado)
  summary(prazo_simulado)
}

q2 <- function() {
  # Dados das atividades
  atividades <- data.frame(
    Atividade = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
    Descricao = c("Obter materiais", "Obter mão de obra", "Escavar A", "Colocar fundação C", "Construir estrutura B,D", "Instalação Hidráulica E", "Instalação Elétrica E", "Acabamento interior F,G", "Acabamento Exterior F", "Limpeza Local H,I"),
    Pred = c("", "", "A", "C", "B,D", "E", "E", "F,G", "F", "H,I"),
    DMin = c(2, 5, 4, 8, 44, 30, 9, 24, 28, 10),
    DMp = c(4, 9, 10, 13, 60, 40, 20, 30, 29, 10),
    DMax = c(18, 19, 28, 36, 100, 74, 43, 48, 96, 12),
    CMin = c(300, 480, 3.750, 8.400, 300.000, 37.650, 10.500, 36.000, 48.750, 360),
    CMp = c(450, 600, 4.500, 9.600, 312.000, 39.600, 11.550, 38.400, 52.500, 450),
    CMax = c(600, 720, 5.250, 10.800, 322.500, 41.400, 12.600, 40.800, 56.250, 540)
  )
  
  # Coeficientes de correlação
  coef_corr <- matrix(1, nrow = 10, ncol = 10)
  coef_corr[1:2, 1:2] <- 0.9
  coef_corr[4:5, 4:5] <- 0.85
  coef_corr[6:7, 6:7] <- 0.9
  coef_corr[8:10, 8:10] <- 0.85
  
  # Função para calcular probabilidades
  calcular_probabilidades <- function(atividades, coef_corr) {
    n <- nrow(atividades)
    probabilidade <- rep(0, n)

    atividades$MT <- atividades$DMax - atividades$DMin
    atividades$ML <- atividades$DMp - atividades$DMin
    
    # Calcular as probabilidades
    for (i in 1:n) {
      prob_caminho_critico <- 1
      for (j in 1:n) {
        if (coef_corr[i, j] > 0) {
          prob_caminho_critico <- prob_caminho_critico * (1 - pnorm(atividades$DMax[i], mean = atividades$DMp[j], sd = atividades$DMp[j] - atividades$DMin[j]) / (atividades$DMax[j] - atividades$DMin[j]))
        }
      }
      probabilidade[i] <- 1 - prob_caminho_critico
    }
    return(probabilidade)
  }
  
  probabilidades <- calcular_probabilidades(atividades, coef_corr)
  atividades$ProbabilidadeCaminhoCritico <- probabilidades
  print(atividades)
}

q3 <- function(){  
  # Dados do projeto da obra
  atividades <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  pred <- c(2, 5, 4, 8, 44, 30, 9, 24, 28, 10)
  DMin <- c(4, 9, 10, 13, 60, 40, 20, 30, 29, 10)
  DMp <- c(18, 19, 28, 36, 100, 74, 43, 48, 96, 12)
  DMax <- c(18, 19, 28, 36, 100, 74, 43, 48, 96, 12)
  CMin <- c(300, 480, 3.750, 8.400, 300.000, 37.650, 10.500, 36.000, 48.750, 360)
  CMp <- c(450, 600, 4.500, 9.600, 312.000, 39.600, 11.550, 38.400, 52.500, 450)
  CMax <- c(600, 720, 5.250, 10.800, 322.500, 41.400, 12.600, 40.800, 56.250, 540)
  
  # Tabela de correlações
  correlacoes <- matrix(0, nrow = length(atividades), ncol = length(atividades))
  rownames(correlacoes) <- atividades
  colnames(correlacoes) <- atividades
  correlacoes["A", "B"] <- 0.9
  correlacoes["B", "A"] <- 0.9
  correlacoes["D", "E"] <- 0.85
  correlacoes["E", "D"] <- 0.85
  correlacoes["F", "G"] <- 0.90
  correlacoes["G", "F"] <- 0.90
  correlacoes["H", "I"] <- 0.85
  correlacoes["I", "H"] <- 0.85
  
  # Cálculo do prazo e custo para cada atividade
  prazo <- DMax
  custo <- CMax
  for (i in 1:length(atividades)) {
    for (j in 1:length(atividades)) {
      if (correlacoes[atividades[i], atividades[j]] > 0) {
        prazo[i] <- prazo[i] + correlacoes[atividades[i], atividades[j]] * (DMax[j] - DMin[j])
        custo[i] <- custo[i] + correlacoes[atividades[i], atividades[j]] * (CMax[j] - CMin[j])
      }
    }
  }
  
  # Gráfico de dispersão
  plot(prazo, custo, main = "Correlação entre Prazo e Custo da Obra", xlab = "Prazo", ylab = "Custo")
}

