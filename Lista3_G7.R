# Grupo 7: Gabriel Almeida, Lucas Capris, Victor Azevedo
# Sessao 3- Risco de Custo
library(triangle) # Instalar e importar biblioteca triangle

q1 <- function() {
  num_simulacoes <- 1000
  resultados <- numeric(num_simulacoes)
  
  # Questão 1.1
  # Parâmetros da distribuição triangular para o tempo gasto por placa
  tempo_minimo <- 3.75  # 3 horas e 45 minutos
  tempo_maximo <- 5.5   # 5 horas e 30 minutos
  tempo_mais_frequente <- 4.25  # 4 horas e 15 minutos
  
  # Número de placas metálicas
  numero_placas <- 562
  
  # Taxa horária do rebitador (USD$7.50 por hora trabalhada)
  taxa_horaria <- 7.5
  
  for (i in 1:num_simulacoes) {
    # Amostras aleatórias para o tempo gasto por placa
    tempo_por_placa <- triangle::rtriangle(numero_placas, a = tempo_minimo, b = tempo_maximo, c = tempo_mais_frequente)
    
    # Custo de mão de obra para cada placa
    custo_mao_de_obra_por_placa <- tempo_por_placa * taxa_horaria
    
    # Custo total de mão de obra para todas as placas
    custo_total_mao_de_obra <- sum(custo_mao_de_obra_por_placa)
    resultados[i] <- custo_total_mao_de_obra
  }
  
  hist(resultados, main = "Distribuição de Custos de Mão de Obra Simulados", xlab = "Custo de Mão de Obra")

  # Questão 1.2
  # Simulação de Monte Carlo
  custo_total_mc <- numeric(num_simulacoes)
  for (i in 1:num_simulacoes) {
    tempo_total <- sample(tempo_por_placa, numero_placas, replace = TRUE)
    custo_total_mc[i] <- sum(tempo_total) * taxa_horaria
  }
  
  # Teorema do Limite Central (TCL)
  media_tempo <- mean(tempo_por_placa)
  desvio_padrao_tempo <- sd(tempo_por_placa)
  custo_total_tcl <- numeric(num_simulacoes)
  for (i in 1:num_simulacoes) {
    tempo_total <- rnorm(numero_placas, media_tempo, desvio_padrao_tempo)
    custo_total_tcl[i] <- sum(tempo_total) * taxa_horaria
  }
  
  # Visualização da comparação usando qqnorm
  par(mfrow = c(1, 2))
  qqnorm(custo_total_mc, main = "MC - Força Bruta")
  qqline(custo_total_mc)
  qqnorm(custo_total_tcl, main = "TCL")
  qqline(custo_total_tcl)
}

q2 <- function() {
  # Parâmetros
  num_simulacoes <- 10000
  carros_ativos <- c(15, 20, 18)
  consumo_litros <- c(40, 60, 58)
  custos_litro <- c(6.0, 7.20, 5.7)
  
  # Função do cálculo do gasto diário
  calcular_gasto_diario <- function() {
    carros_usados <- sample(carros_ativos, 1)
    consumo_total <- sample(consumo_litros, carros_usados, replace = TRUE)
    custo_total <- sum(consumo_total) * sample(custos_litro, 1)
    return(custo_total)
  }
  
  # Simulação de Monte Carlo
  gastos_diarios_simulados <- numeric(num_simulacoes)
  for (i in 1:num_simulacoes) {
    gastos_diarios_simulados[i] <- calcular_gasto_diario()
  }
  
  media_gastos <- mean(gastos_diarios_simulados)
  desvio_padrao_gastos <- sd(gastos_diarios_simulados)
  
  hist(gastos_diarios_simulados, breaks = 20, 
       main = "Distribuição dos Gastos Diários", 
       xlab = "Custo Diário (em Reais)", 
       ylab = "Frequência")
  cat("Média de gastos diários: R$", round(media_gastos, 2), "\n")
  cat("Desvio padrão de gastos diários: R$", round(desvio_padrao_gastos, 2), "\n")
}

q3 <- function() {
  num_simulacoes <- 1000
  resultados <- numeric(num_simulacoes)
  
  for (i in 1:num_simulacoes) {
    # Número de sextas-feiras úteis em um ano (40, 41 ou 42)
    sextas_feiras <- sample(40:42, 1)
    
    # Número de executivos (distribuição triangular entre 16 e 22, com 18 como valor mais provável)
    numero_executivos <- round(triangle::rtriangle(n = 1, a = 16, b = 22, c = 18))
    
    # Custo por executivo (distribuição triangular entre US$ 25 e US$ 36, com US$ 28 como valor mais provável)
    custo_executivo <- triangle::rtriangle(n = numero_executivos, a = 25, b = 36, c = 28)
    
    # Gasto total em um almoço
    gasto_total_almoco <- sum(custo_executivo)
    
    # Gasto total em um ano
    gasto_anual <- sextas_feiras * gasto_total_almoco
    resultados[i] <- gasto_anual
  }
  hist(resultados, main = "Distribuição de Gastos Anuais Simulados", xlab = "Gasto Anual")
}

q4 <- function(){
  # Parâmetros
  num_simulacoes <- 100  # Tamanho da amostra
  dias <- 30
  meses <- 12
  selic <- 0.8675 # Taxa Selic atual = 13,25%
  resultados <- numeric(num_simulacoes)
  lucro_mensal <- numeric(dias)
  lucro_anual <- numeric(meses)
  
  for (i in 1:num_simulacoes) {
    for(j in 1:meses){
      for(k in 1:dias){
        # Distribuição triangular da quantidade de grupos
        qtd_de_grupos <- round(triangle::rtriangle(n = 1, a = 40, b = 120, c = 60))
        
        # Distribuição triangular do gasto de cada grupo em um vetor
        gasto_por_grupo <- round(triangle::rtriangle(n = qtd_de_grupos, a = 90, b = 250, c = 130))
        
        # Distribuição triangular da % de lucro que será aplicada em cada grupo em um vetor
        lucro_parcial <- triangle::rtriangle(n = qtd_de_grupos, a = 0.15, b = 0.30, c = 0.22)
        
        # Cálculo do lucro para cada grupo
        lucro_por_grupo <- gasto_por_grupo * lucro_parcial
        
        # Lucro diário sendo a soma do grupo de cada grupo
        lucro_diario <- sum(lucro_por_grupo)
        
        # Lucro mensagel sendo um vetor contendo cada lucro diário
        lucro_mensal[k] <- lucro_diario
      }
      # Aplicação da taxa Selic
      lucro_mensal <- lucro_mensal * selic
      
      # Lucro anual sendo um vetor do lucro de cada mês do ano
      lucro_anual[j] <- sum(lucro_mensal)
      
      # Cálculo do lucro anual, somando o lucro de cada mês
      valor_presente_do_lucro_total <- sum(lucro_anual)
    }
    resultados[i] <- valor_presente_do_lucro_total
  }
  hist(resultados, main = "Distribuição de Lucros Totais Simulados", xlab = "Lucro Anual")
}