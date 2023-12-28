# Grupo 7: Gabriel Almeida, Lucas Capris, Victor Azevedo

q1 <- function() {
  if (!require(mvtnorm)) {
    install.packages("mvtnorm")
    library(mvtnorm)
  }
  
  # Parâmetros
  total_investment = 10  # Total de investimento em milhões de dólares
  num_assets = 10       # Número de ativos na carteira
  mean_return = 1       # Média do retorno em milhões de dólares
  std_deviation = 1.3   # Desvio padrão do retorno em milhões de dólares
  correlation_values = c(0, 0.25, 0.50, 0.75, 0.90)
  num_simulations = 10000
  
  # Vetor para armazenar as probabilidades de perda
  loss_probabilities = numeric(length(correlation_values))
  
  for (i in 1:length(correlation_values)) {
    correlation = correlation_values[i]
    losses = numeric(num_simulations)
    
    # Cria a matriz de correlação
    correlation_matrix = matrix(correlation, nrow = num_assets, ncol = num_assets)
    diag(correlation_matrix) = 1
    
    for (j in 1:num_simulations) {
      returns = rmvnorm(n = num_assets, mean = rep(mean_return, num_assets), sigma = correlation_matrix * (std_deviation^2))
      portfolio_return = sum(returns)
      
      if (portfolio_return < total_investment) {
        losses[j] = 1
      } else {
        losses[j] = 0
      }
    }
    
    # Calcula a probabilidade de perda
    loss_probabilities[i] = mean(losses)
  }
  # Exibe as probabilidades de perda para cada valor de correlação
  for (i in 1:length(correlation_values)) {
    cat(paste("Correlação =", correlation_values[i], "Probabilidade de perda =", loss_probabilities[i], "\n"))
  }
}

q2 <- function() {
  # Carregue as bibliotecas
  library(quantmod)
  library(cointReg)
  library(corpcor)
  
  # Defina os símbolos das ações
  ativos <- c("PETR4.SA", "VALE3.SA", "BBAS3.SA")
  
  # Defina a data de início e término
  data_inicio <- Sys.Date() - 365 # Há 365 dias
  data_fim <- Sys.Date()
  
  # Baixe os dados do Yahoo Finance
  getSymbols(ativos, from = data_inicio, to = data_fim)
  
  # Extraia os preços de fechamento ajustados
  precos_fechamento <- merge(Ad(get(ativos[1])), Ad(get(ativos[2])), Ad(get(ativos[3])))
                             
  # Calcule os retornos mensais
  retornos_mensais <- to.monthly(precos_fechamento)
  
  # Crie um modelo CER para cada ativo
  modelos_cer <- list()
  for (i in 1:length(ativos)) {
   retorno_ativo <- retornos_mensais[, i]
   mu <- mean(retorno_ativo)
   sigma_sq <- var(retorno_ativo)
   epsilon <- rnorm(length(retorno_ativo), mean = 0, sd = sqrt(sigma_sq))
   modelo_cer <- mu + epsilon
   modelos_cer[[i]] <- modelo_cer
  }
  
  # Exiba os modelos CER
  for (i in 1:length(ativos)) {
   cat("Modelo CER para", ativos[i], ":\n")
   print(modelos_cer[[i]])
   cat("\n")
  }
  
  # Defina os símbolos das ações
  ativos <- c("PETR4.SA", "VALE3.SA", "BBAS3.SA")
  
  # Defina a data de início e término
  data_inicio <- Sys.Date() - 365 # Há 365 dias
  data_fim <- Sys.Date()
  
  # Baixe os dados do Yahoo Finance
  getSymbols(ativos, from = data_inicio, to = data_fim)
  
  # Extraia os preços de fechamento ajustados
  precos_fechamento <- merge(Cl(PETR4.SA), Cl(VALE3.SA), Cl(BBAS3.SA))
  
  # Crie um modelo CER para cada par de ativos usando o método "FM"
  cer_model_pet_vale <- cointReg(precos_fechamento$PETR4.SA, precos_fechamento$VALE3.SA, method = "FM")
  cer_model_pet_bbas <- cointReg(precos_fechamento$PETR4.SA, precos_fechamento$BBAS3.SA, method = "FM")
  cer_model_vale_bbas <- cointReg(precos_fechamento$VALE3.SA, precos_fechamento$BBAS3.SA, method = "FM")
  
  # Obtenha informações detalhadas sobre o modelo CER
  summary(cer_model_pet_vale)
  summary(cer_model_pet_bbas)
  summary(cer_model_vale_bbas)
  
  # Obtenha a matriz de correlação entre os ativos
  matriz_correlacao <- cor(precos_fechamento)
  
  # Imprima a matriz de correlação
  cat("Matriz de Correlação entre os Ativos:\n")
  print(matriz_correlacao)
}
