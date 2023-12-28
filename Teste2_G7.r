# Grupo 7: Gabriel Almeida, Lucas Capris, Victor Azevedo

funcCpm<-function(n, d, Suc, Pre)
{
  #generates early and late start times of for a cpm network
  #Network data:
  #n:number of nodes
  #d:task duration vector
  #Suc:list of successors - zero for none
  #Pre:list of predecessors - zero for none
  
  #Return vectors early and late 
  est<-vector (mode="numeric",length=n)
  eft<-vector (mode="numeric",length=n)
  lst<-vector (mode="numeric",length=n)
  lft<-vector (mode="numeric",length=n)
  
  
  #Functions
  cpmf<-function(s,est){
    #find early start and early finish times
    #print(s)
    
    # Nova Função
    
    eft[s]<-est[s]+d[s]
    if ((Suc[[s]][1]!=0)){
      for (i in Suc[[s]]){
        if (est[i] < eft[s]){
          est[i]<-eft[s]
        }
        est<-cpmf(i,est)
      }	
    } 
    est
  }
  
  cpmb<-function(s,lft){
    #find late start and finish times
    lst[s]<-lft[s]-d[s]
    if (Pre[[s]][1]!=0){
      for (i in Pre[[s]]){
        if (lft[i]>lst[s]){
          lft[i]<-lst[s]                
        }                  
        lft<-cpmb(i,lft)
      }
    }
    lft
  }
  
  n = n
  rf<-cpmf(1,est)                         #call forward step
  lft<-rep(rf[n]+d[n],times=n)            #initialize late finish times
  rb<-cpmb(n,lft)-d                       #call backward step
  slack<-rb-rf                            #slack times 
  r<-list(est=rf,lst=rb,slack=slack)
}

Q1 <- function(){
  # Código em comum das questões 1.1 e 1.2
  # Deve ser executado antes das respectivas funções
  
  library(MASS)
  library(triangle)
  library(Matrix)
  library(igraph)
  
  Ns=5000
  nVar = 9
  nVarDur = nVar + 1
  
  A <- diag(x = 1, nrow = nVar)
  A[1, 2] <- 0.7
  A[2, 1] <- A[1, 2]
  A[1, 3] <- 0.7
  A[3, 1] <- A[1, 3]
  A[4, 5] <- 0.7
  A[5, 4] <- A[1, 2]
  
  ptDuracao <- matrix(data = c(0,0,0,
                               5,6,7,
                               2,4,5,
                               1,3,4,
                               4,5,7,
                               6,8,10,
                               2,3,5,
                               4,6,8,
                               0,0,0), ncol=3, byrow=T)
  
  ptCusto <- matrix(data=c(0,0,0,
                           120,150,190,
                           60,75,85,
                           45,60,80,
                           160,200,240,
                           210,250,300,
                           85,120,140,
                           75,100,125,
                           0,0,0), ncol=3, byrow=T)
  
  sucessores <- list(c(2,4),3,8,5,c(6,8),7,9,9,0,0)
  predecessores <- list(0,0,1,2,1,4,5,6,c(3,5),c(7,8))
  elos<-c(1,2,2,3,2,5,3,4,5,4,9,6,6,7,7,9,7,8,8,10,9,10,10,11)
  
  NCorrTriang<-function(n=nVar, Ns=Ns, A=matrix(rep(0,n*n), ncol=n),pt=matrix(nrow=n, rep(0,3*n))){
    m<-rep(0, times=n)
    Z<-mvrnorm(Ns, mu=m, Sigma=A)
    U<-pnorm(Z)
    Tri<-matrix(nrow=Ns, ncol=n)
    for (i in 1:n){
      Tri[ ,i] <- qtriangle(U[,i], pt[i,1], pt[i,3], pt[i,2])
    }
    Tri
  }
  
  ApplyProb<-function(Custo,Prazo){
    for (i in 1:Ns){
      probCheck1<-runif(1)
      if (!(probCheck1 <= 0.4)){
        Custo[i,3] = 0
        Prazo[i,3] = 0
      }
      probCheck2<-runif(1)
      if (!(probCheck2 <= 0.6)){
        Custo[i,7] = 0
        Prazo[i,7] = 0
      }
      return(list(Custo,Prazo))
    }
  }
  
  #Principal
  cenarioCusto<-NCorrTriang(n=nVar, Ns=Ns, A, ptCusto)
  
  cenarioPrazo<-ceiling(NCorrTriang(n=nVar, Ns=Ns, A, ptDuracao))
  cenarioPrazo<-cbind(rep(0, Ns), cenarioPrazo)
  
  cenarios<-ApplyProb(cenarioCusto,cenarioPrazo)
  
  cenarioCusto<-cenarios[[1]]
  cenarioPrazo<-cenarios[[2]]
  
  #Risco Custo
  Custo <- apply(cenarioCusto, 1, sum)
  
  #Risco de prazo
  Agendas <- matrix(ncol=nVarDur, nrow=Ns)
  cCritico <- matrix(data=rep(0, nVarDur*Ns), ncol=nVarDur, nrow=Ns)
  Prazo <- vector(length=Ns)
  
  for (i in 1:Ns){
    r<-funcCpm(n=nVarDur,d=cenarioPrazo[i,],Suc=sucessores,Pre=predecessores)
    Agendas[i,]<-r$est #Cronogramas de menor prazo
    Prazo[i]<-r$est[nVar]+cenarioPrazo[i,nVarDur]  #Menor prazo de realização
    indCrit<-which(r$slack==0)  #Identificando e marcando atividades no CC
    cCritico[i,indCrit]<-1
    probCC<-apply(cCritico,2,sum)/Ns
  }
  #Retorno
  r<-list(Custo=Custo,Agendas=Agendas,Prazo=Prazo,cCritico=cCritico,probCC=probCC)
  
  hist(Custo, col = rgb(0, 0, 1), main = "Simulação de MC - Custo", xlab = "Custo", ylab = "Frequência", breaks = 100)
  hist(Prazo, col = rgb(1, 0, 0), main = "Simulação de MC - Prazo", xlab = "Prazo", ylab = "Frequência", breaks = seq(min(Prazo) - 0.5, max(Prazo) + 0.5))
}


Q2.1<-function(){
  # Carregar o arquivo CSV
  dados <- read.csv("serieHist.csv", header = FALSE)
  
  # Substituir o valor em branco na primeira coluna por 'tempo'
  colnames(dados)[1] <- "tempo"
  
  # Definir os nomes das colunas para os ativos
  nomes_ativos <- c("CMIG4.SA", "CSNA3.SA", "USIM5.SA", "VALE3.SA")
  colnames(dados)[-1] <- nomes_ativos
  
  # Calcular o modelo CER para cada ativo
  modelos_CER <- list()
  
  for (coluna in nomes_ativos) {
    # Calcular o modelo CER para cada ativo
    modelo <- lm(dados[[coluna]] ~ dados$tempo)
    
    # Salvar o modelo na lista de modelos
    modelos_CER[[coluna]] <- modelo
    
    cat("Resumo do modelo para", coluna, ":\n")
    print(summary(modelo))
    cat("\n")
  }
}
Q2.2<-function(){
  # Converter as colunas dos ativos para tipo numérico
  for (coluna in nomes_ativos) {
    dados[[coluna]] <- as.numeric(as.character(dados[[coluna]]))
  }
  
  # Calcular a correlação entre os ativos
  matriz_correlacao <- matrix(NA, nrow = length(nomes_ativos), ncol = length(nomes_ativos))
  rownames(matriz_correlacao) <- nomes_ativos
  colnames(matriz_correlacao) <- nomes_ativos
  
  for (i in 1:length(nomes_ativos)) {
    for (j in 1:length(nomes_ativos)) {
      # Calcular a correlação entre os ativos i e j
      correlacao <- cor(dados[[nomes_ativos[i]]], dados[[nomes_ativos[j]]], use = "complete.obs")
      
      # Preencher a matriz de correlação
      matriz_correlacao[i, j] <- correlacao
    }
  }
  
  # Mostrar a matriz de correlação
  print("Matriz de correlação entre os ativos:")
  print(matriz_correlacao)
}


Q2.3<-function(){
  # Definir o número de períodos/tempo
  num_periodos <- 100
  
  # Gerar dados simulados para cada ativo
  set.seed(42)  # Define a semente para reproduzir os resultados
  simulacao_ativos <- data.frame(tempo = 1:num_periodos)  # Cria um dataframe com a coluna de tempo
  
  for (coluna in nomes_ativos) {
    # Gerar dados simulados com distribuição normal para cada ativo
    media <- 0  # Média zero para simulação
    desvio_padrao <- 1  # Desvio padrão para simulação
    variacoes <- rnorm(num_periodos, mean = media, sd = desvio_padrao)
    
    # Adicionar variações à coluna do ativo
    simulacao_ativos[[coluna]] <- cumsum(variacoes)  # Calcula o valor acumulado das variações para simular preços
    
    # Visualizar os primeiros valores da simulação para o ativo atual
    cat("Primeiros valores simulados para", coluna, ":\n")
    print(head(simulacao_ativos[[coluna]]))
    cat("\n")
  }
  
  # Visualizar a estrutura dos dados simulados
  cat("Estrutura dos dados simulados:\n")
  print(str(simulacao_ativos))
  
  # Plot dos preços simulados ao longo do tempo para cada ativo
  library(ggplot2)
  
  # Reorganiza os dados para o formato longo (tidy format) para o ggplot
  simulacao_ativos_tidy <- tidyr::gather(simulacao_ativos, key = "Ativo", value = "Preço", -tempo)
  
  # Criação do gráfico
  ggplot(simulacao_ativos_tidy, aes(x = tempo, y = Preço, color = Ativo)) +
    geom_line() +
    labs(title = "Simulação de Evolução dos Ativos", x = "Tempo", y = "Preço") +
    theme_minimal()
}


Q3<-function(){
  # Lista para armazenar os coeficientes do modelo CER
  coeficientes_tempo <- c()
  
  # Calcular o modelo CER para cada ativo e extrair os coeficientes do tempo
  for (coluna in nomes_ativos) {
    modelo <- lm(dados[[coluna]] ~ dados$tempo)
    coeficientes <- coef(modelo)[[2]]  # Extrair o coeficiente do tempo
    
    # Salvar o coeficiente do tempo na lista
    coeficientes_tempo <- c(coeficientes_tempo, coeficientes)
  }
  
  # Criar um dataframe com os coeficientes do tempo
  coeficientes_dataframe <- data.frame(Coeficiente_tempo = coeficientes_tempo, Ativo = rep(nomes_ativos, each = length(coeficientes_tempo) / length(nomes_ativos)))
  
  # Mostrar os coeficientes estimados para cada ativo
  print("Coeficientes estimados para cada ativo:")
  print(coeficientes_dataframe)
  
  # Gráfico da distribuição dos coeficientes do tempo para cada ativo
  ggplot(data = coeficientes_dataframe, aes(x = Coeficiente_tempo, fill = Ativo)) +
    geom_histogram(binwidth = 0.05, color = "black", alpha = 0.7, position = "identity") +
    labs(title = "Distribuição dos Coeficientes do Tempo por Ativo", x = "Coeficiente do Tempo") +
    theme_minimal()
  
  # Calcular a média e a variância dos coeficientes do tempo para cada ativo
  media_coeficientes <- tapply(coeficientes_tempo, nomes_ativos, mean)
  variancia_coeficientes <- tapply(coeficientes_tempo, nomes_ativos, var)
  
  # Exibir a média e a variância dos coeficientes do tempo para cada ativo
  cat("Média dos coeficientes do tempo para cada ativo:\n")
  print(media_coeficientes)
  
  cat("\nVariância dos coeficientes do tempo para cada ativo:\n")
  print(variancia_coeficientes)
}

