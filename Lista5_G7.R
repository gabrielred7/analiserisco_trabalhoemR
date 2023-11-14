# Victor Constâncio de Azevedo - 116158472
# Lucas Capris - 116161776
# Gabriel Almeida Mendes - 117204959

q1a <- function() {
    # Função para simular a duração de uma atividade com distribuição triangular
    simular_duracao <- function(min, mode, max, n_simulacoes) {
    u <- runif(n_simulacoes)
    d <- numeric(n_simulacoes)
    
    for (i in 1:n_simulacoes) {
        if (u[i] <= (mode - min) / (max - min)) {
        d[i] <- min + sqrt(u[i] * (max - min) * (mode - min))
        } else {
        d[i] <- max - sqrt((1 - u[i]) * (max - min) * (max - mode))
        }
    }
    return(d)
    }

    # Função para calcular a duração total do projeto
    calcular_duracao_total <- function(di, P, min_d, mode_d, max_d, n_simulacoes) {
    duracao_total <- numeric(n_simulacoes)
    for (sim in 1:n_simulacoes) {
        duracoes <- numeric(n)
        for (atividade in 1:n) {
        predecessores <- which(P[, atividade] == 1)
        duracao <- di[atividade] + sum(duracoes[predecessores])
        duracoes[atividade] <- max(simular_duracao(min_d[atividade], mode_d[atividade], max_d[atividade], 1), duracao)
        }
        duracao_total[sim] <- max(duracoes)
    }
    return(duracao_total)
    }

    n <- 14
    delta_n <- 44
    di <- c(0, 6, 5, 3, 1, 6, 2, 1, 4, 3, 2, 3, 5, 0)
    P <- matrix(0, n, n)
    P[c(1,1,1,2,3,3,3,4,5,6,7,7,8,9,10,11,12,13), c(2,3,4,9,5,6,7,8,10,12,8,11,13,14,12,12,13,14)] <- 1

    min_d <- c(0, 4, 2, 2, 0.5, 3, 1, 0.5, 2, 2, 1, 1, 3, 0)
    mode_d <- c(0.1, 6, 5, 3, 1, 6, 2, 1, 3, 3, 2, 2, 5, 0.2)
    max_d <- c(0.2, 10, 8, 6, 2, 10, 4, 3, 6, 5, 3, 4, 8, 0.2)
    n_simulacoes <- 10000

    set.seed(123)  # Definir semente para reprodutibilidade
    duracao_total_simulada <- calcular_duracao_total(di, P, min_d, mode_d, max_d, n_simulacoes)
    media_duracao_total <- mean(duracao_total_simulada)
    cat("Aproximação Empirica para a Duração Total Média do Projeto:", round(media_duracao_total, 2), "\n")
}

q1b <- function(){
    # Função para calcular a duração das atividades com base na distribuição triangular
    gerar_duracao <- function() {
    duracoes <- numeric(n)
    for (i in 1:n) {
        duracoes[i] <- runif(1, min_d[i], max_d[i])
    }
    return(duracoes)
    }

    # Função para simular o caminho crítico e calcular a probabilidade
    simular_caminho_critico <- function() {
    duracoes <- gerar_duracao()
    termino <- numeric(n)
    termino[1] <- duracoes[1]
    for (i in 2:n) {
        atividades_predecessoras <- which(P[, i] == 1)
        max_termino_predecessoras <- max(termino[atividades_predecessoras])
        termino[i] <- max_termino_predecessoras + duracoes[i]
    }
    tempo_maximo <- max(termino)
    caminho_critico <- which(termino == tempo_maximo)
    probabilidade <- length(caminho_critico) / n
    return(probabilidade)
    }

    n <- 14
    delta_n <- 44
    d <- c(0, 6, 5, 3, 1, 6, 2, 1, 4, 3, 2, 3, 5, 0)
    c <- c(0, -140, 318, 312, -329, 153, 193, 361, 24, 33, 387, -386, 171, 0)
    P <- matrix(0, n, n)
    P[c(1,2,3,4,2,9,3,5,3,6,3,7,4,8,5,10,6,12,7,8,7,11,8,13,9,14,10,12,11,12,12,13,13,14)] <- 1

    min_d <- c(0, 4, 2, 2, 0.5, 3, 1, 0.5, 2, 2, 1, 1, 3, 0)
    max_d <- c(0.2, 10, 8, 6, 2, 10, 4, 3, 6, 5, 3, 4, 8, 0.2)
    num_simulacoes <- 10000
    resultados <- numeric(num_simulacoes)

    set.seed(123)  # Definir semente para reprodução
    for (i in 1:num_simulacoes) {
    resultados[i] <- simular_caminho_critico()
    }

    probabilidade_media <- mean(resultados)
    cat("Probabilidade média do caminho crítico:", probabilidade_media)
}

q1c <- function(){
    n <- 14
    delta_n <- 44
    d <- c(0, 6, 5, 3, 1, 6, 2, 1, 4, 3, 2, 3, 5, 0)
    c <- c(0, -140, 318, 312, -329, 153, 193, 361, 24, 33, 387, -386, 171, 0)

    atividades <- 1:14
    d_min <- c(0, 4, 2, 2, 0.5, 3, 1, 0.5, 2, 2, 1, 1, 3, 0)
    d_max <- c(0.2, 10, 8, 6, 2, 10, 4, 3, 6, 5, 3, 4, 8, 0.2)

    # Função para amostrar uma duração aleatória para uma atividade
    sample_duration <- function(atividade) {
    runif(1, d_min[atividade], d_max[atividade])
    }

    # Função para realizar uma simulação Monte Carlo para uma atividade
    monte_carlo_simulation <- function(atividade, num_simulacoes) {
    resultados <- numeric(num_simulacoes)
    for (i in 1:num_simulacoes) {
        duração <- sample_duration(atividade)
        resultados[i] <- duração
    }
    return(resultados)
    }

    # Número de simulações Monte Carlo
    num_simulacoes <- 100000

    resultados_simulacao <- list()
    for (atividade in atividades) {
    resultados_simulacao[[atividade]] <- monte_carlo_simulation(atividade, num_simulacoes)
    }
    tempo_conclusao_projeto <- numeric(num_simulacoes)
    for (i in 1:num_simulacoes) {
    tempo_inicial <- 0
    for (atividade in atividades) {
        tempo_inicial <- tempo_inicial + resultados_simulacao[[atividade]][i]
    }
    tempo_conclusao_projeto[i] <- tempo_inicial
    }

    probabilidade_conclusao_antes_prazo <- sum(tempo_conclusao_projeto <= delta_n) / num_simulacoes
    print(paste("Probabilidade de conclusão antes do prazo:", probabilidade_conclusao_antes_prazo))
}