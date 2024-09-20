# Soma dos quadrados totais de um conjunto de dados
somaQuadradosTotais <- function(data) {
  num <- sum(!is.na(data))
  g <- ncol(data)
  global_mean <- mean(as.vector(unlist(data)), na.rm = TRUE)
  res <- 0
  for (j in 1:g) {
    col_sum_of_square <- sum((data[, j] - global_mean)^2, na.rm = TRUE)
    res <- res + col_sum_of_square
  }
  return(res)
}

# Soma dos quadrados dos erros de um conjunto de dados
somaQuadradosResiduos <- function(data) {
  num <- sum(!is.na(data))
  g <- ncol(data)
  global_mean <- mean(as.vector(unlist(data)), na.rm = TRUE)
  res <- 0
  for (j in 1:g) {
    col_mean <- mean(data[, j], na.rm = TRUE)
    col_sum_of_err <- sum((data[, j] - col_mean)^2, na.rm = TRUE)
    res <- res + col_sum_of_err
  }
  return(res)
}

# Soma dos quadrados dos tratamentos de um conjunto de dados
somaQuadradosTratamento <- function(data) {
  num <- sum(!is.na(data))
  g <- ncol(data)
  global_mean <- mean(as.vector(unlist(data)), na.rm = TRUE)
  res <- 0
  for (j in 1:g) {
    col_mean <- mean(data[, j], na.rm = TRUE)
    col_sum_of_trat <- (col_mean - global_mean)^2
    res <- res + (num * col_sum_of_trat)
  }
  return(res)
}

# Cálculo para o teste ANOVA de um fator para amostras independentes
valorF <- function(data) {
  num <- sum(!is.na(data))
  g <- ncol(data)
  numerador <- somaQuadradosTratamento(data) / (g - 1)
  denominador <- somaQuadradosResiduos(data) / (num - g)
  return(numerador / denominador)
}

valorF_para_valorP <- function(data) {
  return(1 - pf(valorF(data), glTrat(data), glErr(data)))
}

glTotal <- function(data) {
  num <- sum(!is.na(data))
  return(num  - 1)
}

glTrat <- function(data) {
  g <- ncol(data)
  return(g - 1)
}

glErr <- function(data) {
  num <- sum(!is.na(data))
  g <- ncol(data)
  return(num - g)
}

# Caso opt seja 1, retorna o desvio padrão, caso contrário, a variância
desvioPadraoVariancia <- function(data, opt) {
  num <- sum(!is.na(data))
  mean <- mean(data, na.rm = TRUE)
  a <- (data - mean)^2
  b <- num - 1
  res <- sum(a/b, na.rm = TRUE)
  return(ifelse(opt == 1, sqrt(res), res))
}

