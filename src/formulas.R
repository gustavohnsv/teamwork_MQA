# Soma dos quadrados totais de um conjunto de dados
somaQuadradosTotais <- function(data) {
  num <- nrow(data)
  g <- ncol(data)
  gl_total <- (num * g) - 1
  global_mean <- mean(colMeans(data))
  res <- 0
  for (j in 1:g) {
    col_sum_of_square <- sum((data[, j] - global_mean)^2)
    res <- res + col_sum_of_square
  }
  print(gl_total)
  return(res)
}

# Soma dos quadrados dos erros de um conjunto de dados
somaQuadradosResiduos <- function(data) {
  num <- nrow(data)
  g <- ncol(data)
  gl_err <- (num * g) - 1
  global_mean <- mean(colMeans(data))
  res <- 0
  for (j in 1:g) {
    col_mean <- mean(data[, j])
    col_sum_of_err <- sum((data[, j] - col_mean)^2)
    res <- res + col_sum_of_err
  }
  print(gl_err)
  return(res)
}

# Soma dos quadrados dos tratamentos de um conjunto de dados
somaQuadradosTratamento <- function(data) {
  num <- nrow(data)
  g <- ncol(data)
  gl_trat <- g - 1
  global_mean <- mean(colMeans(data))
  res <- 0
  for (j in 1:g) {
    col_mean <- mean(data[, j])
    col_sum_of_trat <- (col_mean - global_mean)^2
    res <- res + (num * col_sum_of_trat)
  }
  print(gl_trat)
  return(res)
}

# Cálculo para o teste ANOVA de um fator para amostras independentes
valorF <- function(data) {
  num <- nrow(data)
  g <- ncol(data)
  numerador <- somaQuadradosTratamento(data) / (g - 1)
  denominador <- somaQuadradosResiduos(data) / (num - g)
  return(numerador / denominador)
}

# Caso opt seja 1, retorna o desvio padrão, caso contrário, a variância
desvioPadraoVariancia <- function(data, opt) {
  num <- nrow(data)
  mean <- mean(data)
  res <- sum((data - mean)^2/num-1)
  return(ifelse(opt == 1, sqrt(res), res))
}

