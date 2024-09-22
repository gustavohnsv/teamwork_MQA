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


# Realiza o teste ANOVA para análise de variância de amostras
teste_ANOVA <- function(data, var, group) {
  return(summary(aov(var ~ group, data = data)))
}

# Realiza o teste Tukey para análise distinguir diferença entre amostras
test_TukeyHSD <- function(data, var, group) {
  return(TukeyHSD(teste_ANOVA(data, var, group)))
}

# Realiza o teste de Correlação de Pearson para duas variáveis de uma amostra
test_Corr <- function(var1, var2) {
  if (is.numeric(var1) && is.numeric(var2)) {
    cor(var1, var2, method = "pearson", use = "complete.obs")
  } else {
    message("Insira colunas númericas!")
  }
}

# Realiza o teste Shapiro-Wilk para garantir se uma variável númerica segue uma curva normal 
test_ShapiroWilk <- function(data) {
  if (is.numeric(data)) {
    return(shapiro.test(data))
  } else {
    message("Insira uma coluna númerica!")
  }
}

# Realiza o teste Shapiro-Wilk para garantir se todas as variáveis númericas seguem uma distribuição normal 
testDataframe_ShapiroWilk <- function(data) {
  num <- ncol(data)
  for (j in 1:num) {
    if (is.numeric(data[, j])) {
      print(colnames(data[j]))
      print(shapiro.test(data[, j]))
    }
  }
}

# Função para calcular a moda
moda <- function(data) {
  data <- data[!is.na(data)]
  uniq_value <- unique(data)
  uniq_value[which.max(tabulate(match(data, uniq_value)))]
}