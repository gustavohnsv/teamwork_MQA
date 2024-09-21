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