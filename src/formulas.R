# Realiza o teste ANOVA para análise das médias de amostras (teste paramétrico)
test_ANOVA <- function(data, var, group) {
  return(summary(aov(var ~ group, data = data)))
}

# Realiza o teste de Kruskal-Wallis para análise das médias de amostras (teste não paramétrico)
test_KruskalWallis <- function(data, var, group) {
  return(kruskal.test(formula = var ~ group, data = data))
}

# Realiza o teste de Levene para análise das variâncias de amostras
test_Levene <- function(var, group) {
  return(leveneTest(y = var, group = as.factor(group)))
}

# Realiza o teste de Dunn para analisar quais dos grupos se diferem baseado no teste Kruskal-Wallis (não paramétrico)
test_Dunn <- function(data, var, group) {
  return(dunnTest(var ~ as.factor(group), data = data, method="bonferroni"))
}

# Realiza o teste de Correlação de Pearson para duas variáveis de uma amostra
test_Corr <- function(var1, var2) {
  if (is.numeric(var1) && is.numeric(var2)) {
    return(cor(var1, var2, method = "pearson", use = "complete.obs"))
  } else {
    message("Insira colunas númericas!")
  }
}

# Realiza o teste de Shapiro-Wilk para garantir se uma variável númerica segue uma curva normal 
test_ShapiroWilk <- function(data) {
  if (is.numeric(data)) {
    return(shapiro.test(data))
  } else {
    message("Insira uma coluna númerica!")
  }
}

# Realiza o teste de Shapiro-Wilk para garantir se todas as variáveis númericas seguem uma distribuição normal 
testDataframe_ShapiroWilk <- function(data) {
  num <- ncol(data)
  for (j in 1:num) {
    if (is.numeric(data[, j])) {
      print(colnames(data[j]))
      print(shapiro.test(data[, j]))
    }
  }
}

# Realiza o teste de Anderson-Darling para garantir se uma variável númerica segue uma curva normal
test_AndersonDarling <- function(data) {
  if (is.numeric(data)) {
    return(ad.test(data))
  } else {
    message("Insira uma coluna númerica!")
  }
}

# Realiza o teste de Anderson-Darling para garantir se todas as variáveis númericas seguem uma distribuição normal 
testDataframe_AndersonDarling <- function(data) {
  num <- ncol(data)
  for (j in 1:num) {
    if (is.numeric(data[, j])) {
      print(colnames(data[j]))
      print(ad.test(data[, j]))
    }
  }
}

test_MultipleCorrelation <- function(data, y, ...) {
  predictors <- quos(...)
  formula <- reformulate(termlabels = sapply(predictors, quo_name), response = y)
  summary(lm(formula = formula, data = data))
}

# Função para calcular a moda
moda <- function(data) {
  data <- data[!is.na(data)]
  uniq_value <- unique(data)
  uniq_value[which.max(tabulate(match(data, uniq_value)))]
}

# Função para criar tabela de distribuição de frequências com as frequências absolutas, frequências relativas, frequências absolutas acumuladas e frequências relativas acumuladas 
create_freq_tables <- function(column) {
  # Definir o número de classes (k) usando a Regra de Sturges
  n <- length(column)
  k <- 1 + 3.322 * log10(n)
  
  # Calcular a amplitude total (R)
  R <- max(column) - min(column)
  
  # 3. Calcular a amplitude dos intervalos (h)
  h <- R / k
  
  # 4. Definir os intervalos usando a função cut
  intervals <- cut(column, breaks = seq(min(column), max(column), by = h), right = FALSE)
  
  # 5. Calcular as frequências absolutas e relativas
  absol_freq <- table(intervals)
  rel_freq <- prop.table(absol_freq)
  
  # 6. Criar a tabela final
  freq_tables <- data.frame(
    Intervalos = names(absol_freq),
    Frequencia_Absoluta = as.vector(absol_freq),
    Frequencia_Relativa = as.vector(rel_freq),
    Frequencia_Absoluta_Acumulada = cumsum(as.vector(absol_freq)),
    Frequencia_Relativa_Acumulada = cumsum(as.vector(rel_freq))
  )
  
  return(freq_tables)
}


