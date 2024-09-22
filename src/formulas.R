# Realiza o teste ANOVA para análise das médias de amostras
test_ANOVA <- function(data, var, group) {
  return(summary(aov(var ~ group, data = data)))
}

# Realiza o teste de Tukey para distinguir diferença das médias entre amostras
test_TukeyHSD <- function(data, var, group) {
  return(TukeyHSD(test_ANOVA(data, var, group)))
}

# Realiza o teste de Correlação de Pearson para duas variáveis de uma amostra
test_Corr <- function(var1, var2) {
  if (is.numeric(var1) && is.numeric(var2)) {
    cor(var1, var2, method = "pearson", use = "complete.obs")
  } else {
    message("Insira colunas númericas!")
  }
}