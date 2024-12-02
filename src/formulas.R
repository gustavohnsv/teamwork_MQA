# Realiza o teste ANOVA para análise das médias de amostras (teste paramétrico)
#test_ANOVA <- function(data, var, group) {
#  return(summary(aov(var ~ group, data = data)))
#}

# Realiza o teste de Kruskal-Wallis para análise das médias de amostras (teste não paramétrico)
#test_KruskalWallis <- function(data, var, group) {
#  return(kruskal.test(formula = var ~ group, data = data))
#}

# Realiza o teste de Levene para análise das variâncias de amostras
#test_Levene <- function(var, group) {
#  return(leveneTest(y = var, group = as.factor(group)))
#}

# Realiza o teste de Dunn para analisar quais dos grupos se diferem baseado no teste Kruskal-Wallis (não paramétrico)
#test_Dunn <- function(data, var, group) {
#  return(dunnTest(var ~ as.factor(group), data = data, method="bonferroni"))
#}

# Realiza o teste de Correlação de Pearson para duas variáveis de uma amostra
test_Corr <- function(var1, var2) {
  if (is.numeric(var1) && is.numeric(var2)) {
    return(cor(var1, var2, method = "pearson", use = "complete.obs"))
  } else {
    message("Insira colunas númericas!")
  }
}

# Aplica um modelo de regressão linear múltipla e aplica o processo de stepwise para refinar o modelo
#test_MultipleCorrelation <- function(data, y, ...) {
#  response <- enquo(y)
#  predictors <- quos(...)
#  response_name <- quo_name(response)
#  predictors_names <- sapply(predictors, quo_name)
#  formula <- reformulate(termlabels = predictors_names, response = response_name)
#  model <- lm(formula = formula, data = data)
#  stepwise_model <- step(model, direction = "both", k = log(nrow(data)), trace = TRUE)
#  return(stepwise_model)
#}

# Realiza o teste de Shapiro-Wilk para garantir se uma variável númerica segue uma curva normal 
#test_ShapiroWilk <- function(data) {
#  if (is.numeric(data)) {
#    return(shapiro.test(data))
#  } else {
#    message("Insira uma coluna númerica!")
#  }
#}

# Realiza o teste de Shapiro-Wilk para garantir se todas as variáveis númericas seguem uma distribuição normal 
#testDataframe_ShapiroWilk <- function(data) {
#  num <- ncol(data)
#  for (j in 1:num) {
#    if (is.numeric(data[, j])) {
#      print(colnames(data[j]))
#      print(shapiro.test(data[, j]))
#    }
# }
#}

# Realiza o teste de Anderson-Darling para garantir se uma variável númerica segue uma curva normal
#test_AndersonDarling <- function(data) {
# if (is.numeric(data)) {
#    return(ad.test(data))
# } else {
#   message("Insira uma coluna númerica!")
# }
#}

# Realiza o teste de Anderson-Darling para garantir se todas as variáveis númericas seguem uma distribuição normal 
#testDataframe_AndersonDarling <- function(data) {
# num <- ncol(data)
# for (j in 1:num) {
#    if (is.numeric(data[, j])) {
#      print(colnames(data[j]))
#      print(ad.test(data[, j]))
#    }
#  }
#}

# Função para calcular a moda
#moda <- function(data) {
# data <- data[!is.na(data)]
# uniq_value <- unique(data)
# uniq_value[which.max(tabulate(match(data, uniq_value)))]
#}

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
  
  # 7. Formatar para melhor exibição
  freq_tables <- formattable(freq_tables, list(
    Intervalos = formatter("span", style = x ~ style(color = "black", font.weight = "bold", font.size = "16px")),
    Frequencia_Absoluta = formatter("span", style = x ~ style(color = "black", font.weight = "bold", font.size = "16px")),
    Frequencia_Relativa = formatter("span", style = x ~ style(color = "black", font.weight = "bold", font.size = "16px")),
    Frequencia_Absoluta_Acumulada = formatter("span", style = x ~ style(color = "black", font.weight = "bold", font.size = "16px")),
    Frequencia_Relativa_Acumulada = formatter("span", style = x ~ style(color = "black", font.weight = "bold", font.size = "16px"))
  ))
  
  return(freq_tables)
}

# Função para exibir coeficiente de determinação na regressão linear simples
simple_linear_regression_sqr_r <- function(y,x){
  model <- lm(y~x)
  return(summary(model)$r.square)
}

# Padronização (Z-score)
standardize_z_score <- function(x) {
  if(is.numeric(x)) {
    return((x - mean(x)) / sd(x))
  } else {
    return(x)
  }
}

standardize_log <- function(x) {
  if (is.numeric(x)) {
    return(log(x+1))
  } else {
    return(x)
  }
}

#format_odds_ratios <- function(results_df) {
# if (!all(c("OR", "2.5 %", "97.5 %", "p") %in% names(results_df))) {
#   stop("O data frame deve conter as colunas: 'OR', '2.5 %', '97.5 %', 'p'")
# }
# 
# for (i in 1:nrow(results_df)) {
#   or <- results_df$OR[i]
#   lower_ci <- results_df$`2.5 %`[i]
#   upper_ci <- results_df$`97.5 %`[i]
#   p_value <- results_df$p[i]
#   
#   or_percentage <- sprintf("%.2f%%", (or - 1) * 100)
#   lower_ci_percentage <- sprintf("%.2f%%", (lower_ci - 1) * 100)
#   upper_ci_percentage <- sprintf("%.2f%%", (upper_ci - 1) * 100)
#   p_value_formatted <- format(p_value, scientific = FALSE)
#   
#   cat(sprintf("OR: %s (IC: [%s, %s]) - p: %s\n", 
#               or_percentage, lower_ci_percentage, upper_ci_percentage, p_value_formatted))
# }
#}

# Função para calcular AIC e BIC
#calculate_aic_bic <- function(fit, X, y) {
# n <- length(y)
# 
# # Prever os valores ajustados
#  y_pred <- predict(fit, X, s = fit$lambda.min)
# 
# # Calcular RSS (soma dos resíduos quadrados)
# rss <- sum((y - y_pred)^2)
# 
# # Contar número de parâmetros não zero
# nonzero_params <- sum(coef(fit, s = fit$lambda.min) != 0)
# 
# # Calcular AIC e BIC
# aic <- n * log(rss/n) + 2 * nonzero_params
# bic <- n * log(rss/n) + log(n) * nonzero_params
# 
# return(list(AIC = aic, BIC = bic))
#}

# Função para remover outliers de um dataframe com base nos quartis
no_outliers_df <- function(dataframe){
  dataframe %>%
    mutate(across(where(is.numeric), ~ {
      Q1 <- quantile(.x, 0.25, na.rm = TRUE)
      Q3 <- quantile(.x, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower <- Q1 - 1.5 * IQR
      upper <- Q3 + 1.5 * IQR
      ifelse(.x >= lower & .x <= upper, .x, NA) # Substitui outliers por NA
    }))
}

# Função para remover variáveis com MSA < 0.5
remove_low_msa <- function(data) {
  # Calcular a matriz de correlação
  correlation_matrix <- cor(data)
  
  # Calcular o KMO
  kmo_result <- KMO(correlation_matrix)
  
  # Obter os valores MSA individuais
  msa_values <- kmo_result$MSAi
  
  # Filtrar variáveis com MSA >= 0.5
  variables_to_keep <- names(msa_values[msa_values >= 0.5])
  
  # Retornar um dataframe com as variáveis filtradas
  filtered_data <- data[, variables_to_keep, drop = FALSE]
  
  # Mensagem informando quais variáveis foram removidas
  removed_variables <- names(msa_values[msa_values < 0.5])
  if (length(removed_variables) > 0) {
    cat("Variáveis removidas (MSA < 0.5):", paste(removed_variables, collapse = ", "), "\n")
  } else {
    cat("Nenhuma variável foi removida.\n")
  }
  
  return(filtered_data)
}