# Adiciona uma coluna indicando a cor do vinho de cada dataset
white_wines$colour <- "white"
red_wines$colour <- "red"

# Combina os datasets para um único
wines <- rbind(white_wines, red_wines)

# Retira possíveis observações com campos NA
wines <- na.omit(wines)

# Diz que a coluna "colour" deve ser tratada como um fator
# wines$colour <- as.factor(wines$colour)

colnames(wines)[13] <- "is.red"

wines <- wines %>%
  mutate(is.red = ifelse(is.red == "red", 1, 0))

# Sorteia 5000 observações para compor uma "amostra da amostra"
wines_sample <- wines[sample(nrow(wines), size = 5000, replace = FALSE), ]

# Cria um subset apenas com as colunas que são númericas
wines_numeric <- wines[, sapply(wines, is.numeric)]

# Obtem o número de colunas de "wines_numeric"
wines_numeric_cols <- ncol(wines_numeric)

# Criação de Dataframe para mostrar coeficiente de correlação de cada variável
corr_df <- data.frame (
  variable_1 = character(),
  corr_value = numeric(),
  variable_2 = character(),
  corr_value_module = numeric()
)

# Exibe o coeficiente de correlação de cada variável
for (i in 1:wines_numeric_cols) {
  for (j in 1:wines_numeric_cols) {

    corr_test_result <- test_Corr(wines[, i], wines[, j])
    
    # Adicionar ao dataframe que mostra coeficiente de correlação por variável

    if(corr_test_result != 1){
      corr_df <- rbind(corr_df, data.frame(variable_1 = colnames(wines[i]), corr_value = corr_test_result, variable_2 = colnames(wines[j]), corr_value_module = abs(corr_test_result)))
    }
  }
}

# Remove as variáveis temporárias para o loop
rm(i, j, wines_numeric_cols, corr_test_result)

