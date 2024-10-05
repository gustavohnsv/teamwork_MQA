# Adiciona uma coluna indicando a cor do vinho de cada dataset
white_wines$colour <- "white"
red_wines$colour <- "red"

# Combina os datasets para um único
wines <- rbind(white_wines, red_wines)

# Retira possíveis observações com campos NA
wines <- na.omit(wines)

# Diz que a coluna "colour" deve ser tratada como um fator
wines$colour <- as.factor(wines$colour)

# Sortea 5000 observações para compor uma "amostra da amostra"
wines_sample <- wines[sample(nrow(wines), size = 5000, replace = FALSE), ]

# Cria um subset apenas com as colunas que são númericas
wines_numeric <- wines[, sapply(wines, is.numeric)]

# Obtem o número de colunas de "wines_numeric"
ncol <- ncol(wines_numeric)

# Exibe o coeficiente de correlação de cada variável
for (i in which(numeric_col)) {
  for (j in which(numeric_col)) {
    print(paste(colnames(wines[i]), " x ", colnames(wines[j])))
    print(test_Corr(wines[, i], wines[, j]))
  }
}

# Remove as variáveis temporárias para o loop
rm(i, j, ncol)