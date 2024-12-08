#Carregamento do dataset



# Definindo o diretório de trabalho para a localização do arquivo "winequality.csv"
setwd(dirname(dirname(getActiveDocumentContext()$path)))

# Caminho para o arquivo único com todos os vinhos
file_path <- "./data/winequality-both.csv"

if (!file.exists(file_path)) {
  # Verifica se o arquivo para todos os vinhos existem
  message("File not found at ", file_path)
} else {
  wines <- tryCatch({
    # Leitura dos dados do arquivo único
    read.csv(file_path, header = TRUE, sep = ",")
  }, error = function(e) {
    message("Error reading file: ", e$message)
    NULL
  })
}

if (!is.null(wines)) {
  # Verifica se os dados foram lidos com sucesso
  message("All wines file read successfully")
}



#Carregamento das bibliotecas que serão usadas



# Biblioteca com algumas funções úteis de rstudioapi
if (!requireNamespace("rstudioapi", quietly = TRUE)) {
  install.packages("rstudioapi")
}
library(rstudioapi)

# Biblioteca com algumas funções úteis de rlang
if (!requireNamespace("rlang", quietly = TRUE)) {
  install.packages("rlang")
}
library(rlang)

# Biblioteca para exibição de gráficos
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Biblioteca para exibição de mapa de calor (Correlação)
if (!requireNamespace("pheatmap", quietly = TRUE)) {
  install.packages("pheatmap")
}
library(pheatmap)

# Biblioteca para tratamento de dados, permite a manipulação de data frames e matrizes
if (!requireNamespace("reshape", quietly = TRUE)) {
  install.packages("reshape")
}
library(reshape)

# Biblioteca para funções de manipulação e transformação de dados
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Biblioteca para manipulação de matrizes esparsas
if (!requireNamespace("SparseM", quietly = TRUE)) {
  install.packages("SparseM")
}
library(SparseM)

# Biblioteca que contém conjuntos de dados utilizados em exemplos da biblioteca 'car'
if (!requireNamespace("carData", quietly = TRUE)) {
  install.packages("carData")
}
library(carData)

# Biblioteca para análise de regressão, ANOVA, e outros métodos estatísticos
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}
library(car)

# Biblioteca utilizada para padronização do dataset
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
library(tidyr)

# Biblioteca para cálculo do coeficiente KMO
if (!requireNamespace("psych", quietly = TRUE)) {
  install.packages("psych")
}
library(psych)

# Biblioteca para rotações de vetores
if (!requireNamespace("stats", quietly = TRUE)) {
  install.packages("stats")
}
library(stats)

# Biblioteca para visualização de matrizes de correlação
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}
library(corrplot)



#Fórmulas utilizadas na análise



# Realiza o teste de Correlação de Pearson para duas variáveis de uma amostra
test_Corr <- function(var1, var2) {
  if (is.numeric(var1) && is.numeric(var2)) {
    return(cor(var1, var2, method = "pearson", use = "complete.obs"))
  } else {
    message("Insira colunas númericas!")
  }
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
  intervals <- cut(column,
                   breaks = seq(min(column), max(column), by = h),
                   right = FALSE)
  
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
  freq_tables <- formattable(
    freq_tables,
    list(
      Intervalos = formatter(
        "span",
        style = x ~ style(
          color = "black",
          font.weight = "bold",
          font.size = "16px"
        )
      ),
      Frequencia_Absoluta = formatter(
        "span",
        style = x ~ style(
          color = "black",
          font.weight = "bold",
          font.size = "16px"
        )
      ),
      Frequencia_Relativa = formatter(
        "span",
        style = x ~ style(
          color = "black",
          font.weight = "bold",
          font.size = "16px"
        )
      ),
      Frequencia_Absoluta_Acumulada = formatter(
        "span",
        style = x ~ style(
          color = "black",
          font.weight = "bold",
          font.size = "16px"
        )
      ),
      Frequencia_Relativa_Acumulada = formatter(
        "span",
        style = x ~ style(
          color = "black",
          font.weight = "bold",
          font.size = "16px"
        )
      )
    )
  )
  
  return(freq_tables)
}

# Função para exibir coeficiente de determinação na regressão linear simples
simple_linear_regression_sqr_r <- function(y, x) {
  model <- lm(y ~ x)
  return(summary(model)$r.square)
}

# Padronização (Z-score)
standardize_z_score <- function(x) {
  if (is.numeric(x)) {
    return((x - mean(x)) / sd(x))
  } else {
    return(x)
  }
}

standardize_log <- function(x) {
  if (is.numeric(x)) {
    return(log(x + 1))
  } else {
    return(x)
  }
}

# Função para remover outliers de um dataframe com base nos quartis
no_outliers_df <- function(dataframe) {
  dataframe %>%
    mutate(across(where(is.numeric), ~ {
      Q1 <- quantile(.x, 0.25, na.rm = TRUE)
      Q3 <- quantile(.x, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower <- Q1 - 1.5 * IQR
      upper <- Q3 + 1.5 * IQR
      ifelse(.x >= lower &
               .x <= upper, .x, NA) # Substitui outliers por NA
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
    cat("Variáveis removidas (MSA < 0.5):",
        paste(removed_variables, collapse = ", "),
        "\n")
  } else {
    cat("Nenhuma variável foi removida.\n")
  }
  
  return(filtered_data)
}



#Exemplo de Análise Fatorial



## Exemplo Análise Fatorial
data("mtcars")
# Matriz de correlação
data_subset <- mtcars[, -c(8, 9)]
corr_matrix <- cor(data_subset)
print(corr_matrix)

# Cálculo KMO e MSA
kmo <- KMO(corr_matrix)
print(kmo)
print(kmo$MSAi)

# Teste de Barlett
barlett_test <- cortest.bartlett(corr_matrix, n = nrow(data_subset))
print(barlett_test)

# Cálculo do MSA
print(kmo$MSAi)

# Determinação de número de fatores

# Critério de Kaiser
kaiser_eigenvalues <- eigen(corr_matrix)$values
print(kaiser_eigenvalues)
factors_kaiser <- sum(kaiser_eigenvalues > 1) # critério pro número de fatores
print(factors_kaiser)

# Scree Plot
plot(
  kaiser_eigenvalues,
  type = "b",
  main = "Scree Plot",
  xlab = "Número de Fatores",
  ylab = "Autovalores"
)
second_derivative <- diff(diff(kaiser_eigenvalues))
elbow <- which.max(second_derivative) + 1

lines(
  c(elbow, elbow),
  c(0, kaiser_eigenvalues[elbow]),
  col = "red",
  lty = 2,
  lwd = 2
) # Aponta número de fatores ideal

# Análise fatorial final
factor_number <- factors_kaiser
factorial_analysis <- fa(
  r = corr_matrix,
  nfactors = factor_number,
  rotate = "varimax",
  fm = "ml"
)
print(factorial_analysis)



#Código para plotagem dos gráficos



# Mapa de calor para cada par de colunas das colunas de vinho
wines_cor_matrix <- cor(wines_sample_numeric, use = "complete.obs")
pheatmap(
  wines_cor_matrix,
  display_numbers = TRUE,
  number_color = "#000000",
  fontsize_number = 8
)
rm(wines_cor_matrix)

# Gráficos de dispersão comparando cada variável com um densidade (precisa de atenção)
plot_model <- test_MultipleCorrelation(wines_sample,
                                       density,
                                       residual.sugar,
                                       fixed.acidity,
                                       alcohol,
                                       chlorides,
                                       colour)
avPlots(plot_model)
rm(plot_model)

# Transforma uma coluna fator em uma coluna númerica
wines_standardized$colour <- as.numeric(wines_standardized$colour)

# Verificando os boxplots após padronização
wines_long_z_score <- pivot_longer(
  wines_standardized,
  cols = everything(),
  names_to = "Variable",
  values_to = "Value"
)

# Boxplot das variáveis normalizadas
ggplot(wines_long_z_score, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Boxplots Padronizados (Z-score)", x = "Variáveis", y = "Valores")

# Boxplot da varíavel residual.sugar padronizada para a amostra de 500 elementos
ggplot(wines_sample_numeric, aes(x = "", y = residual.sugar)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(fill = "red", alpha = 0.5) +
  labs(title = "Boxplot para Açúcar residual", y = "Açúcar residual")

# Boxplot da varíavel fixed.acidity padronizada para a amostra de 500 elementos
ggplot(wines_sample_numeric, aes(x = "", y = fixed.acidity)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  labs(title = "Boxplot para Acidez fixa", y = "Acidez fixa")

# Boxplot da varíavel density padronizada para a amostra de 500 elementos
ggplot(wines_sample_numeric, aes(x = "", y = density)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(fill = "yellow", alpha = 0.5) +
  labs(title = "Boxplot para Densidade", y = "Densidade")

# Boxplot da varíavel alcohol padronizada para a amostra de 500 elementos
ggplot(wines_sample_numeric, aes(x = "", y = alcohol)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(fill = "grey", alpha = 0.5) +
  labs(title = "Boxplot para Álcool", y = "Álcool")

# Boxplot da varíavel total.sulfur.dioxide padronizada para a amostra de 500 elementos
ggplot(wines_sample_numeric, aes(x = "", y = total.sulfur.dioxide)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(fill = "green", alpha = 0.5) +
  labs(title = "Boxplot para Dióxido de enxofre total", y = "Dióxido de enxofre total")

# Boxplot da varíavel free.sulfur.dioxide padronizada para a amostra de 500 elementos
ggplot(wines_sample_numeric, aes(x = "", y = free.sulfur.dioxide)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(fill = "lavender", alpha = 0.5) +
  labs(title = "Boxplot para Dióxido de enxofre livre", y = "Dióxido de enxofre livre")

# Boxplot da varíavel chlorides padronizada para a amostra de 500 elementos
ggplot(wines_sample_numeric, aes(x = "", y = chlorides)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(fill = "pink", alpha = 0.5) +
  labs(title = "Boxplot para Cloretos", y = "Cloretos")

# Boxplot da varíavel volatile.acidity padronizada para a amostra de 500 elementos
ggplot(wines_sample_numeric, aes(x = "", y = volatile.acidity)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(fill = "blueviolet", alpha = 0.5) +
  labs(title = "Boxplot para Acidez volátil", y = "Acidez volátil")

# Boxplot da varíavel sulphates padronizada para a amostra de 500 elementos
ggplot(wines_sample_numeric, aes(x = "", y = sulphates)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(fill = "beige", alpha = 0.5) +
  labs(title = "Boxplot para Sulfatos", y = "Sulfatos")

# Boxplot da varíavel pH padronizada para a amostra de 500 elementos
ggplot(wines_sample_numeric, aes(x = "", y = pH)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(fill = "brown", alpha = 0.5) +
  labs(title = "Boxplot para pH", y = "pH")

# Boxplot da varíavel citric.acid padronizada para a amostra de 500 elementos
ggplot(wines_sample_numeric, aes(x = "", y = citric.acid)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(fill = "orange", alpha = 0.5) +
  labs(title = "Boxplot para Ácido cítrico", y = "Ácido cítrico")

# Histograma com uma curva de densidade para a variável residual.sugar para a amostra de 500 elementos
ggplot(wines_sample_numeric, aes(x = residual.sugar)) +
  geom_histogram(
    aes(y = after_stat(density)),
    colour = 1,
    fill = "#FFFFFF",
    bins = 100
  ) +
  geom_density(
    lwd = 1.2,
    linetype = 2,
    colour = "red",
    fill = "red",
    alpha = 0.25
  ) +
  labs(title = "Histograma com curva de densidade para açúcar residual", x = "Açúcar residual", y = "Densidade")

# Histograma com uma curva de densidade para a variável fixed.acidity para a amostra de 500 elementos
ggplot(wines_sample, aes(x = fixed.acidity)) +
  geom_histogram(
    aes(y = after_stat(density)),
    colour = 1,
    fill = "#FFFFFF",
    bins = 100
  ) +
  geom_density(
    lwd = 1.2,
    linetype = 2,
    colour = "blue",
    fill = "blue",
    alpha = 0.25
  ) +
  labs(title = "Histograma com curva de densidade para acidez fixa", x = "Acidez fixa", y = "Densidade")

# Transforma uma coluna numérica em uma coluna fator
wines_standardized$colour <- as.factor(wines_standardized$colour)

# Histograma com uma curva de densidade para a variável density
ggplot(wines_standardized, aes(x = density)) +
  geom_histogram(
    aes(y = after_stat(density)),
    colour = 1,
    fill = "#FFFFFF",
    bins = 200
  ) +
  geom_density(
    lwd = 1.2,
    linetype = 2,
    colour = "#FF69B4",
    fill = "#FF69B4",
    alpha = 0.25
  )

# Histograma com uma curva de densidade para a variável residual.sugar
ggplot(wines_standardized, aes(x = residual.sugar)) +
  geom_histogram(
    aes(y = after_stat(density)),
    colour = 1,
    fill = "#FFFFFF",
    bins = 500
  ) +
  geom_density(
    lwd = 1.2,
    linetype = 2,
    colour = "#FF69B4",
    fill = "#FF69B4",
    alpha = 0.25
  )

# Histograma com uma curva de densidade para a variável fixed.acidity
ggplot(wines_standardized, aes(x = fixed.acidity)) +
  geom_histogram(
    aes(y = after_stat(density)),
    colour = 1,
    fill = "#FFFFFF",
    bins = 500
  ) +
  geom_density(
    lwd = 1.2,
    linetype = 2,
    colour = "#FF69B4",
    fill = "#FF69B4",
    alpha = 0.25
  )

# Histograma com uma curva de densidade para a variável alcohol
ggplot(wines_standardized, aes(x = alcohol)) +
  geom_histogram(
    aes(y = after_stat(density)),
    colour = 1,
    fill = "#FFFFFF",
    bins = 500
  ) +
  geom_density(
    lwd = 1.2,
    linetype = 2,
    colour = "#FF69B4",
    fill = "#FF69B4",
    alpha = 0.25
  )

# Histograma com uma curva de densidade para a variável chlorides
ggplot(wines_standardized, aes(x = chlorides)) +
  geom_histogram(
    aes(y = after_stat(density)),
    colour = 1,
    fill = "#FFFFFF",
    bins = 500
  ) +
  geom_density(
    lwd = 1.2,
    linetype = 2,
    colour = "#FF69B4",
    fill = "#FF69B4",
    alpha = 0.25
  )

# Cria os dados de contagem dos tipos de vinho
df_summary <- as.data.frame(table(wines_sample$colour))

# Substitui 0 por "branco" e 1 por "vermelho"
df_summary$Var1 <- factor(df_summary$Var1,
                          levels = c(0, 1),
                          labels = c("Branco", "Vermelho"))

# Calcula as porcentagens
df_summary$porcentagem <- df_summary$Freq / sum(df_summary$Freq) * 100

# Adiciona os rótulos de porcentagem
df_summary$label <- paste0(round(df_summary$porcentagem, 1), "%")

# Gráfico de pizza
ggplot(df_summary, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # Barras empilhadas
  coord_polar("y") +  # Converte para pizza
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +  # Adiciona rótulos
  labs(fill = "Tipo de Vinho", title = "Distribuição de Vinho Tinto e Branco") +
  theme_void() +  # Remove eixos e background
  theme(
    legend.position = "right",  # Posição da legenda
    plot.title = element_text(hjust = 0.5)  # Centraliza o título
  ) +
  scale_fill_manual(values = c("Branco" = "lightblue", "Vermelho" = "red"))  # Define as cores
# Remove as variáveis necessárias para exibir o gráfico
rm(df_summary)



#Execução dos cálculos da análise fatorial



# Converte o valor númerico para um fator
wines$colour <- as.factor(wines$colour)

# Remove a coluna 'quality'
# wines <- wines[, -12]

# Sorteia 5000 observações para compor uma "amostra da amostra"
wines_sample <- wines[sample(nrow(wines), size = 500, replace = FALSE), ]
wines_sample_red <- wines_red[sample(nrow(wines_red), size = 500, replace = FALSE), , drop = FALSE]
wines_sample_white <- wines_white[sample(nrow(wines_white), size = 500, replace = FALSE), ]

# Aplicando a padronização a cada coluna do dataframe
wines_standardized <- as.data.frame(lapply(wines_sample, standardize_z_score))

# Cria um subset apenas com as colunas que são númericas

wines_numeric <- wines[, sapply(wines, is.numeric)]

wines_sample_numeric <- wines_sample[, sapply(wines_sample, is.numeric)]
wines_sample_numeric_red <- wines_sample_red[, sapply(wines_sample_red, is.numeric)]
wines_sample_numeric_white <- wines_sample_white[, sapply(wines_sample_white, is.numeric), drop = FALSE]
wines_sample_numeric_std <- wines_sample_white[, sapply(wines_standardized, is.numeric), drop = FALSE]

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
    
    if (corr_test_result != 1) {
      corr_df <- rbind(
        corr_df,
        data.frame(
          variable_1 = colnames(wines[i]),
          corr_value = corr_test_result,
          variable_2 = colnames(wines[j]),
          corr_value_module = abs(corr_test_result)
        )
      )
    }
  }
}

# Remove as variáveis temporárias para o loop
rm(i, j, wines_numeric_cols, corr_test_result)

## Análise fatorial

# Cálculo do KMO

# Cálculo do KMO para amostra com vinhos mistos
kmo_method <- KMO(cor(wines_sample_numeric))
print(kmo_method)

# Cálculo do KMO para amostra com vinhos tintos
kmo_method_red <- KMO(cor(wines_sample_numeric_red))
print(kmo_method_red)

# Cálculo do KMO para amostra com vinhos brancos
kmo_method_white <- KMO(cor(wines_sample_numeric_white))
print(kmo_method_white)

# Cálculo do KMO para amostra com vinhos mistos padronizados
kmo_method_standardized <- KMO(cor(wines_sample_numeric_std))
print(kmo_method_standardized)

# Cálculo do KMO retirando as variáveis com um MSA < 0.5

# Recalculando o KMO para amostra com vinhos mistos
kmo_method_after_filter <- KMO(cor(remove_low_msa(wines_sample_numeric)))
print(kmo_method_after_filter)

# Recalculando o KMO para amostra com vinhos tintos
kmo_method_after_filter_red <- KMO(cor(remove_low_msa(wines_sample_numeric_red)))
print(kmo_method_after_filter_red)

# Recalculando o KMO para amostra com vinhos brancos
kmo_method_after_filter_white <- KMO(cor(remove_low_msa(wines_sample_numeric_white)))
print(kmo_method_after_filter_white)

# Recalculando o KMO para amostra com vinhos mistos padronizados
kmo_method_after_filter_std <- KMO(cor(remove_low_msa(wines_sample_numeric_std)))
print(kmo_method_after_filter_std)

# Teste de Barlett

# Alicando teste de Bartlett para amostra com vinhos mistos
bartlett.test(wines_sample_numeric)

# Alicando teste de Bartlett para amostra com vinhos tintos
bartlett.test(wines_sample_numeric_red)

# Alicando teste de Bartlett para amostra com vinhos brancos
bartlett.test(wines_sample_numeric_white)

# Alicando teste de Bartlett para amostra com vinhos mistos padronizados
bartlett.test(wines_sample_numeric_std)

# Matriz de correlação
filtered_wines_corr_matrix <- cor(remove_low_msa(wines_sample_numeric))
print(filtered_wines_corr_matrix)

# Fixando o cálculo do KMO para a amostra de vinhos mistos sem padronização
filtered_wines_kmo <- KMO(filtered_wines_corr_matrix)
print(filtered_wines_kmo)

# Critério de Kaiser para obtenção dos autovalores
filtered_wines_kaiser_eigenvalues <- eigen(filtered_wines_corr_matrix)$values
print(filtered_wines_kaiser_eigenvalues)

# Fixando o número ideal de fatores com base nos autovalores maiores que 1
filtered_wines_factors_kaiser <- sum(filtered_wines_kaiser_eigenvalues > 1) # critério pro número de fatores
print(filtered_wines_factors_kaiser)

# Scree Plot para obtenção dos autovalores
plot(
  filtered_wines_kaiser_eigenvalues,
  type = "b",
  main = "Scree Plot",
  xlab = "Número de Fatores",
  ylab = "Autovalores"
)
filtered_wines_second_derivative <- diff(diff(filtered_wines_kaiser_eigenvalues))
elbow <- which.max(filtered_wines_second_derivative) + 1

# Identificação do número ideal de fatores com base no intercepto do gráfico
lines(
  c(elbow, elbow),
  c(0, filtered_wines_kaiser_eigenvalues[elbow]),
  col = "red",
  lty = 2,
  lwd = 2
) # Aponta número de fatores ideal

# Fixando o número ideal de fatores segundo o critério de Kaiser
filtered_wines_n_factors <- filtered_wines_factors_kaiser

# Realizando a análise fatorial
filtered_wines_fa <- fa(
  wines_sample_numeric,
  nfactors = filtered_wines_n_factors,
  rotate = "none",
  fm = "minchi"
)
print(filtered_wines_fa)

# Realizando a análise fatorial aplicando rotação ortogonal
filtered_wines_fa_varimax <- fa(
  wines_sample_numeric,
  nfactors = filtered_wines_n_factors,
  rotate = "varimax",
  fm = "minchi"
)
print(filtered_wines_fa_varimax)

# Análise dos componentes principais após rotação ortogonal (Varimax)
filtered_wines_varimax_pca_result <- principal(wines_sample_numeric, nfactors = filtered_wines_n_factors, rotate = "varimax")
print(filtered_wines_varimax_pca_result)

# Análise dos resultados da análise fatorial após rotação via Varimax
print(filtered_wines_fa_varimax$loadings) # Cargas fatoriais
print(filtered_wines_fa_varimax$communalities) # Comunidade
print(filtered_wines_fa_varimax$Vaccounted) # Variância explicada

# Análise dos resultados da análise fatorial
print(filtered_wines_fa$loadings) # Cargas fatoriais
print(filtered_wines_fa$communalities) # Comunidade
print(filtered_wines_fa$Vaccounted) # Variância explicada

# Gráficos das cargas fatoriais (loadings)
fa.diagram(filtered_wines_fa_varimax, main = "Gráfico das cargas fatoriais rotacionadas via Varimax")
fa.diagram(filtered_wines_fa, main = "Gráfico das cargas fatoriais")

# Mapa de calor para as cargas da análise fatorial com rotação Varimax
corrplot(
  filtered_wines_fa_varimax$loadings,
  title = "Mapa de calor para cargas fatoriais rotacionadas via Varimax",
  is.corr = FALSE,
  method = "color",
  col = colorRampPalette(c("blue", "white", "red"))(200),
  type = "full",
  order = "original",
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "darkblue",
  tl.srt = 45,
  tl.cex = 1,
  diag = FALSE,
  mar = c(0, 0, 1, 0)
)

# Mapa de calor para as cargas da análise fatorial sem rotação
corrplot(
  filtered_wines_fa$loadings,
  title = "Mapa de calor para cargas fatoriais",
  is.corr = FALSE,
  method = "color",
  col = colorRampPalette(c("blue", "white", "red"))(200),
  type = "full",
  order = "original",
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "darkblue",
  tl.srt = 45,
  tl.cex = 1,
  diag = FALSE,
  mar = c(0, 0, 1, 0)
)