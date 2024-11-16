# Adiciona uma coluna indicando a cor do vinho de cada dataset
white_wines$colour <- "white"
red_wines$colour <- "red"

# Combina os datasets para um único
wines <- rbind(white_wines, red_wines)

# Retira possíveis observações com campos NA
wines <- na.omit(wines)

# Renomeia a coluna 'colour' para 'is.red'
colnames(wines)[13] <- "is.red"

# Substitui a cor por números, sendo 1 para tinto e 0 para branco
wines <- wines %>%
  mutate(is.red = ifelse(is.red == "red", 1, 0))

# Converte o valor númerico para um fator
wines$is.red <- as.factor(wines$is.red)

# Sorteia 5000 observações para compor uma "amostra da amostra"
wines_sample <- wines[sample(nrow(wines), size = 500, replace = FALSE), ]

# Aplicando a padronização a cada coluna do dataframe
wines_standardized <- as.data.frame(lapply(wines_sample, standardize_z_score))

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

## Regressão Logística

# Preparação das variáveis para os modelos de regressão de Ridge e Lasso
# x <- model.matrix(is.red ~ ., data = wines_sample)[, -1]
# y <- as.numeric(wines_sample$is.red)

# Modelos de regressão de Ridge e de Lasso
# ridge_model <- cv.glmnet(x, y, family = "binomial", alpha = 0) # Penaliza o modelo de maneira mais suave
# lasso_model <- cv.glmnet(x, y, family = "binomial", alpha = 1) # Penaliza o modelo drasticamente

# Modelo de regressão logística com todas as variáveis do modelo
# A variável 'density' foi removida pois apresentava um valor VIF muito alto (~10)
logistic_model <- glm(is.red ~ . - density, data = wines_standardized, family = binomial(link = "logit"))

# Modelo de regressão logística após a etapa de Stepwise, que mantém as variáveis mais significativas
stepwise_logistic_model <- step(logistic_model, direction = "both", k = log(nrow(wines_sample)), trace = TRUE)

# Razão de chances para o modelo que não passou por refinamento
default_OR <- odds.ratio(logistic_model)

# Razão de chances para o modelo que passou pelo processo de Stepwise
stepwise_OR <- odds.ratio(stepwise_logistic_model)

# Visualização dos gráficos de probabilidade para cada uma das variáveis para o modelo sem refinamento
marginalModelPlots(logistic_model)

# Visualização dos gráficos de probabilidade para cada uma das variáveis para o modelo que passou pelo processo de Stepwise
marginalModelPlots(stepwise_logistic_model)

# Obtenha as probabilidades previstas
probabilidades <- predict(logistic_model, type = "response")

# Defina um limiar (threshold) para a classificação
limiar <- 0.5  # Este valor pode ser ajustado com base nas necessidades do seu problema
previsoes <- ifelse(probabilidades > limiar, 1, 0)  # 1 para positivo, 0 para negativo

# Crie a matriz de confusão
matriz_confusao <- table(Real = wines_sample$is.red, Previsto = previsoes)

# Visualize a matriz de confusão
print(matriz_confusao)

# Análise de Cluster: açúcar residual x densidade
# Procedimento hierárquico
# Distância Euclidiana entre cada objeto 
sugar_density_df <- data.frame(
  sugar = wines_standardized$residual.sugar,
  density = wines_standardized$density
)
# Retirada de outliers por quantis
filtered_sugar_density_df <- na.omit(no_outliers_df(sugar_density_df))
euclidian_dist <- dist(filtered_sugar_density_df)

# Agrupamento hierárquico aglomerativo por single linkage
hierarchical_groups <- hclust(euclidian_dist, method = "single")

# Visualizar o dendrograma
plot(hierarchical_groups, main = "Dendrograma - Single Linkage", sub = "", xlab = "")

# Procedimento não hierárquico
#Método da silhueta para encontrar k ideal
nbclust_wss <- NbClust(filtered_sugar_density_df, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "silhouette")
print(nbclust_wss$Best.nc)

k <- 4 # Número ideal de clusters dado
non_hierarchical_kmeans <- kmeans(filtered_sugar_density_df, centers = k, nstart = 25)

# Clusters atribuídos
non_hierarchical_kmeans$cluster

# Tamanhos dos clusters
non_hierarchical_kmeans$size

# Centros dos clusters
non_hierarchical_kmeans$centers

# Visualização dos dados clusterizados
non_hierarchical_cluster_data <- data.frame(filtered_sugar_density_df, Cluster = as.factor(non_hierarchical_kmeans$cluster))
ggplot(non_hierarchical_cluster_data, aes(x = non_hierarchical_cluster_data$sugar, y = non_hierarchical_cluster_data$density, color = Cluster)) +
  geom_point() +
  theme_minimal()
