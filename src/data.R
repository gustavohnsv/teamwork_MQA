# Converte o valor númerico para um fator
wines$colour <- as.factor(wines$colour)

# Sorteia 5000 observações para compor uma "amostra da amostra"
wines_sample <- wines[sample(nrow(wines), size = 500, replace = FALSE), ]
wines_sample_red <- wines_red[sample(nrow(wines_red), size = 500, replace = FALSE), , drop = FALSE]
wines_sample_white <- wines_white[sample(nrow(wines_white), size = 500, replace = FALSE),]

# Aplicando a padronização a cada coluna do dataframe
wines_standardized <- as.data.frame(lapply(wines_sample, standardize_z_score))

# Cria um subset apenas com as colunas que são númericas
wines_numeric <- wines[, sapply(wines, is.numeric)]

wines_sample_numeric <- wines_sample[, sapply(wines_sample, is.numeric)]
wines_sample_numeric_red <- wines_sample_red[, sapply(wines_sample_red, is.numeric)]
wines_sample_numeric_white <- wines_sample_white[, sapply(wines_sample_white, is.numeric), drop = FALSE]

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

## Análise fatorial

# Cálculo do KMO

kmo_method <- KMO(cor(wines_sample_numeric))
print(kmo_method)

kmo_method_red <- KMO(cor(wines_sample_numeric_red))
print(kmo_method_red)

kmo_method_white <- KMO(cor(wines_sample_numeric_white))
print(kmo_method_white)

# Teste de Barlett


## Exemplo Análise Fatorial
data("swiss")
View(swiss)
# Matriz de correlação
corr_matrix <- cor(swiss)
print(corr_matrix)

# Cálculo KMO
kmo <- KMO(corr_matrix)
print(kmo)

# Teste de Barlett
barlett_test <- cortest.bartlett(cor(swiss), n = nrow(swiss))
print(barlett_test)

# Cálculo do MSA
print(kmo$MSAi)

# Determinação de número de fatores

# Critério de Kaiser 
kaiser_eigenvalues <- eigen(corr_matrix)$values
print(kaiser_eigenvalues)
factors_kaiser <- sum(kaiser_eigenvalues >1) # critério pro número de fatores
print(factors_kaiser)

# Scree Plot
plot(kaiser_eigenvalues, type = "b", main = "Scree Plot", xlab = "Número de Fatores", ylab = "Autovalores")
second_derivative <- diff(diff(kaiser_eigenvalues))
elbow <- which.max(abs(second_derivative)) + 1

lines(c(elbow,elbow), c(0, kaiser_eigenvalues[elbow]), col="red", lty =2, lwd =2 ) # Aponta número de fatores ideal

# Análise fatorial final
factor_number <- factors_kaiser
factorial_analysis <- fa(r = corr_matrix, nfactors = factor_number, rotate ="varimax", fm = "ml")
print(factorial_analysis)
## Regressão Logística

# Preparação das variáveis para os modelos de regressão de Ridge e Lasso
# x <- model.matrix(is.red ~ ., data = wines_sample)[, -1]
# y <- as.numeric(wines_sample$is.red)

# Modelos de regressão de Ridge e de Lasso
# ridge_model <- cv.glmnet(x, y, family = "binomial", alpha = 0) # Penaliza o modelo de maneira mais suave
# lasso_model <- cv.glmnet(x, y, family = "binomial", alpha = 1) # Penaliza o modelo drasticamente

# Modelo de regressão logística com todas as variáveis do modelo
# A variável 'density' foi removida pois apresentava um valor VIF muito alto (~10)
# logistic_model <- glm(colour ~ . - density, data = wines_standardized, family = binomial(link = "logit"))

# Modelo de regressão logística após a etapa de Stepwise, que mantém as variáveis mais significativas
# stepwise_logistic_model <- step(logistic_model, direction = "both", k = log(nrow(wines_sample)), trace = TRUE)

# Razão de chances para o modelo que não passou por refinamento
# default_OR <- odds.ratio(logistic_model)

# Razão de chances para o modelo que passou pelo processo de Stepwise
# stepwise_OR <- odds.ratio(stepwise_logistic_model)

# Visualização dos gráficos de probabilidade para cada uma das variáveis para o modelo sem refinamento
# marginalModelPlots(logistic_model)

# Visualização dos gráficos de probabilidade para cada uma das variáveis para o modelo que passou pelo processo de Stepwise
# marginalModelPlots(stepwise_logistic_model)

# Obtenha as probabilidades previstas
# probabilidades <- predict(logistic_model, type = "response")

# Defina um limiar (threshold) para a classificação
# limiar <- 0.5  # Este valor pode ser ajustado com base nas necessidades do seu problema
# previsoes <- ifelse(probabilidades > limiar, 1, 0)  # 1 para positivo, 0 para negativo

# Crie a matriz de confusão
# matriz_confusao <- table(Real = wines_sample$colour, Previsto = previsoes)

# Visualize a matriz de confusão
# print(matriz_confusao)

# Tabela de frequência para açúcar residual
# Análise de Cluster: açúcar residual x densidade
# Procedimento hierárquico
#sugar_density_df <- data.frame(
#  sugar = wines_standardized$residual.sugar,
#  acidity = wines_standardized$fixed.acidity
#)

# Retirada de outliers por quantis (Apenas para procedimento não hierárquico)
#filtered_sugar_density_df <- na.omit(no_outliers_df(sugar_density_df))

# Distância euclidiana
#euclidian_dist <- dist(sugar_density_df)

# Agrupamento hierárquico aglomerativo por Ward Method
#hierarchical_groups <- hclust(euclidian_dist, method = "ward.D2")

# Visualizar o dendrograma
#plot(hierarchical_groups, main = "Dendrograma - Ward Method", labels = FALSE, sub = "", xlab = "", cex = 0.8, hang = -1)

# Dividindo o dendograma em 3 clusters e desenhando os retângulos
# rect.hclust(hierarchical_groups, k = 3, border = "green")

# Mudando todas as colunas para númerico
# sugar_density_df <- sugar_density_df[, sapply(sugar_density_df, is.numeric)]

# Dividindo em 3 clusters
# cluster_assignments <- cutree(hierarchical_groups, k = 3)

# Reduzindo a dimensionalidade para 2D com PCA
# pca_result <- prcomp(sugar_density_df, scale. = TRUE)

# Adicionando os clusters como uma coluna para facilitar a visualização
#plot_data <- data.frame(
#  PC1 = pca_result$x[, 1],
#  PC2 = pca_result$x[, 2],
#  Cluster = as.factor(cluster_assignments)
#)

# Cálculo dos limites convexos de cada cluster (determina a geometria dos grupos)
#hulls <- plot_data %>%
#  group_by(Cluster) %>%
#  slice(chull(PC1, PC2))

# Gráfico de dispersão de cada amostra em seus respectivos clusters
# ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
#   geom_point(size = 3) +  # Pontos
#   geom_polygon(data = hulls, aes(fill = Cluster, group = Cluster), 
#                alpha = 0.2, color = "black") +  # Contornos
#   labs(title = "Visualização de 3 Clusters após redução de dimensionalidade para 2D", 
#        x = "Componente Principal 1", 
#        y = "Componente Principal 2",
#        color = "Cluster",
#        fill = "Cluster") +
#   theme_minimal()

# Procedimento não hierárquico

# Método usando índice de Calinski-Harabasz  (mais próximo do método do cotovelo e mais "confiável")
# nbclust_elbow <- NbClust(filtered_sugar_density_df, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "ch")
# print(nbclust_elbow$Best.nc)
# 
# # Método da silhueta para encontrar k ideal
# nbclust_wss <- NbClust(filtered_sugar_density_df, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "silhouette")
# print(nbclust_wss$Best.nc)
# 
# k <- 2 # Número ideal de clusters dado pelo método da silhueta
# non_hierarchical_kmeans <- kmeans(filtered_sugar_density_df, centers = k, nstart = 25)
# 
# # Clusters atribuídos
# non_hierarchical_kmeans$cluster
# 
# # Tamanhos dos clusters
# non_hierarchical_kmeans$size
# 
# # Centros dos clusters
# non_hierarchical_kmeans$centers
# 
# # Visualização dos dados clusterizados
# non_hierarchical_cluster_data <- data.frame(filtered_sugar_density_df, Cluster = as.factor(non_hierarchical_kmeans$cluster))
# ggplot(non_hierarchical_cluster_data, aes(x = sugar, y = acidity, color = Cluster)) +
#   geom_point() +
#   theme_minimal()
# 
# # Método do cotovelo para encontrar k ideal
# 
# # Valores de K variam entre 2 e 10, ou seja, de 2 a 10 clusters
# k_values <- 2:10
# 
# # Vetor que armazenará a soma dos quadrados intra-clusters
# wss_values <- numeric(length(k_values))
# 
# # Itera sobre os diferentes valores de K para calcular a soma dos quadrados intra-clusters
# for (i in seq_along(k_values)) {
# 
#   # Seleciona o cluster atual
#   k <- k_values[i]
#   
#   # Calcula o K-means para o número do iterador
#   kmeans_result <- kmeans(filtered_sugar_density_df, centers = k, nstart = 25, iter.max = 100)
#   
#   # Armazena a soma total das distâncias quadradas intra-clusters parao número do iterador
#   wss_values[i] <- kmeans_result$tot.withinss
# }
# 
# # Exibe os K valores
# print(k_values)
# 
# # Exibe as somas dos quadrados intra-clusters
# print(wss_values)
# 
# # Gráfico de linha da mudança da soma das distâncias quadradas intra-clusters conforme o número de clusters
# elbow_plot <- data.frame(k = k_values, wss = wss_values)
# ggplot(elbow_plot, aes(x = k, y = wss)) +
#   geom_line(color = "blue", size = 1) +
#   geom_point(color = "red", size = 2) +
#   theme_minimal() +
#   labs(title = "Método do Cotovelo",
#        x = "Número de Clusters (k)",
#        y = "Soma dos Quadrados Intra-Cluster (WSS)") +
#   # diff() calcula as diferenças sucessivas entre os elementos consecutivos do vetor
#   # diff(diff()) calcula as diferenças das diferenças, ou seja, a aceleração da redução
#   geom_vline(xintercept = which.min(diff(diff(wss_values))) + 1, 
#              linetype = "dashed", color = "darkgreen")
# 
# # Valores obtidos (Silhueta: 2, CH: 6, Cotovelo: 6)
# elbow_k <- 6
# 
# # Recalcula K-means com K fixado pelo elbow_k
# elbow_kmeans_result <- kmeans(filtered_sugar_density_df, centers = elbow_k, nstart = 25)
# 
# # Armazena o resultado para exibiro gráfico
# elbow_clustered_data <- data.frame(filtered_sugar_density_df, Cluster = as.factor(elbow_kmeans_result$cluster))
# 
# # Cálculo dos limites convexos de cada cluster (determina a geometria dos grupos)
# hulls <- elbow_clustered_data %>%
#   group_by(Cluster) %>%
#   slice(chull(sugar, acidity))
# 
# # Gráfico de dispersão de cada amostra em seus respectivos clusters
# ggplot(elbow_clustered_data, aes(x = sugar, y = acidity, color = Cluster)) +
#   geom_point(size = 3) +
#   geom_polygon(data = hulls, aes(fill = Cluster, group = Cluster), alpha = 0.2, color = "black") +
#   labs(title = paste("Clusters Identificados com K =", elbow_k),
#        x = "Açúcar Residual",
#        y = "Acidez Fixa",
#        color = "Cluster")
# 
# # Exemplo análise de Cluster
# # Data Frame
# data("mtcars")
# View(mtcars)
# 
# mtcars_summ <- mtcars[,c(1,2)]
# 
# # Z-Score (padronização)
# mtcars_summ_stand <- as.data.frame(lapply(mtcars_summ, standardize_z_score))
# 
# # Distância Euclidiana 
# mtcars_euclidian_dist <-  dist(mtcars_summ_stand)
# 
# # Agrupamento hierárquico aglomerativo por single linkage
# mtcars_hierarchical_groups <- hclust(mtcars_euclidian_dist, method = "single")
# mtcars_hierarchical_groups$labels <- rownames(mtcars)
# # Dendograma
# plot(mtcars_hierarchical_groups, main = "Dendrograma - mpg & cyl",sub = "", xlab = "", ylab= "Distance")
# 
# # Procedimento não hierárquico
# # Retirada de outliers
# filtered_mtcars_summ <- na.omit(no_outliers_df(mtcars_summ))
# 
# #Método da silhueta para encontrar k ideal
# mt_cars_silhou <- NbClust(filtered_mtcars_summ, 
#                           distance = "euclidean", 
#                           min.nc = 2, max.nc = 10, method = "kmeans", 
#                           index = "silhouette")
# print(mt_cars_silhou$Best.nc)
# mtcars_k <- 10 # Número ideal de clusters dado pelo método da silhueta
# mtcars_kmeans <- kmeans(filtered_mtcars_summ, centers = mtcars_k, nstart = 25)
# 
# # Clusters atribuídos
# mtcars_kmeans$cluster
# # Tamanhos dos clusters
# mtcars_kmeans$size
# # Centros dos clusters
# mtcars_kmeans$centers
# 
# # Visualização dos dados clusterizados
# mtcars_mean_data <- data.frame(filtered_mtcars_summ, Cluster = as.factor(mtcars_kmeans$cluster))
# ggplot(mtcars_mean_data, aes(x = cyl, y = mpg, color = Cluster)) + geom_point() + theme_minimal()
