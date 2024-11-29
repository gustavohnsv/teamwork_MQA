# Carregar bibliotecas necessárias
library(psych)
library(GPArotation)
library(Matrix)
library(ggplot2)

# Definir semente para reprodutibilidade
set.seed(123)

# Amostragem do dataset
sample_size <- nrow(wines)
sample_indices <- sample(seq_len(nrow(wines)), size = sample_size)
wines_sampled <- wines[sample_indices, ]

# Padronizar os dados
wines_standardized <- as.data.frame(scale(wines_sampled))

# Calcular a matriz de correlação
corr_matrix <- cor(wines_standardized)

# Corrigir a matriz de correlação se necessário
eigenvalues <- eigen(corr_matrix)$values
if (any(eigenvalues <= 0)) {
  epsilon <- 1e-5
  corr_matrix <- nearPD(corr_matrix + epsilon * diag(ncol(corr_matrix)))$mat
}

# Determinar o número ideal de fatores (Critério de Kaiser)
num_factors <- sum(eigen(corr_matrix)$values > 1)

# Visualizar o Scree Plot
scree(corr_matrix)

# Realizar a análise fatorial
fa_result <- fa(r = corr_matrix, nfactors = num_factors, rotate = "varimax", scores = "tenBerge")

# Resumo dos resultados da análise fatorial
print(fa_result)

# Calcular os escores fatoriais
factor_scores <- factor.scores(wines_standardized, fa_result, method = "tenBerge")$scores

# Adicionar os escores fatoriais ao dataframe original
wines_factors <- as.data.frame(factor_scores)
colnames(wines_factors) <- paste0("Factor", 1:ncol(factor_scores))
wines_final <- cbind(wines_sampled, wines_factors)

# Visualizar os escores fatoriais (opcional)
head(wines_final)

# ---- 1. PLOT DOS FATORES PRINCIPAIS ---- #
# Plotar os fatores principais (Fator 1 e Fator 2)
plot1 <- ggplot(wines_final, aes(x = Factor1, y = Factor2)) +
  geom_point(size = 2) +
  labs(
    title = "Visualização dos Fatores 1 e 2",
    x = "Fator 1",
    y = "Fator 2"
  ) +
  theme_minimal()

# Mostrar o primeiro plot
print(plot1)

# ---- 2. PLOT DOS FATORES PRINCIPAIS COM CLUSTERING K-MEANS ---- #
# Realizar o clustering K-means com 3 clusters
set.seed(123)
kmeans_result <- kmeans(wines_factors[, c("Factor1", "Factor2")], centers = 3)
wines_factors$cluster_kmeans <- as.factor(kmeans_result$cluster)

# Plotar os fatores principais com clusters
plot2 <- ggplot(wines_factors, aes(x = Factor1, y = Factor2, color = cluster_kmeans)) +
  geom_point(size = 2) +
  labs(
    title = "Visualização dos Fatores 1 e 2 com Clustering K-means",
    x = "Fator 1",
    y = "Fator 2",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("red", "blue", "green"))

# Mostrar o segundo plot
print(plot2)

# ---- 3. DENDROGRAMA DO CLUSTERING HIERÁRQUICO ---- #
# Construir a matriz de distância usando os escores fatoriais
dist_matrix <- dist(wines_factors[, c("Factor1", "Factor2")], method = "euclidean")

# Realizar o clustering hierárquico
hc <- hclust(dist_matrix, method = "ward.D2")

# Plotar o dendrograma
plot3 <- plot(hc, main = "Dendrograma - Clustering Hierárquico", xlab = "", ylab = "Altura", labels = FALSE)
rect.hclust(hc, k = 3, border = c("red", "blue", "green"))

# Mostrar o terceiro plot
plot3
