# Combinar os datasets e remover valores ausentes
wines <- rbind(white_wines, red_wines)
wines <- na.omit(wines)

# Padronizar as variáveis numéricas (Z-score)
wines_standardized <- as.data.frame(lapply(wines[, sapply(wines, is.numeric)], standardize_z_score))

# Criar uma matriz de correlação
library(pheatmap)
corr_matrix <- cor(wines_standardized, use = "complete.obs")
pheatmap(corr_matrix, display_numbers = TRUE, fontsize_number = 8)

# Método hierárquico
dist_matrix <- dist(wines_standardized, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D")
plot(hc, main = "Dendrograma - Cluster Hierárquico")

# Escolher número de clusters
clusters_hier <- cutree(hc, k = 3)

# Método K-means
set.seed(123)
kmeans_result <- kmeans(wines_standardized, centers = 3)

# Salvar clusters nos dados padronizados
wines_standardized$cluster_hier <- clusters_hier
wines_standardized$cluster_kmeans <- kmeans_result$cluster

# Visualização de clusters
library(factoextra)
fviz_cluster(kmeans_result, data = wines_standardized)
