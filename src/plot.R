# Visualização do dendrograma
plot(hc, main = "Dendrograma - Cluster Hierárquico", xlab = "", ylab = "Altura")

# Escolher o número de clusters e adicionar aos dados
num_clusters <- 3  # Ajuste conforme necessário
clusters_hier <- cutree(hc, k = num_clusters)
wines_standardized$Cluster <- as.factor(clusters_hier)

# Visualização dos clusters (bidimensional)
library(ggplot2)
ggplot(wines_standardized, aes(x = wines_standardized[,1], y = wines_standardized[,2], color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "Clusters Hierárquicos",
       x = "Primeira variável padronizada", y = "Segunda variável padronizada") +
  theme_minimal()

