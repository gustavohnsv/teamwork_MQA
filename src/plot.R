# Carregao dataset completo 
set.seed(123)  # Fixar a semente para garantir reprodutibilidade

# Amostragem 
sample_size <- floor(nrow(wines))  # Calcula 10% do número de linhas
sample_indices <- sample(seq_len(nrow(wines)), size = sample_size)  # Seleciona os índices aleatórios
wines_sampled <- wines[sample_indices, ]  # Cria o dataset amostrado

library (ggplot2)

# Selecionar as variáveis de interesse
selected_data <- wines_sampled[, c("fixed.acidity", "residual.sugar")]

# Padronizar os dados
selected_data_standardized <- scale(selected_data)

# Construir a matriz de distância
dist_matrix <- dist(selected_data_standardized, method = "euclidean")

# Realizar o clustering hierárquico
hc <- hclust(dist_matrix, method = "ward.D")

# Plotar o dendrograma
plot(hc, main = "Dendrograma - Clustering Hierárquico (Sem amostragem)", xlab = "", ylab = "Altura", labels = FALSE)
rect.hclust(hc, k = 3, border = c("red", "blue", "green"))  # Para destacar os clusters
# Atribuir clusters aos dados amostrados
clusters <- cutree(hc, k = 3)
selected_data$cluster <- as.factor(clusters)

# Plotar o dendrograma focando nas hierarquias mais baixas
plot(hc, main = "Dendrograma - Clustering Hierárquico (Corte Inferior)",
     xlab = "Observações (Álcool e Açúcar)", ylab = "Altura", labels = FALSE)
rect.hclust(hc, k = 3, border = c("red", "blue", "green"))  # Para destacar os clusters
# Adicionando uma legenda explicativa
legend("topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3"),
       col = c("red", "blue", "green"), lty = 1, cex = 0.8)
# Adicionando texto no gráfico
text(x = 760, y = 1000, "Maior acidez fixa", col = "red")
# Atribuir clusters aos dados amostrados
clusters <- cutree(hc, k = 3)
selected_data$cluster <- as.factor(clusters)

# Normalizando os valores com z-score
selected_data_standardized <- as.data.frame(scale(selected_data[, c("fixed.acidity", "residual.sugar")]))
colnames(selected_data_standardized) <- c("acidity", "sugar")  # Renomeia para simplificar os eixos
selected_data_standardized$cluster <- selected_data$cluster    # Adiciona os clusters ao dataset

# Gráfico de dispersão com os valores normalizados
ggplot(selected_data_standardized, aes(x = acidity, y = sugar, color = cluster)) +
  geom_point(size = 1) +  # Ajusta o tamanho dos pontos
  labs(title = "Clusters (Normalizados): Acidez x Açúcar",  # Ajusta o título
       x = "Acidez Normalizada", y = "Açúcar Normalizado",    # Ajusta os rótulos dos eixos
       color = "Cluster") +                                 # Ajusta o título da legenda
  theme_minimal(base_size = 14) +                           # Tema minimalista com tamanho da fonte ajustado
  theme(
    legend.position = "right",                             # Coloca a legenda à direita
    plot.title = element_text(hjust = 0.5)                 # Centraliza o título
  ) +
  scale_color_manual(values = c("red", "blue", "green"))    # Cores personalizadas para os clusters

# Aplicando transformação logarítmica
selected_data_transformed <- selected_data[, c("fixed.acidity", "residual.sugar")]
selected_data_transformed$residual.sugar <- log1p(selected_data_transformed$residual.sugar)  # log(1 + x)
selected_data_transformed$fixed.acidity <- log1p(selected_data_transformed$fixed.acidity)

# Padronizando os dados transformados
selected_data_standardized <- as.data.frame(scale(selected_data_transformed))
colnames(selected_data_standardized) <- c("acidity", "sugar")
selected_data_standardized$cluster <- selected_data$cluster

# Gráfico após transformação logarítmica
ggplot(selected_data_standardized, aes(x = acidity, y = sugar, color = cluster)) +
  geom_point(size = 1.5) +
  labs(title = "Clusters Log-Transformados Acidez x Açúcar",
       x = "Acidez Log-Transformada", y = "Açúcar Log-Transformada") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("red", "blue", "green")) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

# Função para normalização robusta
robust_scale <- function(x) {
  (x - median(x)) / IQR(x)
}

# Aplicando a escala robusta
selected_data_robust <- selected_data[, c("fixed.acidity", "residual.sugar")]
selected_data_robust$residual.sugar <- robust_scale(selected_data_robust$residual.sugar)
selected_data_robust$fixed.acidity <- robust_scale(selected_data_robust$fixed.acidity)

# Convertendo para data frame com clusters
selected_data_robust <- as.data.frame(selected_data_robust)
colnames(selected_data_robust) <- c("acidity", "sugar")
selected_data_robust$cluster <- selected_data$cluster

# Gráfico com escala robusta
ggplot(selected_data_robust, aes(x = acidity, y = sugar, color = cluster)) +
  geom_point(size = 1.5) +
  labs(title = "Clusters: Robust Scaled Acidity x Sugar",
       x = "Robust Scaled Acidity", y = "Robust Scaled Sugar") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("red", "blue", "green")) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

# Exclua a coluna cluster, caso tenha sido adicionada
selected_data_standardized <- selected_data_standardized[, 1:2]

# Recalcule o centroid_global
centroid_global <- colMeans(selected_data_standardized)
TSS <- sum(rowSums((selected_data_standardized - centroid_global)^2))

# Continue com o cálculo
WSS <- 0
for (k in unique(clusters)) {
  cluster_points <- selected_data_standardized[clusters == k, ]
  centroid_cluster <- colMeans(cluster_points)
  WSS <- WSS + sum(rowSums((cluster_points - centroid_cluster)^2))
}

# R² final
R_squared <- 1 - (WSS / TSS)
print(paste("R²:", round(R_squared, 4)))
