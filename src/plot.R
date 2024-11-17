# Carregao dataset completo 
set.seed(123)  # Fixar a semente para garantir reprodutibilidade

# Amostragem 
sample_size <- floor(nrow(wines))  # Calcula 5% do número de linhas
sample_indices <- sample(seq_len(nrow(wines)), size = sample_size)  # Seleciona os índices aleatórios
wines_sampled <- wines[sample_indices, ]  # Cria o dataset amostrado

# Selecionar as variáveis de interesse
selected_data <- wines_sampled[, c("fixed.acidity", "residual.sugar")]

# Padronizar os dados
selected_data_standardized <- scale(selected_data)

# Construir a matriz de distância
dist_matrix <- dist(selected_data_standardized, method = "euclidean")

# Realizar o clustering hierárquico
hc <- hclust(dist_matrix, method = "ward.D")

# Plotar o dendrograma
plot(hc, main = "Dendrograma - Clustering Hierárquico (Sem amostragem)", xlab = "", ylab = "Altura")
rect.hclust(hc, k = 3, border = c("red", "blue", "green"))  # Para destacar os clusters

# Atribuir clusters aos dados amostrados
clusters <- cutree(hc, k = 3)
selected_data$cluster <- as.factor(clusters)

# Gráfico de dispersão para visualizar os clusters
library(ggplot2)

ggplot(selected_data, aes(x = fixed.acidity, y = residual.sugar, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "Clusters: Acidez Fixa x Açúcar Residual (Amostragem 5%)",
       x = "Acidez Fixa", y = "Açúcar Residual") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green"))
