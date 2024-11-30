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

# Biblioteca para métodos cotovelo e silhueta para escolha de número de clusters na análise não hierárquica
if (!requireNamespace("NbClust", quietly = TRUE)) {
  install.packages("NbClust")
}
library(NbClust)

# Biblioteca para ajudar a criar tabelas de distribuição de frequência
if (!requireNamespace("formattable", quietly = TRUE)) {
  install.packages("formattable")
}
library(formattable)




# Definindo o diretório de trabalho para a localização do arquivo "winequality.csv"
setwd(dirname(dirname(getActiveDocumentContext()$path)))

# Caminho para o arquivo único com todos os vinhos
file_path <- "./data/winequality-both.csv"

if (!file.exists(file_path)) { # Verifica se o arquivo existe
  message("File not found at ", file_path)
} else {
  wines <- tryCatch({
    # Leitura dos dados do arquivo único
    read.csv(file_path, header = TRUE, sep = ",")  
  }, error = function(e) {
    message("Error reading file: ", e$message)
    NULL
  })
  
  if (!is.null(wines)) { # Verifica se os dados foram lidos com sucesso
    message("File read successfully")
  }
}

# Retira possíveis observações com campos NA
wines <- na.omit(wines)

# Remove o caminho do arquivo
rm(file_path)


# Função para calcular a moda
moda <- function(data) {
  data <- data[!is.na(data)]
  uniq_value <- unique(data)
  uniq_value[which.max(tabulate(match(data, uniq_value)))]
}

# Função para exibir coeficiente de determinação na regressão linear simples
simple_linear_regression_sqr_r <- function(y,x){
  model <- lm(y~x)
  return(summary(model)$r.square)
}

# Padronização (Z-score)
standardize_z_score <- function(x) {
  if(is.numeric(x)) {
    return((x - mean(x)) / sd(x))
  } else {
    return(x)
  }
}

standardize_log <- function(x) {
  if (is.numeric(x)) {
    return(log(x+1))
  } else {
    return(x)
  }
}

# Função para remover outliers de um dataframe com base nos quartis
no_outliers_df <- function(dataframe){
  dataframe %>%
    mutate(across(where(is.numeric), ~ {
      Q1 <- quantile(.x, 0.25, na.rm = TRUE)
      Q3 <- quantile(.x, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower <- Q1 - 1.5 * IQR
      upper <- Q3 + 1.5 * IQR
      ifelse(.x >= lower & .x <= upper, .x, NA) # Substitui outliers por NA
    }))
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
  intervals <- cut(column, breaks = seq(min(column), max(column), by = h), right = FALSE)
  
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
  freq_tables <- formattable(freq_tables, list(
    Intervalos = formatter("span", style = x ~ style(color = "black", font.weight = "bold", font.size = "16px")),
    Frequencia_Absoluta = formatter("span", style = x ~ style(color = "black", font.weight = "bold", font.size = "16px")),
    Frequencia_Relativa = formatter("span", style = x ~ style(color = "black", font.weight = "bold", font.size = "16px")),
    Frequencia_Absoluta_Acumulada = formatter("span", style = x ~ style(color = "black", font.weight = "bold", font.size = "16px")),
    Frequencia_Relativa_Acumulada = formatter("span", style = x ~ style(color = "black", font.weight = "bold", font.size = "16px"))
  ))
  
  return(freq_tables)
}
# Converte o valor númerico para um fator
wines$colour <- as.factor(wines$colour)

# Sorteia 500 observações para compor uma "amostra da amostra"
wines_sample <- wines[sample(nrow(wines), size = 500, replace = FALSE), ]

# Aplicando a padronização a cada coluna do dataframe
wines_standardized <- as.data.frame(lapply(wines_sample, standardize_z_score))

# Tabela de frequência para açúcar residual
# Análise de Cluster: açúcar residual x densidade
# Procedimento hierárquico
sugar_density_df <- data.frame(
  sugar = wines_standardized$residual.sugar,
  acidity = wines_standardized$fixed.acidity
)

# Retirada de outliers por quantis (Apenas para procedimento não hierárquico)
filtered_sugar_density_df <- na.omit(no_outliers_df(sugar_density_df))

create_freq_tables(wines_sample$fixed.acidity)
create_freq_tables(wines_sample$residual.sugar)
# Distância euclidiana
euclidian_dist <- dist(sugar_density_df)

# Agrupamento hierárquico aglomerativo por Ward Method
hierarchical_groups <- hclust(euclidian_dist, method = "ward.D2")

# Visualizar o dendrograma
plot(hierarchical_groups, main = "Dendrograma - Ward Method", labels = FALSE, sub = "", xlab = "", cex = 0.8, hang = -1)

# Dividindo o dendograma em 3 clusters e desenhando os retângulos
rect.hclust(hierarchical_groups, k = 3, border = "green")

# Mudando todas as colunas para númerico
sugar_density_df <- sugar_density_df[, sapply(sugar_density_df, is.numeric)]

# Dividindo em 3 clusters
cluster_assignments <- cutree(hierarchical_groups, k = 3)

# Reduzindo a dimensionalidade para 2D com PCA
pca_result <- prcomp(sugar_density_df, scale. = TRUE)

# Adicionando os clusters como uma coluna para facilitar a visualização
plot_data <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  Cluster = as.factor(cluster_assignments)
)

# Cálculo dos limites convexos de cada cluster (determina a geometria dos grupos)
hulls <- plot_data %>%
  group_by(Cluster) %>%
  slice(chull(PC1, PC2))

# Gráfico de dispersão de cada amostra em seus respectivos clusters
ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3) +  # Pontos
  geom_polygon(data = hulls, aes(fill = Cluster, group = Cluster), 
               alpha = 0.2, color = "black") +  # Contornos
  labs(title = "Visualização de 3 Clusters após redução de dimensionalidade para 2D", 
       x = "Componente Principal 1", 
       y = "Componente Principal 2",
       color = "Cluster",
       fill = "Cluster") +
  theme_minimal()

# Procedimento não hierárquico

# Método usando índice de Calinski-Harabasz  (mais próximo do método do cotovelo e mais "confiável")
nbclust_elbow <- NbClust(filtered_sugar_density_df, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "ch")
print(nbclust_elbow$Best.nc)

# Método da silhueta para encontrar k ideal
nbclust_wss <- NbClust(filtered_sugar_density_df, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "silhouette")
print(nbclust_wss$Best.nc)

k <- 2 # Número ideal de clusters dado pelo método da silhueta
non_hierarchical_kmeans <- kmeans(filtered_sugar_density_df, centers = k, nstart = 25)

# Clusters atribuídos
non_hierarchical_kmeans$cluster

# Tamanhos dos clusters
non_hierarchical_kmeans$size

# Centros dos clusters
non_hierarchical_kmeans$centers

# Visualização dos dados clusterizados
non_hierarchical_cluster_data <- data.frame(filtered_sugar_density_df, Cluster = as.factor(non_hierarchical_kmeans$cluster))
ggplot(non_hierarchical_cluster_data, aes(x = sugar, y = acidity, color = Cluster)) +
  geom_point() +
  theme_minimal()

# Método do cotovelo para encontrar k ideal

# Valores de K variam entre 2 e 10, ou seja, de 2 a 10 clusters
k_values <- 2:10

# Vetor que armazenará a soma dos quadrados intra-clusters
wss_values <- numeric(length(k_values))

# Itera sobre os diferentes valores de K para calcular a soma dos quadrados intra-clusters
for (i in seq_along(k_values)) {
  
  # Seleciona o cluster atual
  k <- k_values[i]
  
  # Calcula o K-means para o número do iterador
  kmeans_result <- kmeans(filtered_sugar_density_df, centers = k, nstart = 25, iter.max = 100)
  
  # Armazena a soma total das distâncias quadradas intra-clusters parao número do iterador
  wss_values[i] <- kmeans_result$tot.withinss
}

# Exibe os K valores
print(k_values)

# Exibe as somas dos quadrados intra-clusters
print(wss_values)

# Gráfico de linha da mudança da soma das distâncias quadradas intra-clusters conforme o número de clusters
elbow_plot <- data.frame(k = k_values, wss = wss_values)
ggplot(elbow_plot, aes(x = k, y = wss)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  theme_minimal() +
  labs(title = "Método do Cotovelo",
       x = "Número de Clusters (k)",
       y = "Soma dos Quadrados Intra-Cluster (WSS)") +
  # diff() calcula as diferenças sucessivas entre os elementos consecutivos do vetor
  # diff(diff()) calcula as diferenças das diferenças, ou seja, a aceleração da redução
  geom_vline(xintercept = which.min(diff(diff(wss_values))) + 1, 
             linetype = "dashed", color = "darkgreen")

# Valores obtidos (Silhueta: 2, CH: 6, Cotovelo: 6)
elbow_k <- 6

# Recalcula K-means com K fixado pelo elbow_k
elbow_kmeans_result <- kmeans(filtered_sugar_density_df, centers = elbow_k, nstart = 25)

# Armazena o resultado para exibiro gráfico
elbow_clustered_data <- data.frame(filtered_sugar_density_df, Cluster = as.factor(elbow_kmeans_result$cluster))

# Cálculo dos limites convexos de cada cluster (determina a geometria dos grupos)
hulls <- elbow_clustered_data %>%
  group_by(Cluster) %>%
  slice(chull(sugar, acidity))

# Gráfico de dispersão de cada amostra em seus respectivos clusters
ggplot(elbow_clustered_data, aes(x = sugar, y = acidity, color = Cluster)) +
  geom_point(size = 3) +
  geom_polygon(data = hulls, aes(fill = Cluster, group = Cluster), alpha = 0.2, color = "black") +
  labs(title = paste("Clusters Identificados com K =", elbow_k),
       x = "Açúcar Residual",
       y = "Acidez Fixa",
       color = "Cluster")

# Exemplo análise de Cluster
# Data Frame
data("mtcars")
View(mtcars)

mtcars_summ <- mtcars[,c(1,2)]

# Z-Score (padronização)
mtcars_summ_stand <- as.data.frame(lapply(mtcars_summ, standardize_z_score))

# Distância Euclidiana 
mtcars_euclidian_dist <-  dist(mtcars_summ_stand)

# Agrupamento hierárquico aglomerativo por single linkage
mtcars_hierarchical_groups <- hclust(mtcars_euclidian_dist, method = "single")
mtcars_hierarchical_groups$labels <- rownames(mtcars)
# Dendograma
plot(mtcars_hierarchical_groups, main = "Dendrograma - mpg & cyl",sub = "", xlab = "", ylab= "Distance")

# Procedimento não hierárquico
# Retirada de outliers
filtered_mtcars_summ <- na.omit(no_outliers_df(mtcars_summ))

#Método da silhueta para encontrar k ideal
mt_cars_silhou <- NbClust(filtered_mtcars_summ, 
                          distance = "euclidean", 
                          min.nc = 2, max.nc = 10, method = "kmeans", 
                          index = "silhouette")
print(mt_cars_silhou$Best.nc)
mtcars_k <- 10 # Número ideal de clusters dado pelo método da silhueta
mtcars_kmeans <- kmeans(filtered_mtcars_summ, centers = mtcars_k, nstart = 25)

# Clusters atribuídos
mtcars_kmeans$cluster
# Tamanhos dos clusters
mtcars_kmeans$size
# Centros dos clusters
mtcars_kmeans$centers

# Visualização dos dados clusterizados
mtcars_mean_data <- data.frame(filtered_mtcars_summ, Cluster = as.factor(mtcars_kmeans$cluster))
ggplot(mtcars_mean_data, aes(x = cyl, y = mpg, color = Cluster)) + geom_point() + theme_minimal()



# Verificando os boxplots após padronização
wines_long_z_score <- pivot_longer(wines_standardized, cols = everything(), 
                                   names_to = "Variable", values_to = "Value")

# Boxplot das variáveis normalizadas
ggplot(wines_long_z_score, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Boxplots Padronizados (Z-score)",
       x = "Variáveis",
       y = "Valores")

# Boxplot da varíavel residual.sugar padronizaada para a amostra de 500 elementos
ggplot(wines_sample, aes(x = "", y = residual.sugar)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(fill = "red", alpha = 0.5) +
  labs(title = "Boxplot para açúcar residual", y = "Açúcar residual")

# Boxplot da varíavel fixed.acidity padronizaada para a amostra de 500 elementos
ggplot(wines_sample, aes(x = "", y = fixed.acidity)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  labs(title = "Boxplot para acidez fixa", y = "Acidez fixa")

# Histograma com uma curva de densidade para a variável residual.sugar para a amostra de 500 elementos
ggplot(wines_sample, aes(x = residual.sugar)) +
  geom_histogram(aes(y = after_stat(density)), colour = 1, fill = "#FFFFFF", bins = 100) +
  geom_density(lwd = 1.2, linetype = 2, colour = "red", fill = "red", alpha = 0.25) +
  labs(title = "Histograma com curva de densidade para açúcar residual", x = "Açúcar residual", y = "Densidade")

# Histograma com uma curva de densidade para a variável fixed.acidity para a amostra de 500 elementos
ggplot(wines_sample, aes(x = fixed.acidity)) +
  geom_histogram(aes(y = after_stat(density)), colour = 1, fill = "#FFFFFF", bins = 100) +
  geom_density(lwd = 1.2, linetype = 2, colour = "blue", fill = "blue", alpha = 0.25) +
  labs(title = "Histograma com curva de densidade para acidez fixa", x = "Acidez fixa", y = "Densidade")

# Transforma uma coluna numérica em uma coluna fator
wines_standardized$is.red <- as.factor(wines_standardized$is.red)

# Histograma com uma curva de densidade para a variável density
ggplot(wines_standardized, aes(x = density)) +
  geom_histogram(aes(y = after_stat(density)), colour = 1, fill = "#FFFFFF", bins = 200) +
  geom_density(lwd = 1.2, linetype = 2, colour = "#FF69B4", fill = "#FF69B4", alpha = 0.25)

# Histograma com uma curva de densidade para a variável residual.sugar
ggplot(wines_standardized, aes(x = residual.sugar)) +
  geom_histogram(aes(y = after_stat(density)), colour = 1, fill = "#FFFFFF", bins = 500) +
  geom_density(lwd = 1.2, linetype = 2, colour = "#FF69B4", fill = "#FF69B4", alpha = 0.25)

# Histograma com uma curva de densidade para a variável fixed.acidity
ggplot(wines_standardized, aes(x = fixed.acidity)) +
  geom_histogram(aes(y = after_stat(density)), colour = 1, fill = "#FFFFFF", bins = 500) +
  geom_density(lwd = 1.2, linetype = 2, colour = "#FF69B4", fill = "#FF69B4", alpha = 0.25)

