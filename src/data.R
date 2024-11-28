# Combinar os datasets e remover valores ausentes
wines <- rbind(white_wines, red_wines)
wines <- na.omit(wines)

# Padronizar as variáveis numéricas (Z-score)
wines_standardized <- as.data.frame(lapply(wines[, sapply(wines, is.numeric)], standardize_z_score))

# Criar uma matriz de correlação
library(pheatmap)
corr_matrix <- cor(wines_standardized, use = "complete.obs")
pheatmap(corr_matrix, display_numbers = TRUE, fontsize_number = 8)

library(psych)

# Realizar a análise fatorial (definindo 3 fatores como exemplo)
fa_result <- fa(r = corr_matrix, nfactors = 3, rotate = "varimax")

# Exibir os resultados da análise fatorial
print(fa_result)

# Scree Plot para avaliar o número adequado de fatores
scree(corr_matrix)

# Plotar os loadings fatoriais
fa.diagram(fa_result)

# Adicionar os scores fatoriais aos dados padronizados
wines_standardized$Factor1 <- fa_result$scores[,1]
wines_standardized$Factor2 <- fa_result$scores[,2]
wines_standardized$Factor3 <- fa_result$scores[,3]

# Determina quantos fatores serão usados como base na regra do autovalor maior que 1
num_factors <- calculate_num_factors(wines_standardized)
