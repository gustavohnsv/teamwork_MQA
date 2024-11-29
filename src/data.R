# Padronizar as variáveis numéricas (Z-score)
wines_standardized <- as.data.frame(lapply(wines[, sapply(wines, is.numeric)], scale))

# Criar uma matriz de correlação
corr_matrix <- cor(wines_standardized, use = "complete.obs")

# Realizar a análise fatorial com método tenBerge
library(psych)
fa_result <- fa(r = corr_matrix, nfactors = 3, rotate = "varimax", scores = "tenBerge")

# Exibir os resultados da análise fatorial
print(fa_result)

# Scree Plot para avaliar o número adequado de fatores
scree(corr_matrix)

# Plotar os loadings fatoriais
fa.diagram(fa_result)

# Adicionar os scores fatoriais ao dataframe padronizado
wines_standardized$Factor1 <- fa_result$scores[,1]
wines_standardized$Factor2 <- fa_result$scores[,2]
wines_standardized$Factor3 <- fa_result$scores[,3]

# Verificar comunalidades
print("Comunalidades:")
print(fa_result$communality)

# Determinar quantos fatores serão usados (autovalores > 1)
eigenvalues <- eigen(corr_matrix)$values
num_factors <- sum(eigenvalues > 1)
print(paste("Número de fatores sugeridos:", num_factors))
