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

# Padronizando os dados (Z-score)
standardize_z_score <- function(x) {
  if (is.numeric(x)) {
    return((x - mean(x)) / sd(x))
  } else {
    return(x)
  }
}

# Subconjunto de 500 amostras e padronização
set.seed(123)  # Para garantir reprodutibilidade
wines_sample <- wines[sample(nrow(wines), size = 500, replace = FALSE), ]
wines_sample_numeric <- as.data.frame(lapply(wines_sample, standardize_z_score))

# Criando subsets para vinhos tintos e brancos
wines_sample_red <- wines_red[sample(nrow(wines_red), size = 500, replace = FALSE), ]
wines_sample_numeric_red <- as.data.frame(lapply(wines_sample_red, standardize_z_score))

wines_sample_white <- wines_white[sample(nrow(wines_white), size = 500, replace = FALSE), ]
wines_sample_numeric_white <- as.data.frame(lapply(wines_sample_white, standardize_z_score))

# KMO (Kaiser-Meyer-Olkin) e Teste de Bartlett
kmo_method <- KMO(cor(wines_sample_numeric))
barlett_test_all <- cortest.bartlett(cor(wines_sample_numeric), n = nrow(wines_sample_numeric))

print("KMO para todos os vinhos: ")
print(kmo_method)
print("Teste de Bartlett para todos os vinhos: ")
print(barlett_test_all)

# Determinando número de fatores com base no critério de Kaiser
kaiser_eigenvalues_all <- eigen(cor(wines_sample_numeric))$values
factors_kaiser_all <- sum(kaiser_eigenvalues_all > 1)

# Reavaliando o número de fatores baseado no Scree Plot
factors_kaiser_all <- 3  # Escolher manualmente após revisão

# Análise Fatorial Ajustada
factorial_analysis_all <- fa(
  r = cor(wines_sample_numeric),
  nfactors = factors_kaiser_all,
  rotate = "oblimin",  # Tente também "varimax"
  fm = "ml"
)

# Escores fatoriais ajustados
factor_scores <- factor.scores(wines_sample_numeric, factorial_analysis_all)
print(factor_scores$scores)

# Comunalidades
print("Comunidades das variáveis:")
print(factorial_analysis_all$communality)

# KMO Individual
print("KMO Individual (MSA):")
print(kmo_method$MSAi)
