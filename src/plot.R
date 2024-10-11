# Histograma com uma curva de densidade para a variável pH
ggplot(wines, aes(x = pH)) +
  geom_histogram(aes(y = after_stat(density)), colour = 1, fill = "#FFFFFF", bins = 200) +
  geom_density(lwd = 1.2, linetype = 2, colour = "#FF69B4", fill = "#FF69B4", alpha = 0.25)

# Gráfico de dispersão com uma linha de correlação para as variáveis pH e ácido cítrico
ggplot(wines, aes(x = volatile.acidity, y = chlorides)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Mapa de calor para cada par de colunas das colunas de vinho
wines_cor_matrix <- cor(wines_sample, use = "complete.obs")
pheatmap(wines_cor_matrix,
         display_numbers = TRUE,
         number_color = "#000000",
         fontsize_number = 8)
rm(wines_cor_matrix)

# Gráficos de dispersão comparando cada variável com um densidade (precisa de atenção)
avPlots(lm(formula = density ~ residual.sugar + fixed.acidity + alcohol + chlorides + is.red, data = wines_sample))

# Aplicando a padronização a cada coluna do dataframe
wines_standardized <- as.data.frame(lapply(wines_sample, standardize_z_score))

# Verificando os boxplots após padronização
wines_long_z_score <- pivot_longer(wines_standardized, cols = everything(), 
                                   names_to = "Variable", values_to = "Value")

ggplot(wines_long_z_score, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Boxplots Padronizados (Z-score)",
       x = "Variáveis",
       y = "Valores")