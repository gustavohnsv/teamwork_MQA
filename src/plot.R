# Histograma com uma curva de densidade para a variável pH
ggplot(wines, aes(x = pH)) +
  geom_histogram(aes(y = after_stat(density)), colour = 1, fill = "#FFFFFF", bins = 200) +
  geom_density(lwd = 1.2, linetype = 2, colour = "#FF69B4", fill = "#FF69B4", alpha = 0.25)

# Gráfico de dispersão com uma linha de correlação para as variáveis pH e ácido cítrico
ggplot(wines, aes(x = volatile.acidity, y = chlorides)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Mapa de calor para cada par de colunas das colunas de vinho
wines_cor_matrix <- cor(wines_numeric, use = "complete.obs")
pheatmap(wines_cor_matrix,
         display_numbers = TRUE,
         number_color = "#000000",
         fontsize_number = 8)
rm(wines_cor_matrix)

# Gráficos de dispersão comparando cada variável com um densidade (precisa de atenção)
png("linearmultipleregression1", width = 1000, height = 1000)
avPlots(lm(formula = density ~ residual.sugar+alcohol+fixed.acidity+chlorides+is.red , data = wines))
dev.off()