# Adiciona uma coluna indicando a cor do vinho de cada dataset
white_wines$colour <- "white"
red_wines$colour <- "red"

# Combina os datasets para um único
wines <- rbind(white_wines, red_wines)

# Retira possíveis observações com campos NA
wines <- na.omit(wines)

# Sortea 5000 observações para compor uma "amostra da amostra"
wines_sample <- wines[sample(nrow(wines), size = 5000, replace = FALSE), ]