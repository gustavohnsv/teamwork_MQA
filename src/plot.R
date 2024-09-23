# Gráfico de linha do número de escravos embarcados e desembarcados conforme o ano
ggplot(sorted_filtered_positive_data, aes(x = Year.of.arrival.at.port.of.disembarkation)) +
  geom_line(aes(y = Total.embarked, color = "blue")) +
  geom_line(aes(y = Total.disembarked, color = "red")) +
  labs(x = "Ano de chegada aos portos de desembarcação") +
  labs(y = "Embarcados x Desembarcados") + 
  labs(color = "Legenda") +
  labs(title = "Quantidade de escravos que foram embarcados e desembarcados durante as travessias")

# Histograma da quantidade de escravos mortos durante travessia
ggplot(sorted_filtered_positive_data, aes(x = Captive.deaths.during.crossing)) +
  geom_histogram(fill = "lightblue", color = "lightblue", bins = 1000) +
  labs(x = "Mortes de escravos durante a travessia", y = "Quantidade") +
  labs(title = "Quantidade de escravos mortos durante as travessias")

# Gráfico de linhas suaves da proporção do gênero dos escravos conforme o ano
ggplot(melt_percent_df, aes(x = year, y = value, color = variable, group = variable)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  scale_color_discrete(labels = c(
    "p_m" = "Homem",
    "p_w" = "Mulher",
    "p_c" = "Criança"
  )) +
  labs(x = "Ano de chegada aos portos de desembarcação") +
  labs(y = "Porcentagem") +
  labs(color = "Grupos") +
  labs(title = "Porcentagem de homens, mulheres e crianças durante as travessias")

# Gráfico de pizza para os principais locais onde os embarques começavam
pie3D(voyage_began_head,
      border = "black",
      height = 0.2,
      radius = 1,
      theta = 1.1,
      shade = 0.5,
      labels = names(voyage_began_head),
      labelcex = 0.6,
      main = "Principais locais de embarque")

# Gráfico de pizza para os principais locais onde os embarques terminavam
pie3D(voyage_disembarkation_head,
      border = "black",
      height = 0.2,
      radius = 1,
      theta = 1.1,
      shade = 0.5,
      labels = names(voyage_disembarkation_head),
      labelcex = 0.6,
      main = "Principais locais de desembarque")

# Boxplot do número de mortes durante travessia por século
boxplot(sorted_filtered_positive_data$Captive.deaths.during.crossing
        ~ sorted_filtered_positive_data$Century,
        col = c("#FF0000", "#00FF00", "#0000FF"),
        names = c("Século 17", "Século 18", "Século 19"),
        main = "Quantidade de mortes durante as viagens por século",
        xlab = "Séculos",
        ylab = "Quantidade de mortes durante as viagens")

# Histograma do número de mortes com gráfico de densidade
ggplot(sorted_filtered_positive_data, aes(x = Captive.deaths.during.crossing)) +
  geom_histogram(aes(y = after_stat(density)), colour = 1, fill = "#FFFFFF", bins = 100) +
  geom_density(lwd = 1.2, linetype = 2, colour = "#FF0000", fill = "#FF0000", alpha = 0.25) +
  labs(x = "Quantidade de mortes") +
  labs(y = "Densidade") +
  labs(title = "Distrivuição da quantidade de mortes durante as viagens")

# Histograma da quantidade de mortes durante a travessia por século, mas separado em grupos
ggplot(sorted_filtered_positive_data, aes(x = Captive.deaths.during.crossing, fill = Century, colour = Century)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 50) +
  labs(x = "Quantidade de mortes durante as viagens") +
  labs(y = "Quantidade de ocorrências") +
  labs(title = "Quantidade de ocorrência de mortes durante as viagens") +
  scale_color_discrete(labels = c("Século 17", "Século 18", "Século 19")) +
  scale_fill_discrete(labels = c("Século 17", "Século 18", "Século 19"))

# Histograma da porcentagem de homens por viagem com gráfico de densidade
ggplot(sorted_filtered_positive_data, aes(x = Percent.men)) +
  geom_histogram(aes(y = after_stat(density)), colour = 1, fill = "#FFFFFF", bins = 100) +
  geom_density(lwd = 1.2, linetype = 2, colour = "#1E90FF", fill = "#1E90FF", alpha = 0.25) +
  labs(x = "Quantidade de homens por viagem") +
  labs(y = "Densidade") +
  labs(title = "Distribuição de homens durante as viagens")

# Histograma da porcentagem de mulheres por viagem com gráfico de densidade
ggplot(sorted_filtered_positive_data, aes(x = Percent.women)) +
  geom_histogram(aes(y = after_stat(density)), colour = 1, fill = "#FFFFFF", bins = 100) +
  geom_density(lwd = 1.2, linetype = 2, colour = "#FF69B4", fill = "#FF69B4", alpha = 0.25) +
  labs(x = "Quantidade de mulheres por viagem") +
  labs(y = "Densidade") +
  labs(title = "Distribuição de mulheres durante as viagens")

# Histograma da porcentagem de crianças por viagem com gráfico de densidade
ggplot(sorted_filtered_positive_data, aes(x = Percent.children)) +
  geom_histogram(aes(y = after_stat(density)), colour = 1, fill = "#FFFFFF", bins = 100) +
  geom_density(lwd = 1.2, linetype = 2, colour = "#32CD32", fill = "#32CD32", alpha = 0.25) +
  labs(x = "Quantidade de crianças por viagem") +
  labs(y = "Densidade") +
  labs(title = "Distribuição de crianças durante as viagens")

# Gráfico QQ (Quantiles-Quantiles) para análise da "curva de distribuição" da varáivel de mortes durante a viagem
qqnorm(sorted_filtered_positive_data$Captive.deaths.during.crossing, main = "Gráfico QQ")
qqline(sorted_filtered_positive_data$Captive.deaths.during.crossing, col = "#FF0000")
  