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
ggplot(melt_percent_df, aes(x = total, y = value, color = variable, group = variable)) +
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
