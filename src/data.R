# Filtragem para os registros que nao contenham NA em algum de seus campos
sorted_filtered_data <-na.omit(brute_data[order(brute_data$Year.of.arrival.at.port.of.disembarkation), ])

# FIltragem para os registros que contenham as porcentagens maiores que 0
sorted_filtered_positive_data <- subset(sorted_filtered_data, 
                                        sorted_filtered_data$Percent.men > 0 & 
                                          sorted_filtered_data$Percent.women > 0 & 
                                          sorted_filtered_data$Percent.children > 0
)

# Removendo os dados filtrados intermediários
rm(sorted_filtered_data)

# Substituindo todas as aparições de "," por "." para valores com ponto flutuante
sorted_filtered_positive_data$Percent.men <- as.numeric(gsub(",", ".", sorted_filtered_positive_data$Percent.men))
sorted_filtered_positive_data$Percent.women <- as.numeric(gsub(",", ".", sorted_filtered_positive_data$Percent.women))
sorted_filtered_positive_data$Percent.children <- as.numeric(gsub(",", ".", sorted_filtered_positive_data$Percent.children))

# Garantindo que o ano seja um valor numérico
sorted_filtered_positive_data$Year.of.arrival.at.port.of.disembarkation <- as.numeric(sorted_filtered_positive_data$Year.of.arrival.at.port.of.disembarkation)

# Bibliteca para exibição de gráficos
library(ggplot2)

# Biblioteca para tratamento de dados
library(reshape)

# Biblitoeca para exibição de gráficos de pizza
library(plotrix)

# Biblioteca para funções especiais
library(dplyr)

# Criando um quadro de dados para as porcentagens de homens, mulheres e criança com base no ano
percents_df <- data.frame(
  year = sorted_filtered_positive_data$Year.of.arrival.at.port.of.disembarkation,
  p_m = sorted_filtered_positive_data$Percent.men,
  p_w = sorted_filtered_positive_data$Percent.women,
  p_c = sorted_filtered_positive_data$Percent.children
)

# Unindo as porcentagens de homens, mulheres e crianças em uma coluna categorizada 
melt_percent_df <- melt(percents_df, id.vars = "year", measure.vars = c(2, 3, 4))

# Convertendo os valores das porcentagens de 'character' para 'numeric'
melt_percent_df$value <- as.numeric(as.character(melt_percent_df$value))

# Função que ordena o quadro de dados em ordem decrescente e exibe apenas os num+1 primeiros elementos
sortElementsInDescendingOrder <- function(data, num) {
  data <- sort(data, decreasing = TRUE)
  data <- c(head(data, num), Outros = (sum(data)) - sum(head(data, num)))
  return(data)
}

# Filtrando pelos principais locais onde os embarques começavam e ordenando eles
voyage_began <- table(sorted_filtered_positive_data$Voyage.itinerary.imputed.port.where.began..ptdepimp..place)
voyage_began_head <- sortElementsInDescendingOrder(voyage_began, 12)

# Filtrando pelos principais locais onde os embarques terminavem e ordenando eles
voyage_disembarkation <- table(sorted_filtered_positive_data$Voyage.itinerary.imputed.principal.port.of.slave.disembarkation..mjslptimp..place)
voyage_disembarkation_head <- sortElementsInDescendingOrder(voyage_disembarkation, 12)

# Criando variáveis temporárias para armazenar em dois ´data frames´ abaixo
total <- sorted_filtered_positive_data$Total.embarked
male <- floor(total * as.numeric(sorted_filtered_positive_data$Percent.men))
female <- floor(total * as.numeric(sorted_filtered_positive_data$Percent.women))
children <- floor(total * as.numeric(sorted_filtered_positive_data$Percent.children))
error <- total - (male + female + children)

# Corrigindo as linhas onde o erro é negativo, ou seja, o erro foi causado por contagem incorreta dos dados
for (i in 1:length(error)) {
  if (is.na(error[i])) next
  else if (error[i] < 0) {
    print(error[i])
    total[i] <- total[i] + error[i]
    male <- floor(total * as.numeric(sorted_filtered_positive_data$Percent.men))
    female <- floor(total * as.numeric(sorted_filtered_positive_data$Percent.women))
    children <- floor(total * as.numeric(sorted_filtered_positive_data$Percent.children))
    error[i] <- NA
  }
}

# Armazendo o total de escravos embarcados separados por gênero
percents_numbers <- data.frame(
  male = male,
  female = female,
  children = children
)

# Armazenando o total embarcado, seguido de uma parcela de erro por arredondamento/contagem incorreta
percent_numbers_err <- data.frame(
  total = total,
  error = error
)

# Removendo dados temporários que não estão sendo mais utilizados
rm(total, male, female, children, error)

# Agrupando os anos em intervalos de 100 anos, e obtendo as informações mais relevantes
year_by_group <- sorted_filtered_positive_data %>%
  mutate(Century = cut(Year.of.arrival.at.port.of.disembarkation, 
                   breaks = seq(min(Year.of.arrival.at.port.of.disembarkation, na.rm = TRUE), 
                                max(Year.of.arrival.at.port.of.disembarkation, na.rm = TRUE), 
                                by = 100),
                   right = FALSE)) %>%
  group_by(Century) %>%
  summarise(
    Sum_embarked = sum(Total.embarked, na.rm = TRUE),
    Sum_disembarked = sum(Total.disembarked, na.rm = TRUE),
    Sum_deaths = sum(Captive.deaths.during.crossing, na.rm = TRUE),
    Mean_duration = mean(Duration.of.captives..crossing..in.days., na.rm = TRUE)
  )

#
# Exibição de total de mortos com base em categorias de século
#

# Função para determinar o século
define_century <- function(year){
  if (year >= 1600 & year < 1700) {
    return("17th_century")
  } else if (year >= 1700 & year < 1800) {
    return("18th_century")
  } else if (year >= 1800 & year < 1900) {
    return("19th_century")
  } else {
    return(NA)
  }
  
}

# Aplicar a função ao dataframe para criar uma nova coluna 'Century'
sorted_filtered_positive_data$Century <- sapply(sorted_filtered_positive_data$Year.of.arrival.at.port.of.disembarkation, define_century)

# Filtrar os dados por século
data_17th_century <- subset(sorted_filtered_positive_data, Century == "17th_century")
data_18th_century <- subset(sorted_filtered_positive_data, Century == "18th_century")
data_19th_century <- subset(sorted_filtered_positive_data, Century == "19th_century")

# Serve para indicar o tamanho máximo da tabela
max_length <- max(nrow(data_17th_century), nrow(data_18th_century), nrow(data_19th_century))

# Criar data frames para cada século, preenchendo com NA onde necessário
df_17th_century <- data.frame(
  "17th_century" = c(data_17th_century$Captive.deaths.during.crossing, 
                     rep(NA, max_length - nrow(data_17th_century)))
)
df_18th_century <- data.frame(
  "18th_century" = c(data_18th_century$Captive.deaths.during.crossing, 
                     rep(NA, max_length - nrow(data_18th_century)))
)
df_19th_century <- data.frame(
  "19th_century" = c(data_19th_century$Captive.deaths.during.crossing, 
                     rep(NA, max_length - nrow(data_19th_century)))
)

# Combinar os data frames em um único data frame
deaths_by_century <- cbind(df_17th_century, df_18th_century, df_19th_century)