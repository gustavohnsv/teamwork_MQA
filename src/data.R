# Filtragem para os registros que nao contenham NA em algum de seus campos
sorted_filtered_data <-na.omit(brute_data[order(brute_data$Year.of.arrival.at.port.of.disembarkation), ])

# FIltragem para os registros que contenham as porcentagens maiores que 0
sorted_filtered_positive_data <- subset(sorted_filtered_data, 
                                        sorted_filtered_data$Percent.men > 0 & 
                                          sorted_filtered_data$Percent.women > 0 & 
                                          sorted_filtered_data$Percent.children > 0
)

# Substituindo todas as aparições de "," por "." para valores com ponto flutuante
sorted_filtered_positive_data$Percent.men <- gsub(",", ".", sorted_filtered_positive_data$Percent.men)
sorted_filtered_positive_data$Percent.women <- gsub(",", ".", sorted_filtered_positive_data$Percent.women)
sorted_filtered_positive_data$Percent.children <- gsub(",", ".", sorted_filtered_positive_data$Percent.children)

# Bibliteca para exibição de gráficos
library(ggplot2)

# Biblioteca para tratamento de dados
library(reshape)

# Biblitoeca para exibição de gráficos de pizza
library(plotrix)

# Criando um quadro de dados para as porcentagens de homens, mulheres e criança com base no ano
percents_df <- data.frame(
  total = sorted_filtered_positive_data$Total.embarked,
  p_m = sorted_filtered_positive_data$Percent.men,
  p_w = sorted_filtered_positive_data$Percent.women,
  p_c = sorted_filtered_positive_data$Percent.children
)

# Unindo as porcentagens de homens, mulheres e crianças em uma coluna categorizada 
melt_percent_df <- melt(percents_df, id.vars = "total", measure.vars = c(2, 3, 4))

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

total <- sorted_filtered_positive_data$Total.embarked
male <- floor(total * as.numeric(sorted_filtered_positive_data$Percent.men))
female <- floor(total * as.numeric(sorted_filtered_positive_data$Percent.women))
children <- floor(total * as.numeric(sorted_filtered_positive_data$Percent.children))
error <- total - (male + female + children)

percents_numbers <- data.frame(
  male = male,
  female = female,
  children = children
)

rm(male, female, children)