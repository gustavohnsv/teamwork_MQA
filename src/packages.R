# Biblioteca com algumas funções úteis de rstudioapi
if (!requireNamespace("rstudioapi", quietly = TRUE)) {
  install.packages("rstudioapi")
}
library(rstudioapi)

# Biblioteca com algumas funções úteis de rlang
if (!requireNamespace("rlang", quietly = TRUE)) {
  install.packages("rlang")
}
library(rlang)

# Biblioteca para exibição de gráficos
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}
library(car)

# Biblioteca para exibição de mapa de calor (Correlação)
if (!requireNamespace("pheatmap", quietly = TRUE)) {
  install.packages("pheatmap")
}
library(pheatmap)

# Biblioteca para tratamento de dados, permite a manipulação de data frames e matrizes
if (!requireNamespace("reshape", quietly = TRUE)) {
  install.packages("reshape")
}
library(reshape)

# Biblioteca para funções de manipulação e transformação de dados
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)


# Biblioteca utilizada para padronização do dataset
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
library(tidyr)

#Biblioteca para a visualização de clusters intuitivamente (k-means e dendogramas)
if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
library(factoextra)

#Biblioteca para o carregamento e cálculo de infos relacionadas a clusters
if (!requireNamespace("cluster", quietly = TRUE)) install.packages("cluster")
library(cluster)

if (!requireNamespace("psych", quietly = TRUE)) {
  install.packages("psych")
}
library(psych)

if (!requireNamespace("GPArotation", quietly = TRUE)) {
  install.packages("GPArotation")
}
library(GPArotation)

if (!requireNamespace("psych", quietly = TRUE)) install.packages("psych")