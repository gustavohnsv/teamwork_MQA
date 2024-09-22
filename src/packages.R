# Biblioteca com algumas funções úteis do RStudio
if (!requireNamespace("rstudioapi", quietly = TRUE)) {
  install.packages("rstudioapi")
}
library(rstudioapi)

# Biblioteca para exibição de gráficos
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Biblioteca para tratamento de dados, permite a manipulação de data frames e matrizes
if (!requireNamespace("reshape", quietly = TRUE)) {
  install.packages("reshape")
}
library(reshape)

# Biblioteca para exibição de gráficos de pizza
if (!requireNamespace("plotrix", quietly = TRUE)) {
  install.packages("plotrix")
}
library(plotrix)

# Biblioteca para funções de manipulação e transformação de dados
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Biblioteca para manipulação de matrizes esparsas
if (!requireNamespace("SparseM", quietly = TRUE)) {
  install.packages("SparseM")
}
library(SparseM)

# Biblioteca para regressão quantílica
if (!requireNamespace("quantreg", quietly = TRUE)) {
  install.packages("quantreg")
}
library(quantreg)

# Biblioteca que contém conjuntos de dados utilizados em exemplos da biblioteca 'car'
if (!requireNamespace("carData", quietly = TRUE)) {
  install.packages("carData")
}
library(carData)

# Biblioteca para análise de regressão, ANOVA, e outros métodos estatísticos
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}
library(car)
