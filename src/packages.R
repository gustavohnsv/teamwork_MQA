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

# Biblioteca para exibição de gráficos de pizza
#if (!requireNamespace("plotrix", quietly = TRUE)) {
#  install.packages("plotrix")
#}
#library(plotrix)

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
#if (!requireNamespace("quantreg", quietly = TRUE)) {
#  install.packages("quantreg")
#}
#library(quantreg)

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

# Biblioteca que contém uma variedade de métodos simples de avaliação de estoques pesqueiros.
#if (!requireNamespace("FSA", quietly = TRUE)) {
#  install.packages("FSA")
#}

#library(FSA)

# Biblioteca utilizada para aplicação do teste de normalidade de Anderson-Darling
#if (!requireNamespace("nortest", quietly = TRUE)) {
#  install.packages("nortest")
#}
#library(nortest)

# Biblioteca utilizada para formatação das tabelas de frequência
#if (!requireNamespace("formattable", quietly = TRUE)) {
#  install.packages("formattable")
#}
#library(formattable)

# Biblioteca utilizada para testes de modelos de regressão linear
#if (!requireNamespace("lmtest", quietly = TRUE)) {
#  install.packages("lmtest")
#}
#library(lmtest)

# Biblioteca utilizada para padronização do dataset
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
library(tidyr)

# Biblioteca utilizada para obter razão de chances para modelos de regressão logística
#if (!requireNamespace("questionr", quietly = TRUE)) {
#  install.packages("questionr")
#}
#library(questionr)

# Biblioteca utilizada para regularização de modelos
#if (!requireNamespace("glmnet", quietly = TRUE)) {
#  install.packages("glmnet")
#}
#library(glmnet)

# Biblioteca para métodos cotovelo e silhueta para escolha de número de clusters na análise não hierárquica
#if (!requireNamespace("NbClust", quietly = TRUE)) {
#  install.packages("NbClust")
#}
#library(NbClust)

# Biblioteca para criar gráficos avançados (não funciona ainda)
#if (!requireNamespace("ggalt", quietly = TRUE)) {
#  install.packages("ggalt")
#}
#library(ggalt)

# Biblioteca para cálculo do coeficiente KMO
if (!requireNamespace("psych", quietly = TRUE)) {
  install.packages("psych")
}
library(psych)

# Biblioteca para rotações de vetores
if (!requireNamespace("stats", quietly = TRUE)) {
  install.packages("stats")
}
library(stats)

# Biblioteca para visualização de matrizes de correlação
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}
library(corrplot)