# Adiciona uma coluna indicando a cor do vinho de cada dataset
white_wines$colour <- "white"
red_wines$colour <- "red"

# Combina os datasets para um único
wines <- rbind(white_wines, red_wines)

# Retira possíveis observações com campos NA
wines <- na.omit(wines)

# Renomeia a coluna 'colour' para 'is.red'
colnames(wines)[13] <- "is.red"

# Substitui a cor por números, sendo 1 para tinto e 0 para branco
wines <- wines %>%
  mutate(is.red = ifelse(is.red == "red", 1, 0))

# Converte o valor númerico para um fator
wines$is.red <- as.factor(wines$is.red)

# Sorteia 5000 observações para compor uma "amostra da amostra"
wines_sample <- wines[sample(nrow(wines), size = 5000, replace = FALSE), ]

# Cria um subset apenas com as colunas que são númericas
wines_numeric <- wines[, sapply(wines, is.numeric)]

# Obtem o número de colunas de "wines_numeric"
wines_numeric_cols <- ncol(wines_numeric)

# Criação de Dataframe para mostrar coeficiente de correlação de cada variável
corr_df <- data.frame (
  variable_1 = character(),
  corr_value = numeric(),
  variable_2 = character(),
  corr_value_module = numeric()
)

# Exibe o coeficiente de correlação de cada variável
for (i in 1:wines_numeric_cols) {
  for (j in 1:wines_numeric_cols) {

    corr_test_result <- test_Corr(wines[, i], wines[, j])
    
    # Adicionar ao dataframe que mostra coeficiente de correlação por variável

    if(corr_test_result != 1){
      corr_df <- rbind(corr_df, data.frame(variable_1 = colnames(wines[i]), corr_value = corr_test_result, variable_2 = colnames(wines[j]), corr_value_module = abs(corr_test_result)))
    }
  }
}

# Remove as variáveis temporárias para o loop
rm(i, j, wines_numeric_cols, corr_test_result)

# Preparação das variáveis para os modelos de regressão de Ridge e Lasso
x <- model.matrix(is.red ~ ., data = wines_sample)[, -1]
y <- wines_sample$is.red

# Modelos de regressão de Ridge e de Lasso
ridge_model <- cv.glmnet(x, y, family = "binomial", alpha = 0) # Penaliza o modelo de maneira mais suave
lasso_model <- cv.glmnet(x, y, family = "binomial", alpha = 1) # Penaliza o modelo drasticamente

# Modelo de regressão logística com todas as variáveis do modelo
logistic_model <- glm(is.red ~ ., data = wines_standardized, family = binomial(link = "logit"))

# Modelo de regressão logística após a etapa de Stepwise, que mantém as variáveis mais significativas
stepwise_logistic_model <- step(logistic_model, direction = "both", k = log(nrow(wines_sample)), trace = TRUE)

# Razão de chances para o modelo que passou pelo processo de Stepwise
OR <- odds.ratio(stepwise_logistic_model)

# Visualização dos gráficos de probabilidade para cada uma das variáveis para o modelo que passou pelo processo de Stepwise
marginalModelPlots(stepwise_logistic_model)

#Exemplo de uso da regressão logística
  #Packages 
  install.packages('readr')
  install.packages('stats')
  require(stats)
  require(readr)

  # Data
  data <- read.csv("http://www.karlin.mff.cuni.cz/~pesta/prednasky/NMFM404/Data/binary.csv")
  data$rank <- factor(data$rank)

  # Model
  model <- glm(admit ~ gre + gpa + rank, data = data, family = binomial(link = "logit"))
  model

  # Odds Ratio
  exp(cbind(OR = coef(model)))

  # Probability Prediction
  pred =data.frame(gre = 500, gpa = 3.5, rank = factor(2))
  pred$prob = predict(model, newdata = pred, type = "response")
  pred

  pred =data.frame(gre = 500, gpa = 3.5, rank = factor(4))
  pred$prob = predict(model, newdata = pred, type = "response")
  pred
