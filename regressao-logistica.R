# Análise das frequências de categorias
table(wines_sample$is.red) # Precisa de proporções parecidas

# Checagem das categorias de referência 
levels(wines_sample$is.red)

# Checagem dos pressupostos

## 1. Variável independente dicotômica (categorias mutuamente exclusivas)
## 2. Independência das observações (sem medidas repetidas)

# Obs: rever variáveis independentes
model <- glm(wines_sample$is.red ~ wines_sample$alcohol + wines_sample$residual.sugar + wines_sample$citric.acid,
             family = binomial(link = 'logit'), data = wines_sample)
#item 6A

summary(model)
## 3. Ausência de outliers/ pontos de alavancagem

summary(stdres(model)) # Entre -3 e +3 

# Representação da equação final
modelo <- glm(wines_sample$is.red ~ wines_sample$alcohol + wines_sample$residual.sugar + wines_sample$citric.acid, family = binomial)

coeficientes <- coef(modelo)
intercepto <- coeficientes[1]
slope <- coeficientes[2]
slope2 <- coeficientes[3]
slope3 <- coeficientes[4]
# Exibindo a equação da regressão logística
cat("Log-odds do evento (log(p / (1 - p))): =", intercepto, "+", slope, "* X_1 +", slope2, "* X_2 +", slope3,"* X_3")

## 4. Ausência de multicolinearidade
#item 6H parte 3
### Variáveis independentes não podem ter r> 0.9
cor(wines_sample$alcohol, wines_sample$residual.sugar,
    method = c("pearson")) 
cor(wines_sample$alcohol, wines_sample$citric.acid,
    method = c("pearson"))
cor(wines_sample$residual.sugar, wines_sample$citric.acid,
    method = c("pearson"))

# From regclass package
#vif(model) # não pode ter VIF>10

# Análise do modelo

## Overall effects
summary(model)

## Obtenção das razões  de chance com IC 95% (log-likelihood)
# item 6D
exp(cbind(OR = coef(model), confint(model)))

# Pseudo-R2
# item 6E
# From DescTools package
#pseudoR2(model, which ="Nagelkerke")

## Teste de autocorrelação (Durbin-Watson)
# item 6H parte 2
dwtest(model)

# log-likelihood do modelo
#item 6E
print(logLik(model))
