# Análise das frequências de categorias
table(wines_sample$is.red) # Precisa de proporções parecidas

# Checagem das categorias de referência 
levels(wines_sample$is.red)

# Checagem dos pressupostos

## 1. Variável independente dicotômica (categorias mutuamente exclusivas)
## 2. Independência das observações (sem medidas repetidas)

# Obs: rever variáveis independentes
model <- glm(wines_sample$is.red ~ wines_sample$alcohol + wines_sample$residual.sugar,
             family = binomial(link = 'logit'), data = wines_sample)

## 3. Ausência de outliers/ pontos de alavancagem

summary(stdres(model)) # Entre -3 e +3 

## 4. Ausência de multicolinearidade

### Variáveis independentes não podem ter r> 0.9
cor(wines_sample$alcohol, wines_sample$residual.sugar,
    method = c("pearson")) # não pode ter r>0.9
#pairs.panels(wines_sample) # não pode ter r>0.9
#vif(model) # não pode ter VIF>10

# Análise do modelo

## Overall effects
summary(model)

## Obtenção das razões  de chance com IC 95% (log-likelihood)

exp(cbind(OR = coef(model), confint(model)))

# Pseudo-R2

pR2(model)["Nagelkerke"]