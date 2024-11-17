# Realiza o teste de Correlação de Pearson para duas variáveis de uma amostra
test_Corr <- function(var1, var2) {
  if (is.numeric(var1) && is.numeric(var2)) {
    return(cor(var1, var2, method = "pearson", use = "complete.obs"))
  } else {
    message("Insira colunas númericas!")
  }
}

# Função para calcular a moda
moda <- function(data) {
  data <- data[!is.na(data)]
  uniq_value <- unique(data)
  uniq_value[which.max(tabulate(match(data, uniq_value)))]
}

# Função para criar tabela de distribuição de frequências com as frequências absolutas, frequências relativas, frequências absolutas acumuladas e frequências relativas acumuladas 
create_freq_tables <- function(column) {
  # Definir o número de classes (k) usando a Regra de Sturges
  n <- length(column)
  k <- 1 + 3.322 * log10(n)
  
  # Calcular a amplitude total (R)
  R <- max(column) - min(column)
  
  # 3. Calcular a amplitude dos intervalos (h)
  h <- R / k
  
  # 4. Definir os intervalos usando a função cut
  intervals <- cut(column, breaks = seq(min(column), max(column), by = h), right = FALSE)
  
  # 5. Calcular as frequências absolutas e relativas
  absol_freq <- table(intervals)
  rel_freq <- prop.table(absol_freq)
  
  # 6. Criar a tabela final
  freq_tables <- data.frame(
    Intervalos = names(absol_freq),
    Frequencia_Absoluta = as.vector(absol_freq),
    Frequencia_Relativa = as.vector(rel_freq),
    Frequencia_Absoluta_Acumulada = cumsum(as.vector(absol_freq)),
    Frequencia_Relativa_Acumulada = cumsum(as.vector(rel_freq))
  )
  
  # 7. Formatar para melhor exibição
  freq_tables <- formattable(freq_tables, list(
    Intervalos = formatter("span", style = x ~ style(color = "black", font.weight = "bold", font.size = "16px")),
    Frequencia_Absoluta = formatter("span", style = x ~ style(color = "black", font.weight = "bold", font.size = "16px")),
    Frequencia_Relativa = formatter("span", style = x ~ style(color = "black", font.weight = "bold", font.size = "16px")),
    Frequencia_Absoluta_Acumulada = formatter("span", style = x ~ style(color = "black", font.weight = "bold", font.size = "16px")),
    Frequencia_Relativa_Acumulada = formatter("span", style = x ~ style(color = "black", font.weight = "bold", font.size = "16px"))
  ))
  
  return(freq_tables)
}

# Função para exibir coeficiente de determinação na regressão linear simples
simple_linear_regression_sqr_r <- function(y,x){
  model <- lm(y~x)
  return(summary(model)$r.square)
}

# Padronização (Z-score)
standardize_z_score <- function(x) {
  if(is.numeric(x)) {
    return((x - mean(x)) / sd(x))
  } else {
    return(x)
  }
}
