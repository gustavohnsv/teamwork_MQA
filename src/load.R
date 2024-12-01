# Definindo o diretório de trabalho para a localização da pasta "teamwork_MQA"
#setwd(dirname(dirname(getActiveDocumentContext()$path)))

# Caminho para os arquivos onde contém as variáveis que serão utilizadas
file_path <- c("./data/winequality-red.csv", "./data/winequality-white.csv")

if (!file.exists(file_path[1]) || !file.exists(file_path[2])) { # Verifica se existe um arquivo com o nome do "file_path"
  message("File not found with ", file_path)
} else {
  red_wines <- tryCatch({ # 1° Leitura dos dados brutos
    read.csv(file_path[1], header = TRUE, sep = ";")  
  }, error = function(e) {
    message("Error reading archive: ", e$message)
    NULL
  })
  white_wines <- tryCatch({ # 2° Leitura dos dados brutos
    read.csv(file_path[2], header = TRUE, sep = ";")  
  }, error = function(e) {
    message("Error reading archive: ", e$message)
    NULL
  })
  if (!is.null(red_wines) && !is.null(white_wines)) { # Verifica se os dados foram lidos com sucesso
    message("Files read succefully")
  }
}

if (!is.null(red_wines) && !is.null(white_wines)) {
  message("Datasets carregados com sucesso!")
}

# Carregando os dados
file_path_red <- "./data/winequality-red.csv"
file_path_white <- "./data/winequality-white.csv"

tryCatch({
  wines_red <- load_wine_data(file_path_red)
  wines_white <- load_wine_data(file_path_white)
}, error = function(e) {
  message("Erro ao carregar os arquivos: ", e$message)
})

# Remove o caminho para os arquivos (não será mais necessário) 
rm(file_path)