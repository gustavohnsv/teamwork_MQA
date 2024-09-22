# Definindo o diretório de trabalho para a localização da pasta "teamwork_MQA"
setwd(dirname(dirname(getActiveDocumentContext()$path)))

# Caminho para o arquivo onde contém as variáveis que serão utilizadas
file_path <- "./data/trafico-escravos-variaveis-utilizadas.csv"

if (!file.exists(file_path)) { # Verifica se existe um arquivo com o nome do "file_path"
  message("File not found with ", file_path)
} else {
  brute_data <- tryCatch({ # Leitura dos dados brutos
    read.csv("./data/trafico-escravos-variaveis-utilizadas.csv", header = TRUE)  
  }, error = function(e) {
    message("Error reading archive: ", e$message)
    NULL
  })
  if (!is.null(brute_data)) { # Verifica se os dados foram lidos com sucesso
    message("File read succefully with ", file_path)
  }
}

# Remove o caminho para o arquivo (não será mais necessário) 
rm(file_path)