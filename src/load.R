# Definindo o diretório de trabalho para a localização do arquivo "winequality.csv"
setwd(dirname(dirname(getActiveDocumentContext()$path)))

# Caminho para o arquivo único com todos os vinhos
file_path <- "./data/winequality-both.csv"

if (!file.exists(file_path)) { # Verifica se o arquivo existe
  message("File not found at ", file_path)
} else {
  wines <- tryCatch({
    # Leitura dos dados do arquivo único
    read.csv(file_path, header = TRUE, sep = ",")  
  }, error = function(e) {
    message("Error reading file: ", e$message)
    NULL
  })
  
  if (!is.null(wines)) { # Verifica se os dados foram lidos com sucesso
    message("File read successfully")
  }
}

# Retira possíveis observações com campos NA
wines <- na.omit(wines)

# Remove o caminho do arquivo
rm(file_path)