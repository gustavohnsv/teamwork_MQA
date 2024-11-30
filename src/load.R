# Definindo o diretório de trabalho para a localização do arquivo "winequality.csv"
setwd(dirname(dirname(getActiveDocumentContext()$path)))

# Caminho para o arquivo único com todos os vinhos
file_path <- "./data/winequality-both.csv"

# Caminho para vinhos tintos
file_path_red <- "./data/winequality-red.csv"

# Caminho para vinhos brancos
file_path_white <- "./data/winequality-white.csv"

if (!file.exists(file_path)) { # Verifica se o arquivo para todos os vinhos existem
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
    message("All wines file read successfully")
  }
}


if (!file.exists(file_path_white)) { # Verifica se o arquivo para vinhos brancos existem
  message("File not found at ", file_path_white)
} else {
  wines_white <- tryCatch({
    # Leitura dos dados do arquivo único
    read.csv(file_path_white, header = TRUE, sep = ";")  
  }, error = function(e) {
    message("Error reading file: ", e$message)
    NULL
  })
  
  if (!is.null(wines_white)) { # Verifica se os dados foram lidos com sucesso
    message("White wines file read successfully")
  }
}


if (!file.exists(file_path_red)) { # Verifica se o arquivo para vinhos tintos existem
  message("File not found at ", file_path_red)
} else {
  wines_red <- tryCatch({
    # Leitura dos dados do arquivo único
    read.csv(file_path_red, header = TRUE, sep = ";")  
  }, error = function(e) {
    message("Error reading file: ", e$message)
    NULL
  })
  
  if (!is.null(wines_red)) { # Verifica se os dados foram lidos com sucesso
    message("Red wines file read successfully")
  }
}

# Retira possíveis observações com campos NA
wines <- na.omit(wines)
wines_white <- na.omit(wines_white)
wines_red <- na.omit(wines_red)

# Remove o caminho do arquivo
rm(file_path)
rm(file_path_white)
rm(file_path_red)