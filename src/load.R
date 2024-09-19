# Verifica se está no diretório correto para tentar ler o arquivo
if (!(grepl("/Documentos/workspace/teamwork_MQA", getwd()))) {
  setwd("Documentos/workspace/teamwork_MQA")  
}

file_path <- "./data/trafico-escravos-variaveis-utilizadas.csv"

if (!file.exists(file_path)) {
  message("File not found with ", file_path)
} else {
  # Leitura dos dados brutos
  brute_data <- tryCatch({
    read.csv("./data/trafico-escravos-variaveis-utilizadas.csv", header = TRUE)  
  }, error = function(e) {
    message("Error reading archive: ", e$message)
    NULL
  })
  
  if (!is.null(brute_data)) {
    message("File read succefully with ", file_path)
  }
  
}
