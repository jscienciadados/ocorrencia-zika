# Mapeando a Ocorrência do Vírus Zika

# Define diretorio de trabalho

setwd("C:/Users/Administrador/Desktop/DataSciecence/ZikaVirus")

# Pacotes
library(dplyr)
library(ggplot2)

# Listando os arquivos e gerando uma lista com os respctivos nomes
temp_files <- list.files(pattern = ".csv")
temp_files

# Organizando os dados

# Carregando todos os arquivos em um único objeto
myfiles <- lapply(temp_files, read.csv, stringsAsFactors = FALSE)

# Resumo dos arquivos
str(myfiles, 1)
lapply(myfiles, names)[1]

lapply(myfiles, head,2)[1:2]

# Organizando o shape dos dados
brazil <- do.call(rbind, myfiles)
brazil <- brazil %>% 
  mutate(report_date = as.Date(report_date))

# Visualizando o dataset
glimpse(brazil)

# Transformando o dataframe em um objeto dplyr
# Transformando o dataframe um uma tabela dplyr e removendo as colunas 5 a 7
brazil <- brazil %>% select(-(6:7)) 

# Visualizando as primeiras 20 linhas
brazil %>% slice (1:20) 

# Para cada reporting_date nós temos 5 regiões
brazil %>% filter(location_type == "region")

# Visualização
brazil %>% filter(location_type == "region") %>% 
  ggplot(aes(x = report_date, y = value, group = location, color = location)) + 
  geom_line() +  
  geom_point() +
  ggtitle("Casos de Zika por Região do Brasil")





















