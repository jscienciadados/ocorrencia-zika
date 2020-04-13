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

# Separando as regiões e visualizando os dados
region <- brazil %>% 
  filter(location_type == "region")

region %>% 
  ggplot(aes(x =location, y = value)) + geom_bar(stat = "identity") +
  ylab("Número de Casos Reportados") + xlab("Region") + 
  ggtitle("Casos de Zika Reportados no Brasil")

region %>% 
  slice(1:length(unique(region$location))) %>% 
  arrange(desc(value)) %>%
  mutate(location = factor(location, levels = location,ordered = TRUE)) %>%
  ggplot(aes(x = location, y = value)) + geom_bar(stat = "identity") +
  ylab("Número de Casos Reportados") + xlab("Region") + 
  ggtitle("Casos de Zika Reportados no Brasil")

# Obtendo localidades únicas
region %>% 
  slice(1:length(unique(region$location)))

# Organziando as localidades únicas por número de casos reportados
region %>% 
  slice(1:length(unique(region$location))) %>% 
  arrange(desc(value))

# Criando variáveis do tipo fator
region %>% 
  slice(1:length(unique(region$location))) %>% 
  arrange(desc(value)) %>%
  mutate(location = factor(location,levels=location,ordered=TRUE)) %>% 
  glimpse()

# Agrupando e sumarizando
brazil_totals <- brazil %>% filter(location=="Brazil") 
region_totals <- brazil %>% filter(location_type=="region") %>%
  group_by(report_date,location) %>%  
  summarize(tot = sum(value)) 

# Padronizar os dados e remover as sumarizações
regvec <- vector()  
length(regvec) <- nrow(brazil)
for (ii in 1:nrow(brazil)) {
  if (brazil[ii,]$location_type != "region")  {
    regvec[ii] <- newlab
  } else {
    newlab <- brazil[ii,]$location
    regvec[ii] <- newlab
  }
}

# Agregando o vetor de regiões ao dataframe brasil
statedf <- cbind(brazil,regvec)

# Eliminar o sumário de linhas por região e país
statedf <- statedf %>% filter(location != "Brazil") 
statedf <- statedf %>% filter(location_type != "region")

# Gerar o total por regiões a partir dos dados transformados
statedf %>% group_by(report_date,regvec) %>% 
  summarize(tot=sum(value)) -> totals

























