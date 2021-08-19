library(lubridate)
library(ggplot2)
library(dplyr)

ler_dados_csv <- function(arquivo){
  dados <- read.csv(file = arquivo)
  return(dados)
}

dados_completos <- ler_dados_csv("C:/Users/matheus/Desktop/Projeto_Covid_em_R/datasets/owid-covid-data.csv")


dados_completos$date <- as.Date(dados_completos$date)
class(dados_completos$date)

sum(is.na(dados_completos$date))
sum(is.na(dados_completos$new_deaths))

dados_completos$new_deaths[is.na(dados_completos$new_deaths)] <- 0

dados_completos %>%
  filter(location == "Brazil") %>%
  ggplot() +
  geom_line(aes(x = date, y = new_deaths), color = "blue")
