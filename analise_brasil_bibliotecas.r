library(lubridate)
library(ggplot2)
library(dplyr)
library(magrittr)

ler_dados_csv <- function(arquivo){
  dados <- read.csv(file = arquivo)
  return(dados)
}

dados_completos <- ler_dados_csv("C:/Users/matheus/Desktop/Projeto_Covid_em_R/datasets/owid-covid-data.csv")

dados_completos <- as_tibble(dados_completos)

dados_completos$date <- as.Date(dados_completos$date)
class(dados_completos$date)

sum(is.na(dados_completos$date))
sum(is.na(dados_completos$new_deaths))
sum(is.na(dados_completos$continent))

dados_completos$new_deaths[is.na(dados_completos$new_deaths)] <- 0

dados_completos %>%
  select(date,location, new_deaths) %>%
  filter(location == "Brazil") %>%
  ggplot() +
  geom_line(aes(x = date, y = new_deaths), color = "blue")

dados_completos %>%
  select(date,location,new_deaths) %>%
  filter (location == "Brazil", year(date) == "2020") %>%
  
dados_completos %>%
  filter(location == "Brazil") %>%
  summarize(
    media = mean(new_deaths),
    mediana = median(new_deaths)
)

dados_completos %>%
  select(location, new_deaths) %>%
  filter(location %in% c("Brazil", "United States", "Spain", "Portugal", "Russia")) %>%
  group_by(location) %>%
  summarize(media_mortes = mean (new_deaths)) %>%
  ggplot() +
  geom_col(aes(x = location, y = media_mortes), color = "blue")



dados_completos$location <- as.factor(dados_completos$location)
levels(dados_completos$location)

