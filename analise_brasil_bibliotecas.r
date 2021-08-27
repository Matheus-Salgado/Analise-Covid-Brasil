library(lubridate)
library(ggplot2)
library(dplyr)
library(magrittr)
library(plotly)

ler_dados_csv <- function(arquivo){
  dados <- read.csv(file = arquivo)
  return(dados)
}

dados_completos <- ler_dados_csv("C:/Users/matheus/Desktop/Projeto_Covid_em_R/datasets/owid-covid-data.csv")

View(dados_completos)
summary(dados_completos)

# quais são os 20 países que mais pessoas morreram (valor bruto)?

dados_completos %>%
  group_by(location) %>%
  mutate(new_deaths = new_deaths/1000) %>%
  filter(!location %in% c("World", "Europe", "South America", "European Union", "Asia", "North America")) %>%
  summarise(total_mortes = sum(new_deaths, na.rm = TRUE)) %>%
  arrange(desc(total_mortes)) %>%
  head(20) %>%
  ggplot() +
  geom_col(aes(x = total_mortes, y = reorder(location, total_mortes)), fill = "blue") +
  labs(title = "Países com mais mortos devido ao COVID-19 - valores brutos", x = "mortes em milhares", y = "países") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 12, face = "bold", color = "black"))

# quais são os 20 países que mais pessoas morreram (valor por milhão)?

dados_completos %>%
  group_by(location) %>%
  filter(!location %in% c("World", "Europe", "South America", "European Union", "Asia", "North America")) %>%
  summarise(total_mortes = sum(new_deaths_per_million, na.rm = TRUE)) %>%
  arrange(desc(total_mortes)) %>%
  head(20) %>%
  ggplot() +
  geom_col(aes(x = total_mortes, y = reorder(location, total_mortes)), fill = "blue") +
  labs(title = "Países com mais mortos devido ao COVID-19 - valores por milhão", x = "mortes por milhão", y = "países") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 12, face = "bold", color = "black"))
 
# temos que, ao analisar os valores brutos, o brazil está na segunda posição em vítimas devido ao COVID-19, esta posição pode ser
# explicada levando em consideração o peso da população brasileira.
# Já nos dados por milhão o brazil cai para a 5° posição, mas, apesar de ter caido algumas posições, mostra a situação complicada do país,
# dado que os estados unidos, que possui um "peso" devido à população semelhante ao brazil, sequer está nas primeiras 20 posições
 

 #######################proxima pergunta#######################################

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
  filter(location == "Brazil") %>%
  summarize(
    media = mean(new_deaths),
    mediana = median(new_deaths)
)

dados_completos %>%
  select(location, new_deaths) %>%
  filter(location %in% c("Brazil", "United States", "Russia", "Mexico", "India")) %>%
  group_by(location) %>%
  summarize(media_mortes = mean (new_deaths)) %>%
  ggplot() +
  geom_col(aes(x = reorder(location,-media_mortes), y = media_mortes), color = "blue", fill = "blue") +
  labs(title = "Média das mortes diárias por país", x = "país", y = "valor da média das mortes diárias") +
  theme_bw() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 8),
        plot.title = element_text(size = 20, face = "bold", color = "darkblue"))




