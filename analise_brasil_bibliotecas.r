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

# quais s�o os 20 pa�ses que mais pessoas morreram (valor bruto)?

dados_completos %>%
  group_by(location) %>%
  mutate(new_deaths = new_deaths/1000) %>%
  filter(!location %in% c("World", "Europe", "South America", "European Union", "Asia", "North America")) %>%
  summarise(total_mortes = sum(new_deaths, na.rm = TRUE)) %>%
  arrange(desc(total_mortes)) %>%
  head(20) %>%
  ggplot() +
  geom_col(aes(x = total_mortes, y = reorder(location, total_mortes)), fill = "blue") +
  labs(title = "Pa�ses com mais mortos devido ao COVID-19 - valores brutos", x = "mortes em milhares", y = "pa�ses") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 12, face = "bold", color = "black"))

# quais s�o os 20 pa�ses que mais pessoas morreram (valor por milh�o)?

dados_completos %>%
  group_by(location) %>%
  filter(!location %in% c("World", "Europe", "South America", "European Union", "Asia", "North America")) %>%
  summarise(total_mortes = sum(new_deaths_per_million, na.rm = TRUE)) %>%
  arrange(desc(total_mortes)) %>%
  head(20) %>%
  ggplot() +
  geom_col(aes(x = total_mortes, y = reorder(location, total_mortes)), fill = "blue") +
  labs(title = "Pa�ses com mais mortos devido ao COVID-19 - valores por milh�o", x = "mortes por milh�o", y = "pa�ses") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 12, face = "bold", color = "black"))
 
# temos que, ao analisar os valores brutos, o brazil est� na segunda posi��o em v�timas devido ao COVID-19, esta posi��o pode ser
# explicada levando em considera��o o peso da popula��o brasileira.
# J� nos dados por milh�o o brazil cai para a 5� posi��o, mas, apesar de ter caido algumas posi��es, mostra a situa��o complicada do pa�s,
# dado que os estados unidos, que possui um "peso" devido � popula��o semelhante ao brazil, sequer est� nas primeiras 20 posi��es
 

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
  labs(title = "M�dia das mortes di�rias por pa�s", x = "pa�s", y = "valor da m�dia das mortes di�rias") +
  theme_bw() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 8),
        plot.title = element_text(size = 20, face = "bold", color = "darkblue"))




