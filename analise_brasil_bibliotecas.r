library(lubridate)
library(ggplot2)
library(dplyr)
library(magrittr)
library(plotly)
library(grid)
library(patchwork)

ler_dados_csv <- function(arquivo){
  dados <- read.csv(file = arquivo)
  return(dados)
}

dados_completos <- ler_dados_csv("C:/Users/matheus/Desktop/Projeto_Covid_em_R/datasets/owid-covid-data.csv")
dados_completos_vacinacao <- ler_dados_csv("C:/Users/matheus/Desktop/Projeto_Covid_em_R/datasets/VaccinationData.csv")
dados_completos$date <- as.Date(dados_completos$date)
dados_completos_vacinacao$date <- as.Date(dados_completos_vacinacao$date)

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
  ggplot(aes(x = total_mortes, y = reorder(location, total_mortes), fill = total_mortes)) +
  geom_col() +
  labs(title = "Países com mais mortos devido ao COVID-19 - valores brutos", x = "mortes em milhares", y = "países") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 12, face = "bold", color = "black")) +
  geom_text(aes(label = total_mortes, x = total_mortes+40))



# quais são os 20 países que mais pessoas morreram (valor por milhão)?

dados_completos %>%
  group_by(location) %>%
  filter(!location %in% c("World", "Europe", "South America", "European Union", "Asia", "North America")) %>%
  summarise(total_mortes = sum(new_deaths_per_million, na.rm = TRUE)) %>%
  arrange(desc(total_mortes)) %>%
  head(20) %>%
  ggplot(aes(x = total_mortes, y = reorder(location, total_mortes), fill = total_mortes)) +
  geom_col() +
  labs(title = "Países com mais mortos devido ao COVID-19 - valores por milhão", x = "mortes por milhão", y = "países") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 12, face = "bold", color = "black")) +
  geom_text(aes(label = total_mortes, x = total_mortes+600))
 
# temos que, ao analisar os valores brutos, o brazil está na segunda posição em vítimas devido ao COVID-19, esta posição pode ser
# explicada levando em consideração o peso da população brasileira.
# Já nos dados por milhão o brazil cai para a 5° posição, mas, apesar de ter caido algumas posições, mostra a situação complicada do país,
# dado que os estados unidos, que possui um "peso" devido à população semelhante ao brazil, sequer está nas primeiras 20 posições
 

 ###############################################################################

# quais são os países que mais aplicaram dose?

dados_completos %>%
  filter(!location %in% c("World", "Europe", "South America", "European Union", "Asia", "North America", "China") &
         !is.na(total_vaccinations)) %>%
  group_by(location) %>%
  select(location, total_vaccinations) %>%
  summarise(total_doses = max(total_vaccinations, na.rm = TRUE)) %>%
  arrange(desc(total_doses)) %>%
  mutate(total_doses = total_doses/1000000) %>%
  head(20) %>%
  ggplot(aes(x = total_doses, y = reorder(location, total_doses), fill = total_doses)) +
  geom_col() +
  labs(title = "TOP 10 países que mais aplicaram dose - valores brutos", x = "doses aplicadas (em milhões)", y = "países") +
  theme_light() +
  geom_text(aes(label = total_doses))

# quais são os países que mais têm pessoas totalmente vacinadas?

dados_completos %>%
  filter(!location %in% c("World", "Europe", "South America", "European Union", "Asia", "North America", "China") &
         !is.na(people_fully_vaccinated)) %>%
  group_by(location) %>%
  select(location, people_fully_vaccinated) %>%
  summarise(pessoas_vacinacao_completa = max(people_fully_vaccinated, na.rm = TRUE)) %>%
  arrange(desc(pessoas_vacinacao_completa)) %>%
  mutate(pessoas_vacinacao_completa = pessoas_vacinacao_completa/1000000) %>%
  head(20) %>%
  ggplot(aes(x = pessoas_vacinacao_completa, y = reorder(location, pessoas_vacinacao_completa), fill = pessoas_vacinacao_completa)) +
  geom_col() +
  labs(title = "TOP 10 países que mais têm pessoas completamente vacinadas", x = "quantidade de pessoas (em milhões)", y = "países") +
  theme_light() +
  geom_text(aes(label = pessoas_vacinacao_completa))

# o Brazil está em 3° lugar tanto no total de aplicações quanto no total de pessoas totalmente vacinadas

################################################################################

# qual foi o ritmo de vacinação do brazil?

grafico_vacinacao_brazil<-dados_completos_vacinacao %>%
  select(location, date, daily_vaccinations) %>%
  filter(location == "Brazil") %>%
  ggplot(aes(x = date, y = daily_vaccinations)) +
  geom_line() +
  labs(title = "Ritmo de vacinação diária brasil - 2021", x = "meses", y = "doses aplicadas") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%B")

ggplotly(grafico_vacinacao_brazil)
# apesar de ter datas em que a vacinaçao foi reduzida, o brasil apresentou uma tendencia de aumento no ritmo de vacinacao

# comparação do ritmo de vacinação entre o brazil e o estados unidos

grafico_vacinacao_brazil_usa<-dados_completos_vacinacao %>%
  select(location, date, daily_vaccinations) %>%
  mutate(daily_vaccinations = daily_vaccinations/1000000) %>%
  filter(location %in% c("Brazil", "United States")) %>%
  group_by(location) %>%
  ggplot(aes(x = date, y = daily_vaccinations, col = location)) +
  geom_line() +
  labs(title = "Ritmo de vacinação diária Brasil x USA - 2021", x = "meses", y = "doses aplicadas (em milhoes)") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%B")

ggplotly(grafico_vacinacao_brazil_usa)

# O estados unidos apresentou um grande ritmo de vacinaçao, sendo que a dificuldade atual é convencer as pessoas a tomarem a vacina,
# o que pode ser visto também nos noticiarios, onde algumas empresas americanas irao começar a cobrar uma taxa extra dos funcionarios
# nao vacinados


################################################################################
  



