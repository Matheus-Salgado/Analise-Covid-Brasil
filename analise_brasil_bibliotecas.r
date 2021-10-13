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

# quais s�o os 20 pa�ses que mais pessoas morreram (valor bruto)?

dados_completos %>%
  group_by(location) %>%
  mutate(new_deaths = new_deaths/1000) %>%
  filter(!location %in% c("World", "Europe", "South America", "European Union", "Asia", "North America")) %>%
  summarise(total_mortes = sum(new_deaths, na.rm = TRUE)) %>%
  arrange(desc(total_mortes)) %>%
  head(20) %>%
  ggplot(aes(x = total_mortes, y = reorder(location, total_mortes), fill = total_mortes)) +
  geom_col() +
  labs(title = "Pa�ses com mais mortos devido ao COVID-19 - valores brutos", x = "mortes em milhares", y = "pa�ses") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 12, face = "bold", color = "black")) +
  geom_text(aes(label = total_mortes, x = total_mortes+40))



# quais s�o os 20 pa�ses que mais pessoas morreram (valor por milh�o)?

dados_completos %>%
  group_by(location) %>%
  filter(!location %in% c("World", "Europe", "South America", "European Union", "Asia", "North America")) %>%
  summarise(total_mortes = sum(new_deaths_per_million, na.rm = TRUE)) %>%
  arrange(desc(total_mortes)) %>%
  head(20) %>%
  ggplot(aes(x = total_mortes, y = reorder(location, total_mortes), fill = total_mortes)) +
  geom_col() +
  labs(title = "Pa�ses com mais mortos devido ao COVID-19 - valores por milh�o", x = "mortes por milh�o", y = "pa�ses") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 12, face = "bold", color = "black")) +
  geom_text(aes(label = total_mortes, x = total_mortes+600))
 
# temos que, ao analisar os valores brutos, o brazil est� na segunda posi��o em v�timas devido ao COVID-19, esta posi��o pode ser
# explicada levando em considera��o o peso da popula��o brasileira.
# J� nos dados por milh�o o brazil cai para a 5� posi��o, mas, apesar de ter caido algumas posi��es, mostra a situa��o complicada do pa�s,
# dado que os estados unidos, que possui um "peso" devido � popula��o semelhante ao brazil, sequer est� nas primeiras 20 posi��es
 

 ###############################################################################

# quais s�o os pa�ses que mais aplicaram dose?

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
  labs(title = "TOP 10 pa�ses que mais aplicaram dose - valores brutos", x = "doses aplicadas (em milh�es)", y = "pa�ses") +
  theme_light() +
  geom_text(aes(label = total_doses))

# quais s�o os pa�ses que mais t�m pessoas totalmente vacinadas?

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
  labs(title = "TOP 10 pa�ses que mais t�m pessoas completamente vacinadas", x = "quantidade de pessoas (em milh�es)", y = "pa�ses") +
  theme_light() +
  geom_text(aes(label = pessoas_vacinacao_completa))

# o Brazil est� em 3� lugar tanto no total de aplica��es quanto no total de pessoas totalmente vacinadas

################################################################################

# qual foi o ritmo de vacina��o do brazil?

grafico_vacinacao_brazil<-dados_completos_vacinacao %>%
  select(location, date, daily_vaccinations) %>%
  filter(location == "Brazil") %>%
  ggplot(aes(x = date, y = daily_vaccinations)) +
  geom_line() +
  labs(title = "Ritmo de vacina��o di�ria brasil - 2021", x = "meses", y = "doses aplicadas") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%B")

ggplotly(grafico_vacinacao_brazil)
# apesar de ter datas em que a vacina�ao foi reduzida, o brasil apresentou uma tendencia de aumento no ritmo de vacinacao

# compara��o do ritmo de vacina��o entre o brazil e o estados unidos

grafico_vacinacao_brazil_usa<-dados_completos_vacinacao %>%
  select(location, date, daily_vaccinations) %>%
  mutate(daily_vaccinations = daily_vaccinations/1000000) %>%
  filter(location %in% c("Brazil", "United States")) %>%
  group_by(location) %>%
  ggplot(aes(x = date, y = daily_vaccinations, col = location)) +
  geom_line() +
  labs(title = "Ritmo de vacina��o di�ria Brasil x USA - 2021", x = "meses", y = "doses aplicadas (em milhoes)") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%B")

ggplotly(grafico_vacinacao_brazil_usa)

# O estados unidos apresentou um grande ritmo de vacina�ao, sendo que a dificuldade atual � convencer as pessoas a tomarem a vacina,
# o que pode ser visto tamb�m nos noticiarios, onde algumas empresas americanas irao come�ar a cobrar uma taxa extra dos funcionarios
# nao vacinados


################################################################################
  



