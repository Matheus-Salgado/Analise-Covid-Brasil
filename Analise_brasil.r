library(lubridate)


ler_dados <- function(arquivo){
  
  dados <- read.csv(file = arquivo)
  return (dados)
}

escolher_pais <- function(dados, pais){
  
  dados_pais <- subset(dados, location == pais)
  return (dados_pais)
}

escolher_ano <- function(dados, ano){
  
  dados_ano <- subset(dados, year(dados$date) == ano)
  return (dados_ano)
}

dados_completos <- ler_dados("C:/Users/matheus/Desktop/Projeto_Covid_em_R/datasets/owid-covid-data.csv")
dados_vacinacao <- ler_dados("C:/Users/matheus/Desktop/Projeto_covid_em_R/datasets/VaccinationData.csv")

dim(dados_completos)

str(dados_completos)
#date está no formato character, quero no formato date

dados_completos$date <- as.Date(dados_completos$date)
class(dados_completos$date)


dados_completos_brazil <- escolher_pais(dados_completos, "Brazil")


dias_pandemia_brazil <- max(dados_completos_brazil$date)-min(dados_completos_brazil$date)
print(dias_pandemia_brazil)
class(dias_pandemia_brazil)


#selecionando variaveis
dados_cortados_brazil <- dados_completos_brazil[, c("date","new_deaths")]

#descobrindo quantos valores NA há nesses dados:

sum(is.na(dados_cortados_brazil$date))
sum(is.na(dados_cortados_brazil$new_deaths))

#onde não foi informado o número de mortes irei colocar 0, já q estava no inicio de 2020

dados_cortados_brazil$new_deaths[is.na(dados_cortados_brazil$new_deaths)] <- 0

sum(is.na(dados_cortados_brazil$new_deaths))


plot(new_deaths~date,main="Dados do Brazil", xlab="Ano/meses", ylab="Novas mortes", data = dados_cortados_brazil, type = "l")

#analise em 2020

dados_2020_brazil <- escolher_ano(dados_cortados_brazil, 2020)

nomes_meses_2020 <- c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sept", "oct", "nov", "dec")
inicio_2020 <- min(month(dados_2020_brazil$date))
final_2020 <- max(month(dados_2020_brazil$date))

meses_2020 <- inicio_2020:final_2020
mortes_mensais_2020 <- numeric()

for (i in 1:length(meses_2020)){
  mortes_mensais_2020[i] <- sum(subset(dados_2020_brazil, month(dados_2020_brazil$date) == meses_2020[i])$new_deaths)
  
}

barplot(mortes_mensais_2020,xlab = "meses", ylab = "mortes", main = "Ano 2020", names.arg = nomes_meses_2020, col = "blue")

dados_2021_brazil <- escolher_ano(dados_cortados_brazil, 2021)
min(month(dados_2021_brazil$date))
max(month(dados_2021_brazil$date))

inicio_2021 <- min(month(dados_2021_brazil$date))
final_2021 <- max(month(dados_2021_brazil$date))

meses_2021 <- inicio_2021:final_2021

mortes_mensais_2021 <- numeric()
for (i in 1:length(meses_2021)){
  mortes_mensais_2021[i] <- sum(subset(dados_2021_brazil, month(dados_2021_brazil$date) == meses_2021[i])$new_deaths)
  
}

barplot(mortes_mensais_2021, xlab = "meses", ylab = "mortes", main = "ano 2021", names.arg = meses_2021, col = "grey")


par(mfrow=c(1,2))
barplot(mortes_mensais_2020,xlab = "meses", ylab = "mortes", main = "Ano 2020", names.arg = nomes_meses_2020, col = "blue")
barplot(mortes_mensais_2021, xlab = "meses", ylab = "mortes", main = "ano 2021", names.arg = meses_2021, col = "grey")

dev.off()


class(dados_vacinacao$date)  
dados_vacinacao$date <- as.Date(dados_vacinacao$date)  


dados_vacinacao_brazil <- subset(dados_vacinacao, location == "Brazil")

sum(is.na(dados_vacinacao_brazil$total_vaccinations))
sum(is.na(dados_vacinacao_brazil$date))
sum(is.na(dados_vacinacao_brazil$daily_vaccinations))
dados_vacinacao_brazil$daily_vaccinations[is.na(dados_vacinacao_brazil$daily_vaccinations)] <- 0

length(dados_vacinacao_brazil$total_vaccinations)

#possibilitar o uso do IF para tratar os dados
dados_vacinacao_brazil$total_vaccinations[is.na(dados_vacinacao_brazil$total_vaccinations)] <- 0

#caso não foi informado o total de pessoas, eu suponho que é o mesmo do dia anterior
for (i in 2:length(dados_vacinacao_brazil$total_vaccinations)){
  
  if (dados_vacinacao_brazil$total_vaccinations[i] == 0){
    dados_vacinacao_brazil$total_vaccinations[i] <- dados_vacinacao_brazil$total_vaccinations[i-1]
  }
}
  
plot(dados_vacinacao_brazil$total_vaccinations~dados_vacinacao_brazil$date,
     main = "Vacinaçao em 2021", xlab = "mês", ylab = "pessoas vacinadas", type = "l")


plot(dados_vacinacao_brazil$daily_vaccinations~dados_vacinacao_brazil$date,
     main = "doses aplicadas por dia - 2021", xlab = "mês",
     ylab = "quantidade de doses", type = "l")


par(mfrow = c(1,2)) 

plot(dados_2021_brazil$new_deaths~dados_2021_brazil$date,
     main="mortes diárias Brazil 2021", xlab="mês", ylab="Novas mortes", type = "l")

plot(dados_vacinacao_brazil$daily_vaccinations~dados_vacinacao_brazil$date,
     main = "doses aplicadas por dia - 2021", xlab = "mês",
     ylab = "quantidade de doses", type = "l")
