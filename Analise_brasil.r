library(lubridate)


dados_completos <- read.csv(file = "C:/Users/matheus/Desktop/Projeto_Covid_em_R/datasets/owid-covid-data.csv")
head(dados_completos,2)
dim(dados_completos)

str(dados_completos)
#date está no formato character, quero no formato date

dados_completos$date <- as.Date(dados_completos$date)
class(dados_completos$date)


dados_completos_brazil <- subset(dados_completos, location=="Brazil")

dias_pandemia_brazil <- max(dados_completos_brazil$date)-min(dados_completos_brazil$date)
print(dias_pandemia_brazil)
class(dias_pandemia_brazil)


#seleciono as variaveis importantes
dados_cortados_brazil <- dados_completos_brazil[, c("date","new_deaths")]

#descobrindo quantos valores NA há nesses dados:

sum(is.na(dados_cortados_brazil$date))
sum(is.na(dados_cortados_brazil$new_deaths))

#onde não foi informado o número de mortes irei colocar 0, já q estava no inicio de 2020

dados_cortados_brazil[is.na(dados_cortados_brazil)] <- 0

sum(is.na(dados_cortados_brazil$date))


plot(new_deaths~date,main="Dados do Brazil", xlab="Ano/meses", ylab="Novas mortes", data = dados_cortados_brazil, type = "l")

#analise em 2020

dados_2020_brazil <- subset(dados_cortados_brazil,year(dados_cortados_brazil$date)=="2020")

nomes_meses <- c("feb","mar","apr","may", "jun", "jul", "aug", "sept","oct","nov", "dec" )
meses <- 2:12

mortes_mensais_2020 <- numeric()
for (i in 1:11){
  mortes_mensais_2020[i] <- sum(subset(dados_2020_brazil, month(dados_2020$date)==meses[i])$new_deaths)
  
}

barplot(mortes_mensais_2020,xlab="meses", ylab="mortes", main = "Ano 2020", names.arg = nomes_meses, col = "blue")

