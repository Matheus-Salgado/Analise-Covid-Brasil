

dados_completos=read.csv(file = "C:/Users/matheus/Desktop/Projeto_Covid_em_R/datasets/owid-covid-data.csv")
head(dados_completos,2)
dim(dados_completos)

str(dados_completos)
#date está no formato character, quero no formato date

dados_completos$date=as.Date(dados_completos$date)
class(dados_completos$date)

#pegando apenas os dados relativos ao Brazil

dados_completos_brazil=subset(dados_completos, location=="Brazil")

dias_pandemia=max(dados_completos_brazil$date)-min(dados_completos_brazil$date)
print(dias_pandemia)
class(dias_pandemia)

#quero analisar a quantidade de mortes pelos dias
#seleciono as variaveis importantes
dados_cortados_brazil=dados_completos_brazil[, c("date","new_deaths")]

#descobrindo quantos valores NA há nesses dados:

sum(is.na(dados_cortados_brazil$date))
sum(is.na(dados_cortados_brazil$new_deaths))

#onde não foi informado o número de mortes irei colocar 0, já q estava no inicio de 2020

dados_cortados_brazil[is.na(dados_cortados_brazil)]=0

sum(is.na(dados_cortados_brazil$date))


plot(new_deaths~date,main="Dados do Brazil", xlab="Ano/meses", ylab="Novas mortes", data = dados_cortados_brazil, type = "l")








