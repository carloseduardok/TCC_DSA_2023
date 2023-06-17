# Análise de microdados da PNAD Contínua
# Limpando arquivos armazenados na memória
# rm(list=ls(all=TRUE))

# install.packages("PNADcIBGE") - https://rpubs.com/gabriel-assuncao-ibge/pnadc
# install.packages("survey")
# install.packages("convey")
# install.packages("magrittr")

library(PNADcIBGE)
library(survey) 
library(convey)
library(magrittr)

# Importar online microdados do 4º trimestre de 2022.
# VD4001 = 	Condição em relação à força de trabalho na semana de referência 
# para pessoas de 14 anos ou mais de idade.
# VD4020 = Rendimento mensal efetivo de todos os trabalhos para pessoas de 14
# anos ou mais de idade (apenas para pessoas que receberam em dinheiro, 
# produtos ou mercadorias em qualquer trabalho).

dadosPNADc <- get_pnadc(year = 2022, quarter = 4, vars = c("VD4001","VD4020"))
dadosPNADc

#dadosPNADc_brutos <- get_pnadc(year = 2022, quarter = 4, vars = c("VD4001","VD4020"), design=FALSE)
#dadosPNADc_brutos

#save(list = ls(all = TRUE), file = "dadosPNADc.Rdata")

# Classe do objeto
class(dadosPNADc)

# Obter nº total
svytotal(x = ~VD4001, design = dadosPNADc, na.rm = TRUE)
svytotal(x = ~VD4020, design = dadosPNADc, na.rm = TRUE)

# Estimar a frequência relativa de cada categoria (valor nominal)
ate1SM <- svymean(x=~VD4020<=1212, design=dadosPNADc, na.rm=TRUE)
ate1SM
#cv(object=ate1SM) # coeficiente de variação
#confint(object=ate1SM) # intervalos de confiança

entre1SMe2SM <- svymean(x=~VD4020>1212 & VD4020<=2424, design=dadosPNADc, na.rm=TRUE)
entre1SMe2SM
#cv(object=entre1SMe2SM) # coeficiente de variação
#confint(object=entre1SMe2SM) # intervalos de confiança

entre2SMe3SM <- svymean(x=~VD4020>2424 & VD4020<=3636, design=dadosPNADc, na.rm=TRUE)
entre2SMe3SM
#cv(object=entre2SMe3SM) # coeficiente de variação
#confint(object=entre2SMe3SM) # intervalos de confiança

maiorque3SM <- svymean(x=~VD4020>3636, design=dadosPNADc, na.rm=TRUE)
maiorque3SM
#cv(object=maiorque3SM) # coeficiente de variação
#confint(object=maiorque3SM) # intervalos de confiança

# Estimar a frequência relativa de cada categoria (valor real, corrigido pela inflação)
#dadosPNADc$variables <- transform(dadosPNADc$variables, VD4020_real=VD4020*Efetivo)
#totalrenda_real <- svytotal(x=~VD4020_real, design=dadosPNADc, na.rm=TRUE)

#ate1SM_real <- svymean(x=~VD4020_real<=1212, design=dadosPNADc, na.rm=TRUE)
#entre1SMe2SM_real <- svymean(x=~VD4020_real>1212 & VD4020_real<=2424, design=dadosPNADc, na.rm=TRUE)
#entre2SMe3SM_real <- svymean(x=~VD4020_real>2424 & VD4020_real<=3636, design=dadosPNADc, na.rm=TRUE)
#maiorque3SM_real <- svymean(x=~VD4020_real>3636, design=dadosPNADc, na.rm=TRUE)

#ate1SM_real

#save(list = ls(all = TRUE), file = "Faixa_SM.Rdata")