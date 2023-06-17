################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","correlation","see",
             "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
             "future","metan")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

################################################################################
#                            REGRESSÃO LINEAR MÚLTIPLA                         #
#                      TCC - CARREGAMENTO DA BASE DE DADOS TCC_R               #
################################################################################

#Listar os arquivos do nosso project
list.files()

#library(readxl)
#library(readxl)
#TCC_R <- read_excel("TCC.R.xlsx", col_types = c("date", 
#                                                "numeric", "numeric", "numeric", "numeric", 
#                                                "numeric"))
#View(TCC_R)

#save(list = ls(all = TRUE), file = "TCC_R.Rdata")

#Carregando a base de dados
load(file = "TCC_R.Rdata")


#Visualizando as observações e as especificações referentes às variáveis do dataset
glimpse(TCC_R) 

#colocar no TCC

#Estatísticas univariadas
summary(TCC_R) 

# Série Temporal

# Considerando as demais informações da base de dados basepetr4

TCC_R_rev <- TCC_R

SM_ts=ts(TCC_R_rev[2])
txjuros_ts=ts(TCC_R_rev[3])
IPCA_acumulado_ts=ts(TCC_R_rev[4])
IGPM_acumulado_ts=ts(TCC_R_rev[5])
INPC_acumulado_ts=ts(TCC_R_rev[6])
EURBRL_ts=ts(TCC_R_rev[7])

# colocando os dados em forma de uma matriz com todos os dados no conjunto dados1

temporal=ts(matrix(1,322,6))
temporal[,1]=SM_ts
temporal[,2]=txjuros_ts
temporal[,3]=IPCA_acumulado_ts
temporal[,4]=IGPM_acumulado_ts
temporal[,5]=INPC_acumulado_ts
temporal[,6]=EURBRL_ts

colnames(temporal)[1]='SM R$'
colnames(temporal)[2]='Tx Juros %'
colnames(temporal)[3]='IPCA_acumulado'
colnames(temporal)[4]='IGPM_acumulado'
colnames(temporal)[5]='INPC_acumulado'
colnames(temporal)[6]='EURBRL R$'

plot(temporal, main="Informações das Variáveis",
     xlab="Tempo em meses (06/96 a 03/23)")

################################################################################
#                             ESTUDO DAS CORRELAÇÕES                           #
################################################################################

#Visualizando a base de dados
TCC_R %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 24)

#A função 'correlation' do pacote 'correlation' faz com que seja estruturado um
#diagrama interessante que mostra a inter-relação entre as variáveis e a
#magnitude das correlações entre elas
#Requer instalação e carregamento dos pacotes 'see' e 'ggraph' para a plotagem
TCC_R %>%
  correlation(method = "pearson") %>%
  plot()

#A função 'chart.Correlation' do pacote 'PerformanceAnalytics' apresenta as
#distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias
chart.Correlation((TCC_R[2:7]), histogram = TRUE) #colocar no TCC

#TCC_R %>%
#  corr_plot(SM, txjuros, IPCA_acumulado, IGPM_acumulado, INPC_acumulado, EURBRL,
#            shape.point = 21,
#            col.point = "black",
#            fill.point = "#FDE725FF",
#            size.point = 2,
#            alpha.point = 0.6,
#            maxsize = 4,
#            minsize = 2,
#            smooth = TRUE,
#            col.smooth = "black",
#            col.sign = "#440154FF",
#            upper = "corr",
#            lower = "scatter",
#            diag.type = "density",
#            col.diag = "#440154FF",
#            pan.spacing = 0,
#            lab.position = "bl")

################################################################################
#    ESTIMANDO UM MODELO MÚLTIPLO COM AS VARIÁVEIS DA BASE DE DADOS TCC_R      #
################################################################################
#Estimando a Regressão Múltipla
modelo_TCC_R_multipla <- lm(formula = SM ~ . -Data,
                            data = TCC_R)

summary(modelo_TCC_R_multipla)
confint(modelo_TCC_R_multipla, level = 0.95) # siginificância de 5%

#Visualização do modelo no ambiente Viewer
#função 'extract_eq' do pacote 'equatiomatic'
#extract_eq(modelo_TCC_R_multipla, use_coefs = T) %>%
#  kable() %>%
#  kable_styling(bootstrap_options = "striped",
#                full_width = F,
#                font_size = 28)

################################################################################
#                            PROCEDIMENTO STEPWISE                             #
################################################################################
#Aplicando o procedimento Stepwise, temos o seguinte código:
step_TCC <- step(modelo_TCC_R_multipla, k = 3.841459)
#INPC_acumulado não é estatisticamente signficante na presença de outras
#variáveis para explicar o comportamento da variável Salário Mínimo.

#De onde vem o argumento k = 3.841459?
qchisq(p = 0.05, df = 1, lower.tail = F)
round(pchisq(3.841459, df = 1, lower.tail = F), 7)

summary(step_TCC)
#teste-F < 0,05, portanto pelo menos um dos b’s é estatisticamente
#significante para a explicação do comportamento de Y.

#teste-t <0,05 para cada um dos parametros, portanto cada um dos parâmetros,
#individualmente, é estatisticamente diferente de zero.

#Este procedimento no R removeu a variável 'INPC_acumulado'.

export_summs(step_TCC, scale = F, digits = 5)

#Parâmetros reais do modelo com procedimento Stepwise
#confint(step_TCC, level = 0.95) # siginificância 5%
#plot_summs(step_TCC, colors = "#440154FF") #função 'plot_summs' do pacote 'ggstance'

#Parâmetros padronizados
plot_summs(step_TCC, scale = TRUE, colors = "#440154FF")
#IPCA_acumulado é a variável mais significante

#Adicionando a caracterização da distribição normal no IC de cada parâmetro beta
#plot_summs(step_TCC, scale = TRUE, plot.distributions = TRUE,
#           inner_ci_level = .95, colors = "#440154FF")

#Comparando os ICs dos betas dos modelos sem e com procedimento Stepwise
#plot_summs(modelo_TCC_R_multipla, step_TCC, scale = TRUE, plot.distributions = TRUE,
#           inner_ci_level = .95, colors = c("#FDE725FF", "#440154FF"))

#Visualização do modelo no ambiente Viewer
#função 'extract_eq' do pacote 'equatiomatic'
#extract_eq(step_TCC, use_coefs = T) %>%
#  kable() %>%
#  kable_styling(bootstrap_options = "striped",
#                full_width = F,
#                font_size = 14)

################################################################################
#        TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE          #
#                             SHAPIRO-FRANCIA                                  #
################################################################################
#Shapiro-Francia: n > 30
sf.test(step_TCC$residuals) #função 'sf.test' do pacote 'nortest'
# p-value > 0,05, portanto o modelo é aderente aos resíduos de normalidade
# utilizando o teste de verifcação de SHAPIRO-FRANCIA, não rejeitamos H0.

################################################################################
#                            TESTE DE BREUSH-PAGAN PARA                        #
#                        DIAGNÓSTICO DE HETEROCEDASTICIDADE                    #                                                            
################################################################################
ols_test_breusch_pagan(step_TCC)
# teste > 0,05, portante temos ausência de heterocedasticidade.
#função 'ols_test_breusch_pagan' do pacote 'olsrr'
#Presença de heterocedasticidade -> omissão de variável(is) explicativa(s) relevante(s)

#H0 do teste: ausência de heterocedasticidade.
#H1 do teste: heterocedasticidade, ou seja, correlação entre resíduos e uma ou mais
#variáveis explicativas, o que indica omissão de variável relevante!

#Plotando os resíduos do modelo step_TCC
TCC_R %>%
  mutate(residuos = step_TCC$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequência") + 
  theme_bw()

#Acrescentando uma curva normal teórica para comparação entre as distribuições
TCC_R %>%
  mutate(residuos = step_TCC$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_TCC$residuals),
                            sd = sd(step_TCC$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

################################################################################
#                         TRANSFORMAÇÃO DE BOX-COX                             #
################################################################################
#Para calcular o lambda de Box-Cox
#função 'powerTransform' do pacote 'car'
lambda_BC <- powerTransform(TCC_R$SM)
lambda_BC

#Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
TCC_R$bcSM <- (((TCC_R$SM ^ lambda_BC$lambda) - 1) / 
                      lambda_BC$lambda)

#Visualizando a nova variável na base de dados
TCC_R %>%
  select(SM, bcSM, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

#Estimando um novo modelo múltiplo com variável dependente transformada por Box-Cox
modelo_bc <- lm(formula = bcSM ~ . -Data -SM, 
                data = TCC_R)

#Parâmetros do modelo
summary(modelo_bc)

#Aplicando o procedimento Stepwise ao modelo Box-Cox
step_TCC_bc <- step(modelo_bc, k = 3.841459)

summary(step_TCC_bc)

#Verificando a normalidade dos resíduos do modelo step_modelo_bc
sf.test(step_TCC_bc$residuals) #função 'sf.test' do pacote 'nortest'

#Resumo dos dois modelos obtidos pelo procedimento Stepwise (linear e com Box-Cox)
#Função 'export_summs' do pacote 'jtools'
export_summs(step_TCC, step_TCC_bc,
             model.names = c("Modelo Linear","Modelo Box-Cox"),
             scale = F, digits = 6)

#Parâmetros padronizados
#Verificar qual a importância de cada variável
#plot_summs(step_TCC_bc, scale = TRUE, colors = "#287D8EFF")

#Salvando os fitted values dos modelos step_empresas e step_modelo_bc no
#dataset empresas
TCC_R$yhat_step_TCC <- step_TCC$fitted.values
TCC_R$yhat_step_TCC_bc <- (((step_TCC_bc$fitted.values*(lambda_BC$lambda))+
                                    1))^(1/(lambda_BC$lambda))

#Visualizando os dois fitted values no dataset
#modelos step_empresas e step_modelo_bc
TCC_R %>%
  select(Data, SM, yhat_step_TCC, yhat_step_TCC_bc) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

#Ajustes dos modelos: valores previstos (fitted values) X valores reais
TCC_R %>%
  ggplot() +
  geom_smooth(aes(x = SM, y = yhat_step_TCC, color = "Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = SM, y = yhat_step_TCC),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = SM, y = yhat_step_TCC_bc, color = "Stepwise Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = SM, y = yhat_step_TCC_bc),
             color = "#287D8EFF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = SM, y = SM), method = "lm", formula = y ~ x,
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#287D8EFF", "#440154FF")) +
  labs(x = "Salário Mínimo", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

##################################################################################
#            DIAGNÓSTICO DE MULTICOLINEARIDADE EM MODELOS DE REGRESSÃO           #
#                   EXEMPLO 06 - CARREGAMENTO DA BASE DE DADOS                   #
##################################################################################

load(file = "TCC_R.Rdata")
##CORRELAÇÃO MUITO ALTA, PORÉM NÃO PERFEITA:
cor(TCC_R$SM, TCC_R$IPCA_acumulado)
cor(TCC_R$SM, TCC_R$IGPM_acumulado)
cor(TCC_R$SM, TCC_R$INPC_acumulado)

TCC_R %>% select(2:7) %>% 
  correlation(method = "pearson") %>%
  plot()

modelo_multicol <- lm(formula = SM ~ . - Data,
                data = TCC_R)

summary(modelo_multicol)
ols_vif_tol(modelo_multicol)

#Existência de variáveis que apresentam a mesma tendência durante
#alguns períodos, em decorrência da seleção de uma amostra que inclua
#apenas observações referentes a estes períodos.

#A boa notícia, conforme também discutem Vasconcellos e Alves (2000), 
#é que a existência de multicolinearidade não afeta a intenção de elaboração
#de previsões, desde que as mesmas condições que geraram os resultados 
#se mantenham para a previsão. Desta forma, as previsões incorporarão 
#o mesmo padrão de relação entre as variáveis explicativas, o que não 
#representa problema algum. Gujarati (2011) ainda destaca que a existência 
#de altas correlações entre variáveis explicativas não gera necessariamente 
#estimadores ruins ou fracos e que a presença de multicolinearidade não 
#significa que o modelo possui problemas. Em outras palavras, alguns autores
#argumentam que uma solução para a multicolinearidade é identificá-la, 
#reconhecê-la e não fazer nada.

##################################################################################
#                       ESTIMAÇÃO DO MODELO DE REGRESSÃO E                       #
#                       DIAGNÓSTICO DE HETEROCEDASTICIDADE                       #                                                            
##################################################################################

#Plotando saeb em função de rendimento, com linear fit
ggplotly(
  ggplot(TCC_R, aes(x = IPCA_acumulado, y = SM)) +
    geom_point(size = 1, color = "#FDE725FF") +
    geom_smooth(method = "lm", formula = y ~ x,
                color = "grey40", se = F) +
    xlab("rendimento") +
    ylab("saeb") +
    theme_classic()
)

#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(step_TCC_bc)

#Estimação do modelo
modelohetero <- lm(formula = SM ~ txjuros,
                   data = TCC_R)

summary(modelohetero)

#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(modelohetero)
#função 'ols_test_breusch_pagan' do pacote 'olsrr'
#Presença de heterocedasticidade -> omissão de variável(is) explicativa(s) relevante(s)

#H0 do teste: ausência de heterocedasticidade.
#H1 do teste: heterocedasticidade, ou seja, correlação entre resíduos e uma ou mais
#variáveis explicativas, o que indica omissão de variável relevante!
