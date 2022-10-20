#Pacotes
install.packages('DescTools')
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tinytex", "tidyverse", "xtable", "knitr", "patchwork", "raster", "rgdal", "broom", "maptools", "gpclib", "viridis", "data.table", "psych", "Hmisc", "Weighted.Desc.Stat", "weights", "RColorBrewer", "scales", "stargazer", "plotly", "wesanderson", "lessR", "kableExtra", "glm")
library(margins)
library(modelsummary)
library(DescTools)
library(lmtest)
library(plm)

#Base de Dados
df.total <- read.csv('bases_eleicao.csv')

#dummies de ano
df.total$d2002 <- ifelse(df.total$ANO_ELEICAO == 2002, 1, 0)
df.total$d2006 <- ifelse(df.total$ANO_ELEICAO == 2006, 1, 0)
df.total$d2010 <- ifelse(df.total$ANO_ELEICAO == 2010, 1, 0)
df.total$d2014 <- ifelse(df.total$ANO_ELEICAO == 2014, 1, 0)
df.total$d2018 <- ifelse(df.total$ANO_ELEICAO == 2018, 1, 0)
#dummie de genero e casado
df.total <- df.total %>% filter(!(DS_GENERO %in% c("N?O INFORMADO","N?O DIVULG?VEL")))
df.total$genero <- ifelse(df.total$DS_GENERO == "MASCULINO", 1, 0)
df.total$casado <- ifelse(df.total$DS_ESTADO_CIVIL == "CASADO(A)", 1, 0)
########## MODELO GERAL########################

#formula
modelo<-eleito ~ genero + NR_IDADE_DATA_POSSE + MEDIA_DESPESA_PARTIDO + IDEO_IMPUTED + casado  + d2002 + d2006 + d2010 + d2014

#modelo linear geral
lmgeral <- lm(modelo,data=df.total)

#probit geral
probitgeral <- glm(modelo, family = binomial(link = "probit"),data = df.total)

#logit geral
logitgeral <- glm(modelo, family = binomial(link = "logit"),data = df.total)

#resultados dos modelos
stargazer(lmgeral,probitgeral,logitgeral,type='text')

#efeitos marginais
modelsummary(list('MPL'=margins(lmgeral),'Probit'=margins(probitgeral),'Logit'=margins(logitgeral)))

########## MODELO PROBIT ANOS ########################

#formula
modeloanos<-eleito ~ genero + NR_IDADE_DATA_POSSE + MEDIA_DESPESA_PARTIDO + IDEO_IMPUTED + casado
#modelos
probit2002 <- glm(modeloanos, family = binomial(link = "probit"),data = df.total%>%filter(ANO_ELEICAO==2002))
probit2006 <- glm(modeloanos, family = binomial(link = "probit"),data = df.total%>%filter(ANO_ELEICAO==2006))
probit2010 <- glm(modeloanos, family = binomial(link = "probit"),data = df.total%>%filter(ANO_ELEICAO==2010))
probit2014 <- glm(modeloanos, family = binomial(link = "probit"),data = df.total%>%filter(ANO_ELEICAO==2014))
probit2018 <- glm(modeloanos, family = binomial(link = "probit"),data = df.total%>%filter(ANO_ELEICAO==2018))
#resultados
stargazer(probit2002,probit2006,probit2010,probit2014,probit2018,type='text',column.labels = c("2002","2006","2010","2014","2018"))
#efeitos marginais
modelsummary(list("2002"=margins(probit2002),"2006"=margins(probit2006),"2010"=margins(probit2010),"2014"=margins(probit2014),"2018"=margins(probit2018)))

#calculando o pseudo R2
PseudoR2(probit2002, c("McFadden", "Nagel"))
PseudoR2(probit2006, c("McFadden", "Nagel"))
PseudoR2(probit2010, c("McFadden", "Nagel"))
PseudoR2(probit2014, c("McFadden", "Nagel"))
PseudoR2(probit2018, c("McFadden", "Nagel"))

########## MODELO LINEAR ANOS ########################

#modelos
linear2002 <- lm(modeloanos, data = df.total%>%filter(ANO_ELEICAO==2002))
linear2006 <- lm(modeloanos, data = df.total%>%filter(ANO_ELEICAO==2006))
linear2010 <- lm(modeloanos, data = df.total%>%filter(ANO_ELEICAO==2010))
linear2014 <- lm(modeloanos, data = df.total%>%filter(ANO_ELEICAO==2014))
linear2018 <- lm(modeloanos, data = df.total%>%filter(ANO_ELEICAO==2018))
#resultados
stargazer(logit2002,logit2006,logit2010,logit2014,logit2018,type='text',column.labels = c("2002","2006","2010","2014","2018"))
#efeitos marginais
modelsummary(list("2002"=margins(linear2002),"2006"=margins(linear2006),"2010"=margins(linear2010),"2014"=margins(linear2014),"2018"=margins(linear2018)))


########## MODELO LOGIT ANOS ########################

#modelos
logit2002 <- glm(modeloanos, family = binomial(link = "logit"),data = df.total%>%filter(ANO_ELEICAO==2002))
logit2006 <- glm(modeloanos, family = binomial(link = "logit"),data = df.total%>%filter(ANO_ELEICAO==2006))
logit2010 <- glm(modeloanos, family = binomial(link = "logit"),data = df.total%>%filter(ANO_ELEICAO==2010))
logit2014 <- glm(modeloanos, family = binomial(link = "logit"),data = df.total%>%filter(ANO_ELEICAO==2014))
logit2018 <- glm(modeloanos, family = binomial(link = "logit"),data = df.total%>%filter(ANO_ELEICAO==2018))
#resultados
stargazer(logit2002,logit2006,logit2010,logit2014,logit2018,type='text',column.labels = c("2002","2006","2010","2014","2018"))
#efeitos marginais
modelsummary(list("2002"=margins(logit2002),"2006"=margins(logit2006),"2010"=margins(logit2010),"2014"=margins(logit2014),"2018"=margins(logit2018)))

#calculando o pseudo R2
PseudoR2(logit2002, c("McFadden", "Nagel"))
PseudoR2(logit2006, c("McFadden", "Nagel"))
PseudoR2(logit2010, c("McFadden", "Nagel"))
PseudoR2(logit2014, c("McFadden", "Nagel"))
PseudoR2(logit2018, c("McFadden", "Nagel"))


########## EFEITOS MARGINAIS ANO ########################


#efeitos marginais 2002 
modelsummary(list("Logit"=margins(logit2002),"Probit"=margins(probit2002),"Linear"=margins(linear2002)))
#efeitos marginais 2006
modelsummary(list("Logit"=margins(logit2006),"Probit"=margins(probit2006),"Linear"=margins(linear2006)))
#efeitos marginais 2010
modelsummary(list("Logit"=margins(logit2010),"Probit"=margins(probit2010),"Linear"=margins(linear2010)))
#efeitos marginais 2014
modelsummary(list("Logit"=margins(logit2014),"Probit"=margins(probit2014),"Linear"=margins(linear2014)))
#efeitos marginais 2018
modelsummary(list("Logit"=margins(logit2018),"Probit"=margins(probit2018),"Linear"=margins(linear2018)))


########## MODELO PPI ########################

#mudando nome da dummy para ppi
names(df.total)[names(df.total) == 'RACA_DUMMY'] <- 'PPI'

#formula
modeloppi<-eleito ~ genero + NR_IDADE_DATA_POSSE + MEDIA_DESPESA_PARTIDO + IDEO_IMPUTED + casado + PPI + d2014

#modelo linear ppi
lmppi <- lm(modeloppi,data=df.total%>%filter(ANO_ELEICAO>2013))

#probit ppi
probitppi <- glm(modeloppi, family = binomial(link = "probit"),data = df.total%>%filter(ANO_ELEICAO>2013))

#logit ppi
logitppi <- glm(modeloppi, family = binomial(link = "logit"),data = df.total%>%filter(ANO_ELEICAO>2013))

#resultados dos modelos
stargazer(lmppi,probitppi,logitppi,type='text')
#efeitos marginais
modelsummary(list('MPL'=margins(lmppi),'Probit'=margins(probitppi),'Logit'=margins(logitppi)))

