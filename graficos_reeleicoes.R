install.packages('patchwork')
library(dplyr)
library(tidyverse)
library(dplyr)
library(data.table)
library(psych)
library(Hmisc)
library(Weighted.Desc.Stat)
library(weights)
library(plotly)
library(wesanderson)
library(RColorBrewer)
library(ggplot2)
library(lessR)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(patchwork)


setwd("C:/Users/jorge/Desktop/Laboratório de Econometria/Data Base Trabalho em Grupo/Base Usada TSE")

df_tse <- read.csv("bases_eleicao.csv",header = TRUE,sep = "," ,na.strings = c("NA","N/A","", " "),skip = 0L, stringsAsFactors = FALSE, strip.white = TRUE)

#Criando as Dummies 2002
reeleicao02<-function(ano){df_tse%>%filter(ANO_ELEICAO==2002)%>%
  ggplot(aes(x=ST_REELEICAO,fill=ST_REELEICAO))+
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=percent,limits=c(0,1),expand = c(0,0))+
  scale_fill_manual(values = c("S"="light blue","N"="light green")) +
  labs(title = "2002",x="",y="",fill='')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))}
#Criando as Dummies 2006
reeleicao06<-function(ano){df_tse%>%filter(ANO_ELEICAO==2006)%>%
  ggplot(aes(x=ST_REELEICAO,fill=ST_REELEICAO))+
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=percent,limits=c(0,1),expand = c(0,0))+
  scale_fill_manual(values = c("S"="light blue","N"="light green")) +
  labs(title = "2006",x="",y="",fill='')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))}

#Criando as Dummies 2010
reeleicao10<-function(ano){df_tse%>%filter(ANO_ELEICAO==2010)%>%
  ggplot(aes(x=ST_REELEICAO,fill=ST_REELEICAO))+
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=percent,limits=c(0,1),expand = c(0,0))+
  scale_fill_manual(values = c("S"="light blue","N"="light green")) +
  labs(title = "2010",x="",y="",fill='')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))}

#Criando as Dummies 2014
reeleicao14<-function(ano){df_tse%>%filter(ANO_ELEICAO==2014)%>%
  ggplot(aes(x=ST_REELEICAO,fill=ST_REELEICAO))+
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=percent,limits=c(0,1),expand = c(0,0))+
  scale_fill_manual(values = c("S"="light blue","N"="light green")) +
  labs(title = "2014",x="",y="",fill='')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))}

#Criando as Dummies 2018
reeleicao18<-function(ano){df_tse%>%filter(ANO_ELEICAO==2018)%>%
  ggplot(aes(x=ST_REELEICAO,fill=ST_REELEICAO))+
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=percent,limits=c(0,1),expand = c(0,0))+
  scale_fill_manual(values = c("S"="light blue","N"="light green")) +
  labs(title = "2018",x="",y="",fill='')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))}
#Criando proporção de Eleitos por Reeleição
reeleicao02(2002)+reeleicao06(2006)+reeleicao10(2010)+reeleicao14(2014)+reeleicao18(2018)+plot_annotation(title = "% de Candidados que buscaram reeleição e lograram")

#Criando proporção de Eleitos por Reeleição

df_tse$eleito <- ifelse(df_tse$DS_SIT_TOT_TURNO=='ELEITO',"Eleito", ifelse(df_tse$DS_SIT_TOT_TURNO=='MÉDIA',"Eleito", ifelse(df_tse$DS_SIT_TOT_TURNO=='ELEITO POR QP',"Eleito", ifelse(df_tse$DS_SIT_TOT_TURNO=='ELEITO POR MÉDIA',"Eleito","Não Eleito"))))


#2002
reeleitos02<-function(reeleicao){df_tse%>%filter(ANO_ELEICAO==2002 & ST_REELEICAO=="S")%>%
  ggplot(aes(x=as.factor(eleito),fill=as.factor(eleito)))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels=percent,limits = c(0,1), expand = c(0,0))+
  scale_fill_manual(values = c("Eleito"="light blue","Não Eleito"="light green")) +
  labs(title = "2002",x="",y="",fill='')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))}

#2006
reeleitos06<-function(reeleicao){df_tse%>%filter(ANO_ELEICAO==2006 & ST_REELEICAO=="S")%>%
  ggplot(aes(x=as.factor(eleito),fill=as.factor(eleito)))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels=percent,limits = c(0,1), expand = c(0,0))+
  scale_fill_manual(values = c("Eleito"="light blue","Não Eleito"="light green")) +
  labs(title = "2006",x="",y="",fill='')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))}

#2010
reeleitos10<-function(reeleicao){df_tse%>%filter(ANO_ELEICAO==2010 & ST_REELEICAO=="S")%>%
  ggplot(aes(x=as.factor(eleito),fill=as.factor(eleito)))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels=percent,limits = c(0,1), expand = c(0,0))+
  scale_fill_manual(values = c("Eleito"="light blue","Não Eleito"="light green")) +
  labs(title = "2010",x="",y="",fill='')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))}

#2014
reeleitos14<-function(reeleicao){df_tse%>%filter(ANO_ELEICAO==2014 & ST_REELEICAO=="S")%>%
  ggplot(aes(x=as.factor(eleito),fill=as.factor(eleito)))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels=percent,limits = c(0,1), expand = c(0,0))+
  scale_fill_manual(values = c("Eleito"="light blue","Não Eleito"="light green")) +
  labs(title = "2014",x="",y="",fill='')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))}

#2018
reeleitos18<-function(reeleicao){df_tse%>%filter(ANO_ELEICAO==2018 & ST_REELEICAO=="S")%>%
  ggplot(aes(x=as.factor(eleito),fill=as.factor(eleito)))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels=percent,limits = c(0,1), expand = c(0,0))+
  scale_fill_manual(values = c("Eleito"="light blue","Não Eleito"="light green")) +
  labs(title = "2018",x="",y="",fill='')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))}
#Salvando os Gráficos em uma só imágem
reeleitos02(2002)+reeleitos06(2006)+reeleitos10(2010)+reeleitos14(2014)+reeleitos18(2018)+plot_annotation(title = "% de Pessoas que lograram suas Reeleições")

