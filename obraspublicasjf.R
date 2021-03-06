library(readxl)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)
library(plm)
library(haven)
library(tidyr)
library(reshape2)
library(geobr)
library(xlsx)
library(psych)
library(readr)
library(treemap)
library(shiny)
#OBRAS P�BLICAS


#criando a base

segsem2020 <- read_excel("C:/Users/Matheus/Desktop/jotaefe/segsemestre2020.xlsx", 
                               col_types = c("text", "numeric", "text", 
                                                  "date", "date", "numeric", "numeric", 
                                             "text"))%>%
  #tirando valores sem identifica��o
  filter(Objeto != is.na(Objeto), `Valor total` != is.na(`Valor total`))%>%
  #gerando dummies para obras fora do prazo, atrasadas pela execu��o financeira e concluidas
  mutate(concluida = case_when(`Situa��o atual da obra` == 'Conclu�da' ~1),
         foradoprazo = case_when(`Data prevista para t�rmino ou prazo de execu��o` < '2020-09-08'
         & `Situa��o atual da obra` != 'Conclu�da' ~1))%>%
  replace_na(list(foradoprazo =0 ,concluida = 0, atrasada = 0))%>%
  mutate(andamento = case_when(concluida == 0 & foradoprazo ==0 ~ 1))%>%
  replace_na(list(andamento =0))%>%
  mutate(obra=1)


##estat�sticas


somatorio <- segsem2020%>%
  group_by(obra)%>%
  summarize(andamento = sum(andamento), concluida = sum(concluida), foradoprazo = sum(foradoprazo), obra = sum(obra))%>%
  ungroup()%>%
  mutate(porc_andamento = (andamento/obra)*100,
         porc_concluida = (concluida/obra)*100,
         porc_foradoprazo = (foradoprazo/obra)*100)

situacao<- c('Em Andamento: 68,9%', "Conclu�da: 6,8%", "Fora do Prazo: 24,1%")


porcentagem <- c(somatorio$porc_andamento,
           somatorio$porc_concluida,
           somatorio$porc_foradoprazo)

treemapds <- tibble(situacao,porcentagem)

# estreiando treemap

treemap(treemapds,
        index="situacao",
        vSize="porcentagem",
        type="index",
        title="Obras P�blicas em Juiz de Fora",                      # Customize your title
        fontsize.title=15, 
        fontsize.labels=16
)

#grafico de colunas com cada um dos casos 

situacao<- c('Em Andamento: 68,9%', "Conclu�da: 6,8%", "Fora do Prazo: 24,1%")


porcentagem <- c(somatorio$porc_andamento,
                 somatorio$porc_concluida,
                 somatorio$porc_foradoprazo)

treemapds <- tibble(situacao,porcentagem)%>%
  arrange(porcentagem)%>%
  mutate(bem = if_else(situacao == "Conclu�da: 6,8%","sim","n�o"))

ggplot(treemapds, aes(x= reorder(situacao, porcentagem) , y=porcentagem , group=1, fill= bem)) +
  geom_col()  + 
    labs(x = "Situa��o da Obra", y = "Porcentagem de Obras ",
         title = "Como est�o as obras em JF") + theme_classic() + theme(axis.line = element_line(linetype = "solid"), 
    axis.ticks = element_line(size = 1.2)) + theme(legend.position = "none")


#execu��o financeira dessas obras a�?


emandamento <- segsem2020%>%
  filter(andamento==1)%>%
  mutate(nemcomecou = if_else(`Percentual de execu��o financeira` < 0.0001,1,0),
         jacomecou = if_else(nemcomecou == 0,1,0))%>%
  

colapsadoemandamento <- emandamento%>%
  summarize(nemcomecou = sum(nemcomecou), jacomecou = sum(jacomecou), obra =sum(obra))%>%
  mutate(p_nem = (nemcomecou/obra)*100 , p_ja = (jacomecou/obra)*100)

porc <- c(30, 70)
situ <- c("Nem come�ou", "J� come�ou")
marca <- c('n�o', 'sim')

execobras <- tibble(situ, porc, marca)


ggplot(execobras, aes(x= reorder(situ, porc) , y=porc, group=1, fill= marca)) +
  geom_col()  + 
  labs(x = "Situa��o das Obras em Andamento", y = "Porcentagem de Obras nessa Situa��o",
       title = "Como est�o as obras em andamento") + theme_classic() + theme(axis.line = element_line(linetype = "solid"), 
                                                                      axis.ticks = element_line(size = 1.2)) + theme(legend.position = "none")



#correlacionando data de entrega e percentual de execu��o financeira
ggplot(emandamento, aes(x=`Data prevista para t�rmino ou prazo de execu��o` 
                        , y=`Percentual de execu��o financeira`, group=1)) +
  geom_point(size = 6, position = 'jitter')  + 
  labs(x = "Prazo de conclus�o", y = "Percentual de Execu��o Financeira",
       title = "Execu��o financeira e prazo de conclus�o das obras em andamento") + theme_classic() + theme(axis.line = element_line(linetype = "solid"), 
                                                                             axis.ticks = element_line(size = 1.2)) + theme(legend.position = "none")


#
ggplot(emandamento, aes(x=`Valor total`, y=`Percentual de execu��o financeira`, group=1)) +
  geom_point(size = 6, position = 'jitter')  + 
labs(x = "Valor Total da Obra", y = "Percentual de Execu��o Financeira",
  title = "Execu��o financeira e valor das obras em andamento") +
  theme_classic() + 
  theme(axis.line = element_line(linetype = "solid"), axis.ticks = element_line(size = 1.2))
+ theme(legend.position = "none")





