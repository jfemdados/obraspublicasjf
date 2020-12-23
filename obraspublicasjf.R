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
#OBRAS PÚBLICAS


#criando a base

segsem2020 <- read_excel("C:/Users/Matheus/Desktop/jotaefe/segsemestre2020.xlsx", 
                               col_types = c("text", "numeric", "text", 
                                                  "date", "date", "numeric", "numeric", 
                                             "text"))%>%
  #tirando valores sem identificação
  filter(Objeto != is.na(Objeto), `Valor total` != is.na(`Valor total`))%>%
  #gerando dummies para obras fora do prazo, atrasadas pela execução financeira e concluidas
  mutate(concluida = case_when(`Situação atual da obra` == 'Concluída' ~1),
         foradoprazo = case_when(`Data prevista para término ou prazo de execução` < '2020-09-08'
         & `Situação atual da obra` != 'Concluída' ~1))%>%
  replace_na(list(foradoprazo =0 ,concluida = 0, atrasada = 0))%>%
  mutate(andamento = case_when(concluida == 0 & foradoprazo ==0 ~ 1))%>%
  replace_na(list(andamento =0))%>%
  mutate(obra=1)


##estatísticas


somatorio <- segsem2020%>%
  group_by(obra)%>%
  summarize(andamento = sum(andamento), concluida = sum(concluida), foradoprazo = sum(foradoprazo), obra = sum(obra))%>%
  ungroup()%>%
  mutate(porc_andamento = (andamento/obra)*100,
         porc_concluida = (concluida/obra)*100,
         porc_foradoprazo = (foradoprazo/obra)*100)

situacao<- c('Em Andamento: 68,9%', "Concluída: 6,8%", "Fora do Prazo: 24,1%")


porcentagem <- c(somatorio$porc_andamento,
           somatorio$porc_concluida,
           somatorio$porc_foradoprazo)

treemapds <- tibble(situacao,porcentagem)

# estreiando treemap

treemap(treemapds,
        index="situacao",
        vSize="porcentagem",
        type="index",
        title="Obras Públicas em Juiz de Fora",                      # Customize your title
        fontsize.title=15, 
        fontsize.labels=16
)

#grafico de colunas com cada um dos casos 

situacao<- c('Em Andamento: 68,9%', "Concluída: 6,8%", "Fora do Prazo: 24,1%")


porcentagem <- c(somatorio$porc_andamento,
                 somatorio$porc_concluida,
                 somatorio$porc_foradoprazo)

treemapds <- tibble(situacao,porcentagem)%>%
  arrange(porcentagem)%>%
  mutate(bem = if_else(situacao == "Concluída: 6,8%","sim","não"))

ggplot(treemapds, aes(x= reorder(situacao, porcentagem) , y=porcentagem , group=1, fill= bem)) +
  geom_col()  + 
    labs(x = "Situação da Obra", y = "Porcentagem de Obras ",
         title = "Como estão as obras em JF") + theme_classic() + theme(axis.line = element_line(linetype = "solid"), 
    axis.ticks = element_line(size = 1.2)) + theme(legend.position = "none")


#execução financeira dessas obras aí?


emandamento <- segsem2020%>%
  filter(andamento==1)%>%
  mutate(nemcomecou = if_else(`Percentual de execução financeira` < 0.0001,1,0),
         jacomecou = if_else(nemcomecou == 0,1,0))%>%
  

colapsadoemandamento <- emandamento%>%
  summarize(nemcomecou = sum(nemcomecou), jacomecou = sum(jacomecou), obra =sum(obra))%>%
  mutate(p_nem = (nemcomecou/obra)*100 , p_ja = (jacomecou/obra)*100)

porc <- c(30, 70)
situ <- c("Nem começou", "Já começou")
marca <- c('não', 'sim')

execobras <- tibble(situ, porc, marca)


ggplot(execobras, aes(x= reorder(situ, porc) , y=porc, group=1, fill= marca)) +
  geom_col()  + 
  labs(x = "Situação das Obras em Andamento", y = "Porcentagem de Obras nessa Situação",
       title = "Como estão as obras em andamento") + theme_classic() + theme(axis.line = element_line(linetype = "solid"), 
                                                                      axis.ticks = element_line(size = 1.2)) + theme(legend.position = "none")



#correlacionando data de entrega e percentual de execução financeira
ggplot(emandamento, aes(x=`Data prevista para término ou prazo de execução` 
                        , y=`Percentual de execução financeira`, group=1)) +
  geom_point(size = 6, position = 'jitter')  + 
  labs(x = "Prazo de conclusão", y = "Percentual de Execução Financeira",
       title = "Execução financeira e prazo de conclusão das obras em andamento") + theme_classic() + theme(axis.line = element_line(linetype = "solid"), 
                                                                             axis.ticks = element_line(size = 1.2)) + theme(legend.position = "none")


#
ggplot(emandamento, aes(x=`Valor total`, y=`Percentual de execução financeira`, group=1)) +
  geom_point(size = 6, position = 'jitter')  + 
labs(x = "Valor Total da Obra", y = "Percentual de Execução Financeira",
  title = "Execução financeira e valor das obras em andamento") +
  theme_classic() + 
  theme(axis.line = element_line(linetype = "solid"), axis.ticks = element_line(size = 1.2))
+ theme(legend.position = "none")





