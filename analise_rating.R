#PROJETO TCC
#Análise e classificação de empresas usando balanço patrimonial.
#André Forlin Dosciati
#10/12/2022
#MBA USP / ESALQ


# Load libraries for data manipulation and visualization
install.packages("gbm")
install.packages("adabag")
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret)
library(gbm)
library(adabag)
# Load the data into a dataframe
df <- read_excel("base de dados/archive/Pasta11.xlsx", 
                      col_types = c("text", "text", "text", 
                                     "text", "text", "text", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric"))7
head(df)
names(df)
glimpse(df)
str(df)
df$classificação
df$setor
df$`ebit por receita`
df_nova<-df[,-c(2,3,4,5,6)]#selecionar colunas que ficam.
prop.table(table(df$classificação))
#Troca das variaveis 
#classificação=="A", 1),
#classificação=="AA", 2),
#classificação=="AAA", 3),
#classificação=="B", 4),
#classificação=="BB", 5),
#classificação=="BBB", 6),
#classificação=="C", 7),
#classificação=="CC", 8),
#classificação=="CCC", 9),
#classificação=="D", 10))
df_nova_1 <- mutate(df_nova, 
                        classificação = replace(classificação, classificação=="A", 1),
                        classificação = replace(classificação, classificação=="AA", 2),
                        classificação = replace(classificação, classificação=="AAA", 3),
                        classificação = replace(classificação, classificação=="B", 4),
                        classificação = replace(classificação, classificação=="BB", 5),
                        classificação = replace(classificação, classificação=="BBB", 6),
                        classificação = replace(classificação, classificação=="C", 7),
                        classificação = replace(classificação, classificação=="CC", 8),
                        classificação = replace(classificação, classificação=="CCC", 9),
                        classificação = replace(classificação, classificação=="D", 10))
df$classificação
#teste de algoritmo - preparando as amostras de treino.
set.seed(16031976)
amostra <- sample(1:nrow(df), nrow(df) * 0.7, replace = FALSE)
dados_treino <- df[amostra, ]
dados_treino$classificação <- as.factor(dados_treino$classificação)
dados_teste <- df[-amostra, ]
dados_teste$classificação <- as.factor(dados_teste$classificação)
# mfinal -> total de iterações coeflearn -> fórmula para o amount of say;
# 'Breiman' a usual 1/2ln((1-err)/err).
mod_adaboost <- boosting(classificação ~ ., data = dados_treino, boos = TRUE, mfinal = 30, 
                         coeflearn = "Breiman")
