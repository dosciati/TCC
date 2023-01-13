#PROJETO TCC
#Análise e classificação de empresas usando balanço patrimonial.
#André Forlin Dosciati
#10/12/2022
#MBA USP / ESALQ


# Load libraries for data manipulation and visualization
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
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
                                     "numeric", "numeric", "numeric"))
str(df)
df$classificação
df$setor
df$`ebit por receita`
prop.table(table(df$setor))

#teste de algoritmo - preparando as amostras de treino.
set.seed(1234)
amostra <- sample(1:nrow(df), nrow(df) * 0.7, replace = FALSE)
dados_treino <- df[amostra, ]
dados_treino$classificação <- as.factor(dados_treino$classificação)
dados_teste <- df[-amostra, ]
dados_teste$classificação <- as.factor(dados_teste$classificação)

