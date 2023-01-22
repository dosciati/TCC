#PROJETO TCC
#Análise e classificacao de empresas usando balanço patrimonial.
#André Forlin Dosciati
#10/12/2022
#MBA USP / ESALQ


# Load libraries for data manipulation and visualization
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret)
library(gbm)
library(rpart)
library(rpart.plot)
library(randomForest)

#Load the data into a dataframe
df<-read_excel("base de dados/archive/Pasta12.xlsx")

#convert all columns except the first one to numeric
df[,-1] <- lapply(df[,-1], as.numeric)
prop.table(table(df$classificacao))
#----------------------------
#Dicionário de riscos
classificacao_dict <- c("A" = "Low Risk",
                        "AA" = "Low Risk",
                        "AAA" = "Lowest Risk",
                        "BBB" = "Medium Risk",
                        "BB" = "High Risk",
                        "B" = "High Risk",
                        "CCC" = "Highest Risk",
                        "CC" = "Highest Risk",
                        "C" = "High Risk",
                        "D" ="In Default")
df$classificacao <- classificacao_dict[match(df$classificacao, names(classificacao_dict))]

#teste de algoritmo - preparando as amostras de treino.
set.seed(12)
amostra <- sample(1:nrow(df), nrow(df) * 0.7, replace = FALSE)
dados_treino <- df[amostra, ]
dados_treino$classificacao <- as.factor(dados_treino$classificacao)
dados_teste <- df[-amostra, ]
dados_teste$classificacao <- as.factor(dados_teste$classificacao)
#Arvore de descisão
pruneControl = rpart.control(minsplit = 1, minbucket = 5)
mod_arvores = rpart(classificacao ~ ., data = dados_treino, control = pruneControl)
prp(mod_arvores)

classificacoes_arvores <- predict(mod_arvores, dados_teste[, -1], type = "class") #updated here

#matriz de confusão
classificacoes_arvores <- factor(classificacoes_arvores, levels = levels(dados_teste$classificacao))
mc_arvores <- confusionMatrix(classificacoes_arvores,
                              as.factor(dados_teste$classificacao),
                              positive = "1", mode = "prec_recall")

mc_arvores$table
#metricas
acuracia_arvores <- mc_arvores$overall["Accuracy"]
precisao_arvores <- mc_arvores$byClass["Precision"]
recall_arvores <- mc_arvores$byClass["Recall"]
f1_arvores <- mc_arvores$byClass["F1"]
metricas_arvores <- c(acuracia_arvores, precisao_arvores, recall_arvores, f1_arvores)
metricas_arvores

#--------------------------
#randon forest
# ntree: número de árvores
#Novo dataframe para executar randonforest
df_rf <- df
#Dicionário de riscos alterando os fatores para numeros
classificacao_dict_rf<- c("Low Risk"= 1,
                          "Low Risk" =2,
                          "Lowest Risk"=3,
                          "Medium Risk"=4,
                          "High Risk"=5,
                          "High Risk"=6,
                          "Highest Risk"=7,
                          "Highest Risk"=8,
                          "High Risk"=9,
                          "In Default"=10)
#aplicando o dicionário na coluna nova
df_rf$classificacao <- classificacao_dict_rf[match(df_rf$classificacao, names(classificacao_dict_rf))]
#dados treino df_rf
set.seed(12)
amostra <- sample(1:nrow(df_rf), nrow(df_rf) * 0.7, replace = FALSE)
dados_treino_rf <- df_rf[amostra, ]
#dados_treino_rf$classificacao <- (dados_treino_rf$classificacao)
dados_teste_rf <- df_rf[-amostra, ]
#dados_teste_rf$classificacao <- as.numeric(dados_teste_rf$classificacao)
###
#ntree: número de árvores
mod_rf <- randomForest(as.factor(classificacao) ~ ., data = dados_treino_rf, ntree = 101)
classificacoes_rf <- predict(mod_rf, dados_teste_rf[, -1])

mc_rf <- confusionMatrix(classificacoes_rf, as.factor(dados_teste_rf[, "classificacao"]), positive = "1", 
                         mode = "prec_recall")
mc_rf$table
