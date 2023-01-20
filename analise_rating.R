#PROJETO TCC
#Análise e classificacao de empresas usando balanço patrimonial.
#André Forlin Dosciati
#10/12/2022
#MBA USP / ESALQ


# Load libraries for data manipulation and visualization
install.packages("gbm")
install.packages("adabag")
install.packages("doParallel")
#library(doParallel)
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
#library(caret)
library(gbm)
#library(adabag)
library(rpart)
library(rpart.plot)
library(randomForest)

# Load the data into a dataframe
df<-read_excel("base de dados/archive/Pasta12.xlsx")


# convert all columns except the first one to numeric
df[,-1] <- lapply(df[,-1], as.numeric)
prop.table(table(df$classificacao))
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
# mfinal -> total de iterações coeflearn -> fórmula para o amount of say;
# 'Breiman' a usual 1/2ln((1-err)/err).
#doParallel::registerDoParallel()
#mod_adaboost <- boosting(classificacao ~ ., data = dados_treino, boos = TRUE, mfinal = 30, 
#             coeflearn = "Breiman") #%dopar% {
#doParallel::stopImplicitCluster()}

#arvore de decisão

pruneControl = rpart.control(minsplit = 1, minbucket = 5)
mod_arvores = rpart(classificacao ~ ., data = dados_treino, control = pruneControl)

classificacoes_arvores <- predict(mod_arvores, dados_teste[, -1])
classificacoes_arvores <- as.data.frame(classificacoes_arvores)
classificacoes_arvores["classificacao"] <- ifelse(classificacoes_arvores >= 0.5, 1, 0)

# Visualização da árvore de decisão

prp(mod_arvores)                   
mod_arvores
#matriz de confusão
mc_arvores <- confusionMatrix(as.factor(classificacoes_arvores$classificacao[,2]), 
                              as.factor(dados_treino[,1]), positive = "1", mode = "prec_recall")

mc_arvores$table
classificacoes_arvores  

#randon forest
#ntree: número de árvores

mod_rf <- randomForest(as.factor(classificacao) ~ ., data = dados_treino, ntree = 101)

classificacoes_rf <- predict(mod_rf, dados_treino[, -1])
mc_rf <- confusionMatrix(classificacoes_rf, as.factor(dados_treino[, 1]), positive = "1", 
                         mode = "prec_recall")
mc_rf$table
dim(classificacoes_rf)
dim(dados_teste)