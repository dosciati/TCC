#PROJETO TCC
#Análise e classificação de empresas usando balanço patrimonial.
#André Forlin Dosciati
#10/12/2022
#MBA USP / ESALQ


# Load libraries for data manipulation and visualization
install.packages("gbm")
install.packages("adabag")
install.packages("doParallel")
library(doParallel)
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret)
library(gbm)
library(adabag)
library(rpart)
library(rpart.plot)
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
set.seed(12)
amostra <- sample(1:nrow(df), nrow(df) * 0.7, replace = FALSE)
dados_treino <- df_nova_1[amostra, ]
dados_treino$classificação <- as.factor(dados_treino$classificação)
dados_teste <- df_nova_1[-amostra, ]
dados_teste$classificação <- as.factor(dados_teste$classificação)
# mfinal -> total de iterações coeflearn -> fórmula para o amount of say;
# 'Breiman' a usual 1/2ln((1-err)/err).
doParallel::registerDoParallel()
mod_adaboost <- boosting(classificação ~ ., data = dados_treino, boos = TRUE, mfinal = 1, 
                         coeflearn = "Breiman") %dopar% {
doParallel::stopImplicitCluster()}
#arvore de decisão
pruneControl = rpart.control(minsplit = 15, minbucket = 5)
mod_arvores = rpart(classificação ~ ., data = dados_treino, control = pruneControl)
# Visualização da árvore
prp(mod_arvores)                   

classificacoes_arvores <- predict(mod_arvores, dados_teste[, -1])
classificacoes_arvores <- as.data.frame(classificacoes_arvores)
classificacoes_arvores["class"] <- ifelse(classificacoes_arvores >= 0.5, 1, 0)  
#matriz de confusão
mc_arvores <- confusionMatrix(as.factor(classificacoes_arvores$class[,2]), 
                              as.factor(dados_teste[,1]), 
                              positive = "1", 
                              mode = "prec_recall")
mc_arvores$table
    