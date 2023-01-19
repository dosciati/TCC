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
library(randomForest)

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

DfN1 <- mutate(df_nova, 
                        classificação = replace(classificação, classificação=="A", 'Low Risk'),
                        classificação = replace(classificação, classificação=="AA", 'Low Risk'),
                        classificação = replace(classificação, classificação=="AAA", 'Lowest Risk'),
                        classificação = replace(classificação, classificação=="B", 'High Risk'),
                        classificação = replace(classificação, classificação=="BB", 'High Risk'),
                        classificação = replace(classificação, classificação=="BBB", 'Medium Risk'),
                        classificação = replace(classificação, classificação=="C", 'Highest Risk'),
                        classificação = replace(classificação, classificação=="CC", 'Highest Risk'),
                        classificação = replace(classificação, classificação=="CCC", 'Highest Risk'),
                       classificação = replace(classificação, classificação=="D", 'In Default'))
DfN1<- rename(DfN1, classificacao = 'classificação')
DfN1<-rename(DfN1, relacaoatual = 'relação atual')
DfN1<-rename(DfN1, relacaixa = 'relação do caixa')
DfN1<-rename(DfN1, proprapida = 'proporção rápida')
DfN1<-rename(DfN1, diaVenPend = 'Dias de vendas pendentes')
DfN1<-rename(DfN1, marLucLiq = 'Margem de lucro liquido')
DfN1<-rename(DfN1, margEbit = 'Margem de lucro antes dos impostos')
DfN1<-rename(DfN1, margLucBruto = 'Margem de lucro bruto')
DfN1<-rename(DfN1, margLucOper = 'Margem de lucro operacional')
DfN1<-rename(DfN1, relacaixa = 'relação do caixa')
DfN1<-rename(DfN1, retAtivos = 'Retorno sobre ativos')
DfN1<-rename(DfN1, retCapitalEmpregado = 'Retorno sobre o capital empregado')
DfN1<-rename(DfN1, retPatLiquido = 'Retorno sobre o patrimonio liquido')
DfN1<-rename(DfN1, giroAtivosFixos = 'giro de ativos fixos')
DfN1<-rename(DfN1, IndEdivi = 'Indice de endividamento')
DfN1<-rename(DfN1, IndDivida = 'Indice de divida')
DfN1<-rename(DfN1, TaxImpEfe = 'Taxa de imposto efetiva')
DfN1<-rename(DfN1, fluxocaixa = 'Fluxo de Caixa Livre Índice de Fluxo de Caixa Operacional')
DfN1<-rename(DfN1, rotAtivo = 'Rotatividade do ativo')
DfN1<-rename(DfN1, fluCaixaAcao = 'fluxo de caixa operacional por ação')
DfN1<-rename(DfN1, dinhPAcao = 'Dinheiro por ação')
DfN1<-rename(DfN1, fluCaixaLivreAcao = 'Fluxo de caixa livre por ação')
DfN1<-rename(DfN1, multPaEmpresa = 'Multiplicador do patrimonio da empresa')
DfN1<-rename(DfN1, EbitPorReceita = 'ebit por receita')
DfN1<-rename(DfN1, valorEmpMultiplo = 'Valor da empresa Múltiplo')
DfN1<-rename(DfN1, contaPagarVolNeg = 'contas a pagar Volume de negócios')
DfN1<-rename(DfN1, indVenFluxCaixaOpera = 'Índice de vendas de fluxo de caixa operacional')

#teste de algoritmo - preparando as amostras de treino.
set.seed(12)
amostra <- sample(1:nrow(DfN1), nrow(DfN1) * 0.7, replace = FALSE)
dados_treino <- DfN1[amostra, ]
dados_treino$classificacao <- as.factor(dados_treino$classificacao)
dados_teste <- DfN1[-amostra, ]
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

# Visualização da árvore de decisão

prp(mod_arvores)                   
mod_arvores
#matriz de confusão
mc_arvores <- confusionMatrix(as.factor(classificacoes_arvores$class[,1]), 
as.factor(dados_teste[,1]), positive = "1", mode = "prec_recall")

mc_arvores$table
classificacoes_arvores  

#randon forest
#ntree: número de árvores

mod_rf <- randomForest(as.factor(classificacao) ~ ., data = dados_treino, ntree = 101)
classificacoes_rf <- predict(mod_rf, dados_teste[, -1])
mc_rf <- confusionMatrix(classificacoes_rf, as.factor(dados_teste[, 1]), positive = "1", 
                         mode = "prec_recall")
mc_rf$table

    
