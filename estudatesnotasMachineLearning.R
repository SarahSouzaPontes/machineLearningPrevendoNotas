getwd()
# Regressão Linear
# Definição do Problema: Prever as notas dos alunos com base em diversas métricas
#BANCO DE MATEMATICA E LINGUA PORTUGUES
#TARGET G3 TEM RELACAO COM G2 E G1
#G1 E G2 TESTE E G3 PROVA FINAL
# HÁ DADOS DE FALTA NO BANCO PODE SER IMPORTANTE AO MODELO

# Carregando o dataset
df <- read.csv2('estudantes.csv')
View(df)

# Explorando os dados
summary(df)

#TIPO DE DADOS
str(df)

#INVESTIGANDO MISSING
any(is.na(df))
# NÃO HÁ DADOS FALTANTES

# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("dplyr")
library(ggplot2)
library(ggthemes)
library(dplyr)

# Obtendo apenas as colunas numéricas
colunas_numericas <- sapply(df, is.numeric)
#QUAIS SAO AS COLUNAS NUMERICAS?
colunas_numericas

# Filtrando as colunas numéricas para correlação
#TODAS AS LINHAS E COLUNAS APENAS NUMERAS
data_cor <- cor(df[,colunas_numericas])
#DATA APENAS COM COLUNAS NUMERICAS
data_cor
head(data_cor)

# Pacotes para visualizar a análise de correlação
# install.packages('corrgram')
# install.packages('corrplot')
library(corrplot)
library(corrgram)

# Criando um corrplot
corrplot(data_cor, method = 'color')
#observações: escala 1 alta correlacao positiva, 0 sem 
#correlacao, e -1 correlacao negativa
#azul ralacao da variavel com ela mesma
#tendendo azul alta correlaçãO

# Criando um corrgram
corrgram(df)

corrgram(df, order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt)

#CARREGANDO GGPLOT
library(ggplot2)

# Criando um histograma
ggplot(df, aes(x = G3)) + 
  geom_histogram(bins = 20, 
                 alpha = 0.5, fill = 'blue') + 
  theme_minimal()
#COMPRENDENDO TARGET (G3) DISTRIBUIÇÃ0

# Treinando e Interpretando o Modelo
# Import Library
install.packages("caTools")
library(caTools)

# Criando as amostras de forma randômica
set.seed(101) 
?sample.split
#DIVIDINDO ENTRE DADOS DE TREINO E TESTE
#70% PARA DADO DE TREINO E 30% TESTE
amostra <- sample.split(df$age, SplitRatio = 0.70)

# ***** Treinamos nosso modelo nos dados de treino *****
# *****   Fazemos as predições nos dados de teste  *****

# Criando dados de treino - 70% dos dados
treino = subset(df, amostra == TRUE)

# Criando dados de teste - 30% dos dados
teste = subset(df, amostra == FALSE)
#-------------------------------#MODELO TREINADO
# Gerando o Modelo (Usando todos os atributos)
#PONTO TODAS DEMAIS VARIAVEIS
modelo_v1 <- lm(G3 ~ ., treino)
#TREINO DATASET
#G2 E G1 PREDITORAS
modelo_v2 <- lm(G3 ~ G2 + G1, treino)

#FALTAS COMO PREDITORA
modelo_v3 <- lm(G3 ~ absences, treino)
#MEDU = EDUCAÇÃO DA MÃE
modelo_v4 <- lm(G3 ~ Medu, treino)

#________________________________________________ANALISANDO MODELO
# Interpretando o Modelo
summary(modelo_v1) # 0.86


#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      -1.329568   2.474569  -0.537 0.591574    
#schoolMS          0.838581   0.470545   1.782 0.076016 .  
#sexM              0.034883   0.275586   0.127 0.899382    
#age              -0.214994   0.119579  -1.798 0.073472 .  
#addressU          0.067190   0.326035   0.206 0.836905    
#famsizeLE3       -0.111068   0.283228  -0.392 0.695302    
#PstatusT         -0.153653   0.401679  -0.383 0.702417    
#Medu              0.279949   0.171111   1.636 0.103164    
#Fedu             -0.221275   0.151103  -1.464 0.144422    
#Mjobhealth        0.002065   0.610532   0.003 0.997304    
#Mjobother         0.509947   0.403195   1.265 0.207209    
#Mjobservices      0.475476   0.435332   1.092 0.275857    
#Mjobteacher       0.285345   0.550640   0.518 0.604802    
#Fjobhealth        0.433172   0.774191   0.560 0.576343    
#Fjobother        -0.296792   0.577217  -0.514 0.607611    
#Fjobservices     -0.311595   0.593628  -0.525 0.600148    
#Fjobteacher      -0.321205   0.712695  -0.451 0.652628    
#reasonhome       -0.431435   0.319907  -1.349 0.178755    
#reasonother       0.159612   0.454480   0.351 0.725755    
#reasonreputation -0.051845   0.317894  -0.163 0.870589    
#guardianmother    0.267462   0.311371   0.859 0.391226    
#guardianother    -0.157335   0.554872  -0.284 0.777003    
#traveltime        0.274301   0.197865   1.386 0.166968    
#studytime        -0.140650   0.155149  -0.907 0.365577    
#failures         -0.185333   0.211040  -0.878 0.380739    
#schoolsupyes      0.562716   0.379303   1.484 0.139268    
#famsupyes         0.369402   0.268848   1.374 0.170745    
#paidyes           0.060643   0.270971   0.224 0.823107    
#activitiesyes    -0.286006   0.247519  -1.155 0.249063    
#nurseryyes       -0.426858   0.315064  -1.355 0.176774    
#higheryes         0.503353   0.677346   0.743 0.458148    
#internetyes      -0.097405   0.331040  -0.294 0.768834    
#romanticyes      -0.243837   0.268414  -0.908 0.364577    
#famrel            0.494479   0.136663   3.618 0.000363 ***
#  freetime         -0.139869   0.138297  -1.011 0.312879    
#goout             0.078871   0.128794   0.612 0.540879    
#Dalc             -0.248633   0.178366  -1.394 0.164651    
#Walc              0.221434   0.139178   1.591 0.112950    
#health            0.027495   0.095100   0.289 0.772748    
#absences          0.060830   0.017915   3.396 0.000804 ***
#  G1                0.162966   0.076956   2.118 0.035255 *  
#  G2                0.994677   0.066450  14.969  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.868 on 235 degrees of freedom
#Multiple R-squared:  0.8616,	Adjusted R-squared:  0.8374 
#F-statistic: 35.68 on 41 and 235 DF,  p-value: < 2.2e-16



#OUTROS MODELOS TREINADOS

summary(modelo_v2) # 0.82
summary(modelo_v3) # 0.0002675
summary(modelo_v4) # 0.06442

#MELHOR MODELO O PRIMEIRO

# Visualizando o Modelo e Fazendo Previsões

# Obtendo os resíduos
res <- residuals(modelo_v1)

# Convertendo o objeto para um dataframe
res <- as.data.frame(res)
head(res)

#res
#1   0.9678451
#5   1.1829980
#6  -1.4096050
#7   0.1125706
#9   0.3814670
#10  0.3221204

# Histograma dos resíduos
ggplot(res, aes(res)) +  
  geom_histogram(fill = 'blue', 
                 alpha = 0.5, 
                 binwidth = 1)

# Plot do Modelo
plot(modelo_v1)

# Fazendo as predições
modelo_v1 <- lm(G3 ~ ., treino)
#CONJUNTO DE TESTE CRIADO ACIMA
prevendo_G3 <- predict(modelo_v1, teste)
#DADOS DE TESTE
prevendo_G3

# Visualizando os valores previstos e observados
resultados <- cbind(prevendo_G3, teste$G3) 
colnames(resultados) <- c('Previsto','Real')
resultados <- as.data.frame(resultados)
resultados
min(resultados)
#VALORES NEGATIVOS - NO MODELO
# NÃO HÁ NOTAS NEGATIVAS
#LOGO, O MODELO NÃO APRENDEU QUE NÃO HÁ NOTA NEGATIVA
#PROBLEMA DE NEGOCIO É DO CIENTISTA DE DADOS


# Tratando os valores negativos
#SE O VALOR FOR NEGATIVO SERÁ SUBSTITUIDO POR ZERO
trata_zero <- function(x){
  if  (x < 0){
    return(0)
  }else{
    return(x)
  }
}

# Aplicando a função para tratar valores negativos em nossa previsão
resultados$Previsto <- sapply(resultados$Previsto, trata_zero)
#SEM DADOS NEGATIVOS
resultados$Previsto
#___________________CALCULANDO O ERRO DO MODELO

# Calculando o erro médio
# Quão distantes seus valores previstos estão dos valores observados
# MSE
mse <- mean((resultados$Real - resultados$Previsto)^2)
print(mse)

# RMSE
rmse <- mse^0.5
rmse

# Calculando R Squared
SSE = sum((resultados$Previsto - resultados$Real)^2)
SST = sum((mean(df$G3) - resultados$Real)^2)

# R-Squared
# Ajuda a avaliar o nível de precisão do nosso modelo. Quanto maior, melhor, sendo 1 o valor ideal.
R2 = 1 - (SSE/SST)
R2






