#Configura o diretório de trabalho
getwd()
setwd("C:/Users/gusta/Desktop/P_Projects/TemporalSeries/Bitcoin")

#Carrega as bibliotecas necessarias
library(lubridate)
library(prophet)
library(ggplot2)
library(scales)
library(MLmetrics)

#Carrega os dados históricos dos preços da criptomoeda de 21/05/2018 a 21/05/2020
BitcoinData <- read.csv("BitcoinData.csv")

#Renomeia as colunas
names(BitcoinData)[1] <- "ds"
names(BitcoinData)[2] <- "Ultimo"
names(BitcoinData)[3] <- "Abertura"
names(BitcoinData)[4] <- "Maxima"
names(BitcoinData)[5] <- "Minima"
names(BitcoinData)[6] <- "Volume"
names(BitcoinData)[7] <- "Variacao_percentual"

#Remove os pontos das colunas Ultimo, Abertura, Maxima e Minima
BitcoinData$Ultimo <- gsub("[.]", "", BitcoinData$Ultimo)
BitcoinData$Abertura <- gsub("[.]", "", BitcoinData$Abertura)
BitcoinData$Maxima <- gsub("[.]", "", BitcoinData$Maxima)
BitcoinData$Minima <- gsub("[.]", "", BitcoinData$Minima)

#Substitui a vírgula das colunas Ultimo, Abertura, Maxima e Minima por pontos
BitcoinData$Ultimo <- gsub("[,]", ".", BitcoinData$Ultimo)
BitcoinData$Abertura <- gsub("[,]", ".", BitcoinData$Abertura)
BitcoinData$Maxima <- gsub("[,]", ".", BitcoinData$Maxima)
BitcoinData$Minima <- gsub("[,]", ".", BitcoinData$Minima)

#Converte as colunas Ultimo, Abertura, Maxima e Minima para formato numérico 
BitcoinData$Ultimo <- as.numeric(BitcoinData$Ultimo)
BitcoinData$Abertura <- as.numeric(BitcoinData$Abertura)
BitcoinData$Maxima <- as.numeric(BitcoinData$Maxima)
BitcoinData$Minima <- as.numeric(BitcoinData$Minima)

#Substitui os pontos da coluna ds por hífens para posterior conversão em formato Date
BitcoinData$ds <- gsub("[.]", "-", BitcoinData$ds)

#Converte a coluna ds para tipo Date
BitcoinData$ds <- as.Date(BitcoinData$ds, "%d-%m-%Y")

#Cria a coluna Price como uma média entre o valor mínimo e máximo para cada dia
BitcoinData$Price <- (BitcoinData$Maxima + BitcoinData$Minima)/2

#plota a série temporal
ggplot(data = BitcoinData, aes(x = ds, y = Price, group = 1, size = 1)) +
  geom_line(color = "#60AFCB", size = 1) + 
    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
      scale_x_date(labels = date_format("%m-%Y")) +
        labs(title = "Cotação média do Bitcoin de maio/2018 a maio/2020 em dólares",
             x = "Período",
             y = "Preço médio da unidade de Bitcoin")

#Cria dataframe para construção do modelo prophet
df <- data.frame(BitcoinData$ds, BitcoinData$Price)

#Renomeia as colunas para deixá-las de acordo com o padrão exigido pela biblioteca prophet
df <- df %>% 
           rename(
                  ds = BitcoinData.ds,
                  y = BitcoinData.Price)

#Cria modelo prophet
model <- prophet()

#Treina o modelo 
model <- fit.prophet(model, df)

#Cria o dataset para as previsões do modelo
df_for_predictions <- make_future_dataframe(model, periods = 180, freq = "day")

#Faz as predições 
forecast_prophet_model <- predict(model, df_for_predictions)

#Converte a coluna ds para Date
forecast_prophet_model$ds <- as.Date(forecast_prophet_model$ds, "%Y-%m-%d")

#Calcula o erro em percentual para o dataset de treino
?MAPE()
MAPE(forecast_prophet_model[1:732, "yhat"], df[, "y"])

#plota as previsões
ggplot(data = forecast_prophet_model[732:912,], aes(x = ds, y = yhat, group = 1, size = 1)) +
  geom_line(color = "#60AFCB", size = 1) + 
    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
      scale_x_date(labels = date_format("%m-%Y")) +
        labs(title = "Previsão da Cotação média do Bitcoin para o período maio-novembro/2020",
             x = "Período",
             y = "Previsão preço médio da unidade de Bitcoin (dólares)")


