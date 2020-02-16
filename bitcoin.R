install.packages('coindeskr')
install.packages('forecast')
install.packages("timeSeries")
install.packages("seastests")
install.packages("seasonal")
library(seasonal)
library(seastests)
library(timeSeries)
library(coindeskr)
library(forecast)
library(tseries)
library(ggplot2)
bitcoin2=get_historic_price(currency = "USD", start = Sys.Date() - 3470, end = Sys.Date() - 13)
bitcoin = ts(bitcoin2, frequency = 365, start = c(2010,225), end = c(2020,32))
bitcoin3=get_historic_price(currency = "USD", start = Sys.Date() - 12, end = Sys.Date() - 1)
bitcoin.atual=ts(bitcoin3)
is.ts(bitcoin)
head(bitcoin)
tail(bitcoin)
start(bitcoin)
end(bitcoin)
frequency(bitcoin)
sum(is.na(bitcoin))
tsbitcoin=ts(bitcoin, frequency = 365)
dbitcoin=decompose(tsbitcoin, "multiplicative")
plot(dbitcoin)
cycle(bitcoin)


## gráficos iniciais
ts.plot(bitcoin, main="Série Temporal Bitcoin (15/08/2010 - 31/01/2020)", xlab="Tempo (ano)", ylab="Preço Bitcoin em US$")

r = ts(diff(log(bitcoin),1))
ra = na.omit(r)

plot.ts(r, main="Diferencial do Log do Preço do Bitcoin", xlab="Tempo (dias)", ylab="Dif do Log do Bitcoin")

plot.ts(log(bitcoin), main="Log da série temporal inteira", xlab="Tempo (ano)", ylab="Log do preço diário do Bitcoin")

windows()
par(mfrow = c(2,1))

acf(r, main="ACF diff log preço do Bitcoin", ylab="")
acf(r^2, main="ACF do diff log do preço do Bitcoin elevado ao quadrado", ylab="")
pacf(r, main="PACF diff log preço do Bitcoin", ylab="")
Box.test(ra^2,lag=20)
acf(diff(bitcoin,2))

## boxcox para tirar a variancia
lambda=BoxCox.lambda(bitcoin)
bitcoin.bc=BoxCox(bitcoin,lambda = lambda)

## estatistica descritiva
summary(bitcoin)
var(bitcoin)
sd(bitcoin)
head(bitcoin)
ggtsdisplay(bitcoin)
ggtsdisplay(bitcoin.dif)
ggtsdisplay(r)


## teste estacionariedade
adf.test(bitcoin)
adf.test(log(bitcoin))
adf.test(diff(bitcoin))
adf.test(diff(log(bitcoin)))
adf.test(ra^2)
adf.test(bitcoin.dif)

ndiffs(log(bitcoin))
ndiffs(bitcoin)
bitcoin.dif=diff(log(bitcoin), 2)




plot(bitcoin.dif)
acf(bitcoin.dif)
pacf(bitcoin.dif)

## sazonalidade
plot(stl(log(bitcoin), "periodic"))
stl(bitcoin, "periodic")
seasonplot(bitcoin, col=rainbow(12),labels=TRUE, type="o", pch=16)
ocsb = ocsb.test(bitcoin)

qs(bitcoin, autoarima = TRUE, freq = 365, residuals = T, diff = F) ## teste de sazonalidade

nsdiffs(log(bitcoin), test = "ocsb")

## Modelos arima
  ## amostra inteira
  modelo = auto.arima(log(bitcoin), max.p = 9, max.q = 9, ic="aic", trace = T, optim.method="Nelder-Mead")
  modelo = arima(log(bitcoin), order = c(8,2,1), optim.method="Nelder-Mead")
  
  plot(bitcoin, type="l")
  lines(modelo$fitted, col="blue")
  Box.test(modelo$residuals,lag = 1, type = "Ljung-Box")
  checkresiduals(modelo)
  qqnorm(modelo$residuals)
  qqline(modelo$residuals)
  tsdiag(modelo)

  ## amostra dividida em 2
    # 1
    modelo.1 = auto.arima(log(bitcoin[1:1729]), max.p = 9, max.q = 9, ic="aic", trace = T, optim.method="Nelder-Mead")
    plot(bitcoin[1:1729], type="l")
    lines(modelo.1$fitted, col="red")
    checkresiduals(modelo.1)
    Box.test(modelo.1$residuals,lag = 1, type = "Ljung-Box")
    qqnorm(modelo.treino$residuals)
    qqline(modelo.treino$residuals)
    tsdiag(modelo.treino)
    
    # 2
    modelo.2 = auto.arima(log(bitcoin[1730:3458]),max.p = 9, max.q = 9, ic="aic", trace = T, optim.method="Nelder-Mead")
    plot(bitcoin[1730:3458], type="l")
    lines(modelo.2$fitted, col="red")
    checkresiduals(modelo.2)
    Box.test(modelo.2$residuals,lag = 4, type = "Ljung-Box")
    qqnorm(modelo.treino$residuals)
    qqline(modelo.treino$residuals)
    tsdiag(modelo.treino)

  ## amostra dividida em 3
    # 3
    modelo.3 = auto.arima(log(bitcoin[1:1153]),max.p = 9, max.q = 9, ic="aic", trace = T, optim.method="Nelder-Mead")
    plot(bitcoin[1:1153], type="l")
    lines(modelo.3$fitted, col="red")
    checkresiduals(modelo.3)
    Box.test(modelo.3$residuals,lag = 6, type = "Ljung-Box")
    qqnorm(modelo.treino$residuals)
    qqline(modelo.treino$residuals)
    tsdiag(modelo.treino)

    # 4
    modelo.4 = auto.arima(log(bitcoin[1154:2305]),max.p = 9, max.q = 9, ic="aic", trace = T, optim.method="Nelder-Mead")
    plot(bitcoin[1154:2305], type="l")
    lines(modelo.4$fitted, col="red")
    checkresiduals(modelo.4)
    Box.test(modelo.4$residuals,lag = 3, type = "Ljung-Box")
    qqnorm(modelo.treino$residuals)
    qqline(modelo.treino$residuals)
    tsdiag(modelo.treino)
    
    # 5
    modelo.5 = auto.arima(log(bitcoin[2306:3458]),max.p = 9, max.q = 9, ic="aic", trace = T, optim.method="Nelder-Mead")
    plot(bitcoin[2306:3458], type="l")
    lines(modelo.5$fitted, col="red")
    checkresiduals(modelo.5)
    Box.test(modelo.5$residuals,lag = 4, type = "Ljung-Box")
    qqnorm(modelo.treino$residuals)
    qqline(modelo.treino$residuals)
    tsdiag(modelo.treino)
    
    
  # dividindo por 4
    # 6
    modelo.6 = auto.arima(log(bitcoin[1:865]),max.p = 9, max.q = 9, ic="aic", trace = T, optim.method="Nelder-Mead")
    plot(bitcoin[1:865], type="l")
    lines(modelo.6$fitted, col="red")
    checkresiduals(modelo.6)
    Box.test(modelo.6$residuals,lag = 5, type = "Ljung-Box")
    qqnorm(modelo.treino$residuals)
    qqline(modelo.treino$residuals)
    tsdiag(modelo.treino)
    
    # 7
    modelo.7 = auto.arima(log(bitcoin[866:1730]),max.p = 9, max.q = 9, ic="aic", trace = T, optim.method="Nelder-Mead")
    plot(bitcoin[866:1730], type="l")
    lines(modelo.7$fitted, col="red")
    checkresiduals(modelo.7)
    Box.test(modelo.7$residuals,lag = 9, type = "Ljung-Box")
    qqnorm(modelo.treino$residuals)
    qqline(modelo.treino$residuals)
    tsdiag(modelo.treino)
    
    # 8
    modelo.8 = auto.arima(log(bitcoin[1731:2595]), trace = T, optim.method="Nelder-Mead")
    plot(bitcoin[1731:2595], type="l")
    lines(modelo.8$fitted, col="red")
    checkresiduals(modelo.8)
    Box.test(modelo.8$residuals,lag = 2, type = "Ljung-Box")
    qqnorm(modelo.treino$residuals)
    qqline(modelo.treino$residuals)
    tsdiag(modelo.treino)
    
    # 9
    modelo.9 = auto.arima(log(bitcoin[2596:3458]),max.p = 9, max.q = 9, ic="aic", trace = T, optim.method="Nelder-Mead")
    plot(bitcoin[2596:3458], type="l")
    lines(modelo.9$fitted, col="red")
    checkresiduals(modelo.9)
    Box.test(modelo.9$residuals,lag = 4, type = "Ljung-Box")
    qqnorm(modelo.treino$residuals)
    qqline(modelo.treino$residuals)
    tsdiag(modelo.treino)
    
  # dividindo por 5
    #10
    modelo.10 = auto.arima(log(bitcoin[1:692]),max.p = 9, max.q = 9, ic="aic", trace = T, optim.method="Nelder-Mead")
    plot(bitcoin[1:692], type="l")
    lines(modelo.10$fitted, col="red")
    checkresiduals(modelo.10)
    Box.test(modelo.10$residuals,lag = 2, type = "Ljung-Box")
    qqnorm(modelo.treino$residuals)
    qqline(modelo.treino$residuals)
    tsdiag(modelo.treino)
    
    #11
    modelo.11 = auto.arima(log(bitcoin[693:1384]),max.p = 9, max.q = 9, ic="aic", trace = T, optim.method="Nelder-Mead")
    plot(bitcoin[693:1384], type="l")
    lines(modelo.11$fitted, col="red")
    checkresiduals(modelo.11)
    Box.test(modelo.11$residuals,lag = 12, type = "Ljung-Box")
    qqnorm(modelo.treino$residuals)
    qqline(modelo.treino$residuals)
    tsdiag(modelo.treino)
    
    #12
    modelo.12 = auto.arima(log(bitcoin[1385:2076]),max.p = 9, max.q = 9, ic="aic", trace = T, optim.method="Nelder-Mead")
    plot(bitcoin[1385:2076], type="l")
    lines(modelo.12$fitted, col="red")
    checkresiduals(modelo.12)
    Box.test(modelo.12$residuals,lag = 50, type = "Ljung-Box")
    qqnorm(modelo.treino$residuals)
    qqline(modelo.treino$residuals)
    tsdiag(modelo.treino)
    
    #13
    modelo.13 = auto.arima(log(bitcoin[2077:2768]),max.p = 9, max.q = 9, ic="aic", trace = T, optim.method="Nelder-Mead")
    plot(log(bitcoin[2077:2768]), type="l")
    lines(modelo.13$fitted, col="red")
    checkresiduals(modelo.13)
    Box.test(modelo.13$residuals,lag = 4, type = "Ljung-Box")
    qqnorm(modelo.treino$residuals)
    qqline(modelo.treino$residuals)
    tsdiag(modelo.treino)
    
    #14
    modelo.14 = auto.arima(log(bitcoin[2769:3458]),max.p = 9, max.q = 9, ic="aic", trace = T)
    plot(bitcoin[2769:3458], type="l")
    lines(modelo.14$fitted, col="red")
    checkresiduals(modelo.14)
    Box.test(modelo.14$residuals,lag = 19, type = "Ljung-Box")
    qqnorm(modelo.treino$residuals)
    qqline(modelo.treino$residuals)
    tsdiag(modelo.treino)
  
    

## previsão
    

  #30 dias  
    # inteiro
      forec = forecast(modelo, h=12)
      plot(forec)
      plot(forec, xlim=c(2019.3,2020.3))
      plot(log(bitcoin.atual), type="l", main="Amostra inteira")
      lines(as.numeric(forec$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual)) , as.numeric(forec$mean))
      
      
      # 1
      forec.1 = forecast(modelo.1, h=30)
      plot(forec.1)
      plot(forec.1, xlim=c(1500,2000))
      plot(log(bitcoin[1730:1760]), type="l", main="Amostra do modelo 1")
      lines(as.numeric(forec.1$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[1730:1760])) , as.numeric(forec.1$mean))
      
      # 2
      forec.2 = forecast(modelo.2, h=12)
      plot(forec.2)
      plot(forec.2, xlim=c(1500,2000))
      plot(log(bitcoin.atual), type="l", main="Amostra do modelo 2")
      lines(as.numeric(forec.2$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual)) , as.numeric(forec.2$mean))
      
      # 3
      forec.3 = forecast(modelo.3, h=30)
      plot(forec.3)
      plot(forec.3, xlim=c(1100,1250))
      plot(log(bitcoin[1154:1184]), type="l", main="Amostra do modelo 3")
      lines(as.numeric(forec.3$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[1154:1184])) , as.numeric(forec.3$mean))
      
      # 4
      forec.4 = forecast(modelo.4, h=30)
      plot(forec.4)
      plot(forec.4, xlim=c(1100,1250))
      plot(log(bitcoin[2306:2336]), type="l", main="Amostra do modelo 4")
      lines(as.numeric(forec.4$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[2306:2336])) , as.numeric(forec.4$mean))
      
      # 5
      forec.5 = forecast(modelo.5, h=12)
      plot(forec.5)
      plot(forec.5, xlim=c(1100,1250))
      plot(log(bitcoin.atual), type="l", main="Amostra do modelo 5")
      lines(as.numeric(forec$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual)) , as.numeric(forec.5$mean))
      
      # 6
      forec.6 = forecast(modelo.6, h=30)
      plot(forec.6)
      plot(forec.6, xlim=c(800,1000))
      plot(log(bitcoin[866:896]), type="l", main="Amostra do modelo 6")
      lines(as.numeric(forec.6$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[866:896])) , as.numeric(forec.6$mean))
      
      # 7
      forec.7 = forecast(modelo.7, h=30)
      plot(forec.7)
      plot(forec.7, xlim=c(800,1000))
      plot(log(bitcoin[1731:1761]), type="l", main="Amostra do modelo 7")
      lines(as.numeric(forec.7$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[1731:1761])) , as.numeric(forec.7$mean))
      
      # 8
      forec.8 = forecast(modelo.8, h=30)
      plot(forec.8)
      plot(forec.8, xlim=c(800,1000))
      plot(log(bitcoin[2596:2626]), type="l", main="Amostra do modelo 8")
      lines(as.numeric(forec.8$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[2596:2626])) , as.numeric(forec.8$mean))
      
      # 9
      forec.9 = forecast(modelo.9, h=12)
      plot(forec.9)
      plot(forec.9, xlim=c(800,1000))
      plot(log(bitcoin.atual), type="l", main="Amostra do modelo 9")
      lines(as.numeric(forec.9$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual)) , as.numeric(forec.9$mean))
      
      # 10
      forec.10 = forecast(modelo.10, h=30)
      plot(forec.10)
      plot(forec.10, xlim=c(600,800))
      plot(log(bitcoin[693:723]), type="l", main="Amostra do modelo 10")
      lines(as.numeric(forec.10$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[693:723])) , as.numeric(forec.10$mean))
      
      # 11
      forec.11 = forecast(modelo.11, h=30)
      plot(forec.11)
      plot(forec.11, xlim=c(600,800))
      plot(log(bitcoin[1385:1415]), type="l", main="Amostra do modelo 11", ylim=c(6.3,6.6))
      lines(as.numeric(forec.11$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[1385:1415])) , as.numeric(forec.11$mean))
      
      # 12
      forec.12 = forecast(modelo.12, h=30)
      plot(forec.12)
      plot(forec.12, xlim=c(600,800))
      plot(log(bitcoin[2077:2107]), type="l", main="Amostra do modelo 12")
      lines(as.numeric(forec.12$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[2077:2107])) , as.numeric(forec.12$mean))
      
      # 13
      forec.13 = forecast(modelo.13, h=30)
      plot(forec.13)
      plot(forec.13, xlim=c(600,800))
      plot(log(bitcoin[2769:2799]), type="l", main="Amostra do modelo 13", ylim=c(8.8,9.5))
      lines(as.numeric(forec.13$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[2769:2799])) , as.numeric(forec.13$mean))
      
      # 14
      forec.14 = forecast(modelo.14, h=12)
      plot(forec.14)
      plot(forec.14, xlim=c(600,800))
      plot(log(bitcoin.atual), type="l", main="Amostra do modelo 14")
      lines(as.numeric(forec.14$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual)) , as.numeric(forec.14$mean))
      
  # 10 dias
  
      windows()
      par(mfrow = c(3,3))    
      # 2
      forec.2 = forecast(modelo.2, h=10)
      plot(forec.2)
      plot(forec.2, xlim=c(1500,2000))
      plot(log(bitcoin.atual[1:10]), type="l", main="Amostra do modelo 2")
      lines(as.numeric(forec.2$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual[1:10])) , as.numeric(forec.2$mean))
      
      
      # 5
      forec.5 = forecast(modelo.5, h=10)
      plot(forec.5)
      plot(forec.5, xlim=c(1100,1250))
      plot(log(bitcoin.atual[1:10]), type="l", main="Amostra do modelo 5")
      lines(as.numeric(forec.5$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual[1:10])) , as.numeric(forec.5$mean))
      
      
      # 8
      forec.8 = forecast(modelo.8, h=10)
      plot(forec.8)
      plot(forec.8, xlim=c(800,1000))
      plot(log(bitcoin[2596:2605]), type="l", main="Amostra do modelo 8")
      lines(as.numeric(forec.8$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[2596:2626])) , as.numeric(forec.8$mean))
      
      # 9
      forec.9 = forecast(modelo.9, h=10)
      plot(forec.9)
      plot(forec.9, xlim=c(800,1000))
      plot(log(bitcoin.atual[1:10]), type="l", main="Amostra do modelo 9")
      lines(as.numeric(forec.9$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual[1:10])) , as.numeric(forec.9$mean))
      
      # 10
      forec.10 = forecast(modelo.10, h=10)
      plot(forec.10)
      plot(forec.10, xlim=c(600,800))
      plot(log(bitcoin[693:702]), type="l", main="Amostra do modelo 10", ylim=c(1.90,2.15))
      lines(as.numeric(forec.10$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[693:722])) , as.numeric(forec.10$mean))
      
      
      # 13
      forec.13 = forecast(modelo.13, h=10)
      plot(forec.13)
      plot(forec.13, xlim=c(600,800))
      plot(log(bitcoin[2769:2778]), type="l", main="Amostra do modelo 13", ylim=c(8.8,9.5))
      lines(as.numeric(forec.13$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[2769:2778])) , as.numeric(forec.13$mean))
      
      plot(bitcoin[3500:3600]) ## para deixar os graficos organizados
      
      # 14
      forec.14 = forecast(modelo.14, h=10)
      plot(forec.14)
      plot(forec.14, xlim=c(600,800))
      plot(log(bitcoin.atual[1:10]), type="l", main="Amostra do modelo 14")
      lines(as.numeric(forec.14$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual[1:10])) , as.numeric(forec.14$mean))
      
      
    # 5 dias
      
      # 2
      forec.2 = forecast(modelo.2, h=5)
      plot(forec.2)
      plot(forec.2, xlim=c(1500,2000))
      plot(log(bitcoin.atual[1:5]), type="l", main="Amostra do modelo 2")
      lines(as.numeric(forec.2$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual[1:5])) , as.numeric(forec.2$mean))
      
      
      # 5
      forec.5 = forecast(modelo.5, h=5)
      plot(forec.5)
      plot(forec.5, xlim=c(1100,1250))
      plot(log(bitcoin.atual[1:5]), type="l", main="Amostra do modelo 5")
      lines(as.numeric(forec.5$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual[1:10])) , as.numeric(forec.5$mean))
      
      
      # 8
      forec.8 = forecast(modelo.8, h=5)
      plot(forec.8)
      plot(forec.8, xlim=c(800,1000))
      plot(log(bitcoin[2596:2600]), type="l", main="Amostra do modelo 8")
      lines(as.numeric(forec.8$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[2596:2626])) , as.numeric(forec.8$mean))
      
      # 9
      forec.9 = forecast(modelo.9, h=5)
      plot(forec.9)
      plot(forec.9, xlim=c(800,1000))
      plot(log(bitcoin.atual[1:5]), type="l", main="Amostra do modelo 9")
      lines(as.numeric(forec.9$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual[1:5])) , as.numeric(forec.9$mean))
      
      # 10
      forec.10 = forecast(modelo.10, h=5)
      plot(forec.10)
      plot(forec.10, xlim=c(600,800))
      plot(log(bitcoin[693:697]), type="l", main="Amostra do modelo 10", ylim=c(1.90,2.15))
      lines(as.numeric(forec.10$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[693:697])) , as.numeric(forec.10$mean))
      
      
      # 13
      forec.13 = forecast(modelo.13, h=5)
      plot(forec.13)
      plot(forec.13, xlim=c(600,800))
      plot(log(bitcoin[2769:2773]), type="l", main="Amostra do modelo 13", ylim=c(8.8,9.5))
      lines(as.numeric(forec.13$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[2769:2773])) , as.numeric(forec.13$mean))
      
      # 14
      forec.14 = forecast(modelo.14, h=5)
      plot(forec.14)
      plot(forec.14, xlim=c(600,800))
      plot(log(bitcoin.atual[1:5]), type="l", main="Amostra do modelo 14")
      lines(as.numeric(forec.14$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual[1:5])) , as.numeric(forec.14$mean))
      
   # 1 dia
      
      # 2
      forec.2 = forecast(modelo.2, h=1)
      plot(forec.2)
      plot(forec.2, xlim=c(1500,2000))
      plot(log(bitcoin.atual[1:2]), type="l", main="Amostra do modelo 2")
      lines(as.numeric(forec.2$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual[1:2])) , as.numeric(forec.2$mean))
      
      
      # 5
      forec.5 = forecast(modelo.5, h=1)
      plot(forec.5)
      plot(forec.5, xlim=c(1100,1250))
      plot(log(bitcoin.atual[1:5]), type="l", main="Amostra do modelo 5")
      lines(as.numeric(forec.5$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual[1:10])) , as.numeric(forec.5$mean))
      
      
      # 8
      forec.8 = forecast(modelo.8, h=1)
      plot(forec.8)
      plot(forec.8, xlim=c(800,1000))
      plot(log(bitcoin[2596:2600]), type="l", main="Amostra do modelo 8")
      lines(as.numeric(forec.8$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[2596:2626])) , as.numeric(forec.8$mean))
      
      # 9
      forec.9 = forecast(modelo.9, h=1)
      plot(forec.9)
      plot(forec.9, xlim=c(800,1000))
      plot(log(bitcoin.atual[1:5]), type="l", main="Amostra do modelo 9")
      lines(as.numeric(forec.9$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual[1:5])) , as.numeric(forec.9$mean))
      
      # 10
      forec.10 = forecast(modelo.10, h=1)
      plot(forec.10)
      plot(forec.10, xlim=c(600,800))
      plot(log(bitcoin[693:697]), type="l", main="Amostra do modelo 10", ylim=c(1.90,2.15))
      lines(as.numeric(forec.10$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[693:697])) , as.numeric(forec.10$mean))
      
      
      # 13
      forec.13 = forecast(modelo.13, h=1)
      plot(forec.13)
      plot(forec.13, xlim=c(600,800))
      plot(log(bitcoin[2769:2773]), type="l", main="Amostra do modelo 13", ylim=c(8.8,9.5))
      lines(as.numeric(forec.13$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin[2769:2773])) , as.numeric(forec.13$mean))
      
      # 14
      forec.14 = forecast(modelo.14, h=1)
      plot(forec.14)
      plot(forec.14, xlim=c(600,800))
      plot(log(bitcoin.atual[1:5]), type="l", main="Amostra do modelo 14")
      lines(as.numeric(forec.14$mean),col="blue")
      checkresiduals(modelo)
      accuracy(as.numeric(log(bitcoin.atual[1:5])) , as.numeric(forec.14$mean))
      
      

# end