library(evd)
library(ghyp)
library(fGarch)
library(copula)
library(rugarch)
library(PerformanceAnalytics)
library(data.table)
library(QuantTools)
library(corrplot)
library(derivmkts)

stock_1 = data.table(get_yahoo_data( "F", from = "2017-01-02", to = "2020-01-01" ))
stock_2 = data.table(get_finam_data( "GAZP", from = "2017-01-02", to = "2020-01-01" ))
stock_3 = data.table(get_yahoo_data( "UBS", from = "2017-01-02", to = "2020-01-01" ))

# download data
stock_1 = stock_1[, .( close / shift( close, fill = close[1] ) - 1)]
stock_2 = stock_2[, .( close / shift( close, fill = close[1] ) - 1)]
stock_3 = stock_3[, .( close / shift( close, fill = close[1] ) - 1)]

stock_1 <- unlist(stock_1, use.names=FALSE)
stock_2 <- unlist(stock_2, use.names=FALSE)
stock_3 <- unlist(stock_3, use.names=FALSE)

ARFIMAspec <- arfimaspec(mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, arfima = FALSE, external.regressors = NULL), distribution.model = c("norm", "std", "t"))
ARFIMAspec.fit <- arfimafit(ARFIMAspec, stock_1)
ARFIMAroll <- arfimaroll(ARFIMAspec, stock_1, n.ahead = 1, refit.window = c("rolling"), window.size = 250, calculate.VaR = TRUE, VaR.alpha = c(0.05))
VaR1 <- ARFIMAroll@forecast[["VaR"]][["alpha(5%)"]]
BACKTESTING <- report(ARFIMAroll, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

ARFIMAspec <- arfimaspec(mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, arfima = FALSE, external.regressors = NULL), distribution.model = c("norm", "std", "t"))
ARFIMAspec.fit <- arfimafit(ARFIMAspec, stock_2)
ARFIMAroll <- arfimaroll(ARFIMAspec, stock_2, n.ahead = 1, refit.window = c("rolling"), window.size = 250, calculate.VaR = TRUE, VaR.alpha = c(0.05))
VaR2 <- ARFIMAroll@forecast[["VaR"]][["alpha(5%)"]]
BACKTESTING <- report(ARFIMAroll, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

ARFIMAspec <- arfimaspec(mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, arfima = FALSE, external.regressors = NULL), distribution.model = c("norm", "std", "t"))
ARFIMAspec.fit <- arfimafit(ARFIMAspec, stock_3)
ARFIMAroll <- arfimaroll(ARFIMAspec, stock_3, n.ahead = 1, refit.window = c("rolling"), window.size = 250, calculate.VaR = TRUE, VaR.alpha = c(0.05))
VaR3 <- ARFIMAroll@forecast[["VaR"]][["alpha(5%)"]]
BACKTESTING <- report(ARFIMAroll, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)

graph1 <-stock_1[0:250]
#graph2 <-stock_2[0:250]
#graph3 <-stock_3[0:250]
plot(graph1, type = "l", col = "black")
lines(VaR1, col="blue")
#lines(VaR2, col="blue")
#lines(VaR3, col="blue")

