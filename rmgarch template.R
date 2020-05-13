
# libraries
library(Rfast)
library(xts)
library(rmgarch)
library(crypto)
no_cores = detectCores() - 1

# choose coins and dates
list_of_coins = c('BTC', 'ETH')
start_date = as.Date("2016-06-01")
end_date = as.Date("2018-05-25")

# download data and combine into one df
all_coin_price = getCoins(coin=list_of_coins, start_date=format(start_date, "%Y%m%d"), end_date=format(end_date, "%Y%m%d"))

needed_columns = c('date', 'close')
all_data = NULL
for (ticket in list_of_coins){
  coin_price = all_coin_price[all_coin_price$symbol==ticket,]
  
  coin_price = coin_price[,needed_columns]
  colnames(coin_price) = c('date', paste('close', ticket, sep='_'))
  
  if (is.null(all_data)){
    all_data = coin_price
  } else{
    all_data = merge(x=all_data, y=coin_price, by='date')
  }
  
  
  if (ncol(all_data) == 0){
    print('No intersection in dates')
    break()
  }
}


# preprocess data
all_data$date = as.Date(all_data$date)
#all_data[,-1] = data.frame(lapply(all_data[,-1], function(x) (x - mean(x))/sd(x) )) # mean(x)
all_data_ts = xts(all_data[,-1], order.by=all_data$date)
all_data_ts_diff = diff(log(all_data_ts))[-1,]


# SOME PARAMS FOR CALCULATION
p = 1
n_sim = 2*10^4
alpha = 0.05

n_series = ncol(all_data_ts_diff)
T_series = nrow(all_data_ts_diff)

# specify univariate model for volatility -- sGARCH
garch.spec = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                        distribution.model='norm')

# specify multivariate model for mean and cov matrix -- VAR(1) + DCC or Copula
spec.DCC_mvnorm = dccspec(uspec = multispec( replicate(n_series, garch.spec) ),
                                    VAR = TRUE, robust = TRUE, lag = p,
                                    dccOrder = c(1,1),
                                    model = 'DCC',
                                    distribution = 'mvnorm')

spec.Copula_mvt = cgarchspec(uspec=multispec( replicate(n_series, garch.spec) ),
                             VAR = TRUE, robust = TRUE, lag = p,
                             dccOrder = c(1, 1), asymmetric = FALSE,
                             distribution.model = list(copula = "mvt",
                                                       method = "Kendall", 
                                                       time.varying = FALSE, 
                                                       transformation = "empirical"))

# fit DCC model and make a prediction
cl = makeCluster(no_cores)
dcc.fit = dccfit(spec.DCC_mvnorm, data = all_data_ts_diff, cluster=cl)
stopCluster(cl)

pred_x = dccforecast(dcc.fit, n.ahead=1)
fitted(pred_x)


# fit Copula model and make simulations and then a prediction
cl = makeCluster(no_cores)
cgarch.fit = cgarchfit(spec.Copula_mvt, data = all_data_ts_diff, cluster=cl)
stopCluster(cl)

cl = makeCluster(no_cores)
sim = cgarchsim(cgarch.fit, n.sim=1, m.sim=n_sim, startMethod = "sample", cluster = cl, rseed=42)
stopCluster(cl)
sim_x = do.call(rbind,  sim@msim$simX)

ms = colMeans(sim_x)
sds = apply(sim_x, 2, sd)


# obtain skew and shape
k = 0
skew = coef(cgarch.fit)[paste(c('[', colnames(all_data_ts)[k], '].skew'), sep='', collapse = '')]
shape = coef(cgarch.fit)[paste(c('[', colnames(all_data_ts)[k], '].shape'), sep='', collapse = '')]


# use this fucntions for conducting VaR and ES tests after backtesting
VaRTest(alpha=alpha, conf.level=0.95, actual=temp_real, VaR=temp_VaR)
ESTest(alpha=0.05, conf.level = 0.95, actual=temp_real, ES=temp_ES, VaR=temp_VaR, boot=TRUE)