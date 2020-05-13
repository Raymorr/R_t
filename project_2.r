library( QuantTools ) # Market data loading
library( data.table ) # Library for data management
library( ghyp ) # GHP

stock_1 = get_yahoo_data( "^NDX", from = "2014-01-01", to = "2018-01-01" )
stock_2 = get_yahoo_data( "GS" , from = "2014-01-01", to = "2018-01-01" )
stock_3 = get_yahoo_data( "JPM" , from = "2014-01-01", to = "2018-01-01" )
stock_4 = get_yahoo_data( "DB" , from = "2014-01-01", to = "2018-01-01" )

t = as.Date("2018-01-01")-as.Date("2014-01-01")


stock_1 = stock_1[, .( date, return_1 = close / shift( close, fill = close[1] ) - 1 )]
stock_2 = stock_2[, .( date, return_2 = close / shift( close, fill = close[1] ) - 1 )]
stock_3 = stock_3[, .( date, return_3 = close / shift( close, fill = close[1] ) - 1 )]
stock_4 = stock_4[, .( date, return_3 = close / shift( close, fill = close[1] ) - 1 )]

portfolio <- merge(stock_1,stock_2, by="date")
portfolio <- merge(stock_3,portfolio, by="date")
portfolio <- merge(stock_4,portfolio, by="date")

#let's simplify our task for just one stock

stock_1
aic.uv <- stepAIC.ghyp(stock_1[,2][1:365], dist=c("ghyp", "hyp", "t", "gauss"), silent=TRUE)
summary(aic.uv$best.model)

distr<-aic.uv$best.model

#generate paths for one asset
asset.paths <- function(s0, mu, sigma, 
                        nsims = 10000, 
                        periods = c(0:t)   # time periods at which to simulate prices
) 
{
  s0 = as.vector(s0)
  nsteps = length(periods)
  dt = c(periods[1], diff(periods))
  
  if( length(s0) == 1 ) {
    drift = mu - 0.5 * sigma^2
    if( nsteps == 1 ) {
      s0 * exp(c(drift) * dt + c(sigma) * sqrt(dt) * rnorm(nsims))
    } else {
      temp = matrix(exp(c(drift) * dt + c(sigma) * sqrt(dt) * rnorm(nsteps * nsims)), nc=nsims)
      for(i in 2:nsteps) temp[i,] = temp[i,] * temp[(i-1),]
      s0*temp
    }
  } else {
    require(MASS)
    drift = mu - 0.5 * diag(sigma)
    n = length(mu)
    if( nsteps == 1 ) {
      s0 * exp(drift * dt + sqrt(dt) * t(mvrnorm(nsims, rep(0, n), sigma)))
    } else {
      temp = array(exp(as.vector(drift %*% t(dt)) + t(sqrt(dt) * mvrnorm(nsteps * nsims, rep(0, n), sigma))), c(n, nsteps, nsims))
      for(i in 2:nsteps) temp[,i,] = temp[,i,] * temp[,(i-1),]
      s0 * temp
    }
  }
}


  #simulate for one stock
s0 <- 3576.24
mu <- 0
sigma = distr@sigma

mu
sigma
periods = 0:1825


prices <- asset.paths(s0, mu, sigma, nsims = 20000, periods = periods)

matplot(prices[,1:100], type='l', xlab='Days', ylab='Prices',
        main='Selected Price Paths')

price_dist <- prices[365,]

barrier_price <- s0*0.9
barrier_price

proba<-c(0:4)

for(i in c(1:5)){
  price_dist<-prices[365*i,]
  hit<-price_dist[price_dist>=barrier_price]
  proba[i]<-length(hit)/length(price_dist)
}

proba
true_proba = c(1:5)

prob.dist <- function(i, x, y){
  if(i == 1) return(x)
  else return (prob.dist(i-1,  x+y -x*y, y))
}

for(i in c(1:5)) {
    true_proba[6-i] = prob.dist(i, proba[5], proba[4])
}
  
true_proba
#go 4 stocks
s01<-3592
s02<-167.9233
s03<-52.35753
s04<-45.39858

start_prices<-c(s01,s02,s03,s04)
means<-c(0:3)
#libor USD == means
means[1] = 0.000443861
means[2] = 0.000443861
means[3] = 0.000443861
means[4] = 0.000443861
means0<-c(0:3)*0

#covar matrix
k <- ncol(portfolio[,2:5]) 
n <- nrow(portfolio[,2:5]) 

#create means for each column
colnames(portfolio)[2] <- "1"
colnames(portfolio)[3] <- "2"
colnames(portfolio)[4] <- "3"
colnames(portfolio)[5] <- "4"

covar<-cov(portfolio[,2:5])

barriers <- start_prices*0.9

nsims = 5000
prices <- asset.paths(start_prices, means, covar, nsims = nsims, periods = periods)

price_dist <- c(1:4)
price_dist <- prices[,365,1]
amount = 0
for(i in c(1:5)){
  for(j in c(1:5000)) { 
    temp<-prices[, 365*i,j]
    if(temp[1] >= barriers[1] & temp[2] >= barriers[2] & temp[3] >= barriers[3] & temp[4] >= barriers[4]) {
      amount = amount + 1
    }
  }
  proba[i]<-amount/nsims
  amount = 0
}
proba
true_proba = c(1:5)

for(i in c(1:5)) {
  true_proba[6-i] = prob.dist(i, proba[5], proba[4])
}

true_proba
