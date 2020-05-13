library( QuantTools ) # Market data loading
library( data.table ) # Library for data management
library( ghyp )       # GHP
library( copula )     # Copula
library( fGarch )     # GARCH   
library(rmgarch)
# download and prepating data
asset_1 <- data.table(read.csv(file="/home/p/hash/bnce_btcusdt.csv", fill = TRUE))
asset_2 <- data.table(read.csv(file="/home/p/hash/btmx_btcusd.csv", fill = TRUE))

asset_1 = asset_1[, .( timestamp, value_1 = close - shift( close, fill = close[1] ))]
asset_2 = asset_2[, .( timestamp, value_2 = close - shift( close, fill = close[1] ))]

portfolio = merge( asset_1, asset_2, by = "timestamp" )
data = as.vector( portfolio[, .( value_1, value_2 ) ] )



numb = nrow(data)
train = data[1 : round(numb/2)]
test = data[round(numb/2)+1:numb]

setDF(data)
plot(seq(0, 360, 1), type = 'l', data[, "value_1"][1:numb %% 1440 == 0],col="blue")
lines(seq(0, 360, 1), data[, "value_2"][1:numb %% 1440 == 0],col="red")
plot(data[, "value_1"][1:numb %% 1440 == 0], data[, "value_2"][1:numb %% 1440 == 0])

plot.new()
plot(seq(0, 1000, 1), type = 'l', data[, "value_1"][1:1001], col="blue")
lines(seq(0, 1000, 1), data[, "value_2"][1:1001], col="red")

# find garch volatility and bind our assets by DCC
setDF(train)
train_1 = train[1]
train_1 = na.omit(train_1)

train_2 = train[2]
train_2 = na.omit(train_2)

#d = stepAIC.ghyp( train, dist = c( "gauss", "t", "ghyp" ), symmetric=NULL, silent=T )
#d$best.model

norm.cop = normalCopula( dim=2, param=0.5, dispstr="un" )
stud.cop = tCopula     ( dim=2, param=0.5 )
gumb.cop = gumbelCopula( dim=2, param=2 )

persp( norm.cop, dCopula )
persp( stud.cop, dCopula )
persp( gumb.cop, dCopula )


asset1.gfit = garchFit( data = train_1, formula =~arma(0, 3) + garch(1,1), trace=F )

asset2.gfit = garchFit( data = train_2, formula =~garch(1,1), trace=F )

z = matrix( nrow=round(numb/2), ncol=2 )
z[,1] = asset1.gfit@residuals / asset1.gfit@sigma.t
z[,2] = asset2.gfit@residuals / asset2.gfit@sigma.t

# CDF for residuals
mean = c(0,0); sd = c(1,1); nu = c(1.3,1.3)
xi = c(1, 1)
cdf = matrix( nrow=round(numb/2), ncol=2 )

cdf[,1] = psged( z[,1], mean=mean[1], sd=sd[1], nu=nu[1], xi=xi[1] )
cdf[,2] = psged (z[,2], mean=mean[2], sd=sd[2], nu=nu[2], xi=xi[2] )


# Copula fiting
norm.fit = fitCopula( cdf, copula=norm.cop )
stud.fit = fitCopula( cdf, copula=stud.cop )
gumb.fit = fitCopula( cdf, copula=gumb.cop )

# best model
data.table(
  norm = norm.fit@loglik,
  stud = stud.fit@loglik,
  gumb = gumb.fit@loglik
)

N = 10^6
cdf.sim = rCopula( n=N, copula=stud.fit@copula )
z.sim = matrix( nrow=N, ncol=2 )
for (i in 1:2) 
  z.sim[,i] = qsged(cdf.sim[,i], mean=mean[i], sd=sd[i], nu=nu[i], xi=xi[i] )

frc1 = predict( asset1.gfit, n.ahead=1 )
frc2 = predict( asset2.gfit, n.ahead=2 )

mu = matrix(nrow = 2, ncol = 2)
sigma = matrix(nrow = 2, ncol = 2)

for (i in 1:2)
  mu[1, i] = frc1[i,1]
  mu[2, i] = frc2[i,1]
  sigma[1, i] = frc1[i,3]
  sigma[2, i] = frc2[i,3]

prt.sim1 = (mu[1]+sigma[1]*mean(z.sim[,1]))
prt.sim2 = (mu[2]+sigma[2]*mean(z.sim[,2]))
prt.sim1
prt.sim2
