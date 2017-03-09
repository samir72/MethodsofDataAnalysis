# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

## --- Investigate time series properties of 
## trend + white noise + seasonal
ts.season = function(n, slope = 0.01, mean = 0.0, sd = 1.0, start = 1990, freq = 12){
  temp = seq(0, slope * n, length.out = n) + 
    rnorm(n, mean = mean, sd = sd) +
    2 * sin(0:(n -1) * pi / freq) +
    cos(0:(n -1) * pi / freq)
  ts(temp, start = start, freq = 12)
}

#plot the autocorrelation function (acf) and partial autocorrelation function (pacf) of the white noise series
plot.acf <- function(df, col = 'remainder', is.df =TRUE){
  if(is.df) temp <- df[, col]
  else temp <- df
  par(mfrow = c(2,1))
  acf(temp, main = paste('ACF of', col))
  pacf(temp, main = paste('PACF of', col))
  par(mfrow = c(1,1))
}

## Decomposition of the time series into components
ts.decomp <- function(df, col = 'elec.ts', span = 0.5, Mult = TRUE, is.df = TRUE){
  #if(Mult) temp = log(df[, col])  else temp = ts(df[, col]
  if(is.df) temp = log(df[, col])  
  else temp = df
  spans = span * length(temp)  
  fit <- stl(temp, s.window = "periodic", t.window = spans)
  plot(fit, main = paste('Decompositon of',col,'with lowess span = ', as.character(span)))
  fit$time.series
}
season.trend = ts.season(180, slope = 0.05)      
temp = ts.decomp(season.trend, is.df = FALSE, Mult = FALSE)

## ---- Simple ARMA models ------
## Simulate an ARMA process
arma.sim = function(ar = c(0.9), ma = c(0), n = 300, mean = 1.0){
  ar1.model = list(ar = ar, ma = ma)
  print(ar1.model)
  ar1 = mean + arima.sim(model = ar1.model, n = n)
  ar1
}
## --- AR(1) process
arMod = arma.sim()
options(repr.pmales.extlot.width=8, repr.plot.height=4)
plot(arMod, main = 'Plot of AR(1) model time series')

#Apply ARIMA Model
## Function for ARIMA model estimation
ts.model = function(ts, col = 'remainder', order = c(0,0,1)){
  mod = arima(ts, order = order, include.mean = FALSE)
  print(mod)
  mod
}
mod.est = ts.model(arMod, col = 'AR(1) process', order = c(1,0,0))
plot.acf(mod.est$resid[-
                         1], col = 'AR(1) estimate', is.df = F)

### ------- Real world data sets ------------
#
# --------Electricity, beer, and chocolate ----
#
# Load the data from the Internet
#www = "http://www.maths.adelaide.edu.au/emac2009/#Data/cbe.dat"
getwd()
CBE = read.table('cbe.dat', sep ="", header = TRUE)
head(CBE)

elec.ts = ts(CBE[,3], start = 1958, freq = 12)
lnelec.ts = log(ts(CBE[,3], start = 1958, freq = 12))
beer.ts = ts(CBE[,2], start = 1958, freq = 12)
lnbeer.ts = ts(log(CBE[,2]), start = 1958, freq = 12)
choclate.ts = ts(CBE[,1], start = 1958, freq = 12)
lnchoclate.ts = log(ts(CBE[,1], start = 1958, freq = 12))
aus.ts = cbind(elec.ts, lnelec.ts, beer.ts, lnbeer.ts, choclate.ts, lnchoclate.ts)
## First look at the series
options(repr.pmales.extlot.width=8, repr.plot.height=8)
plot(aus.ts)

#Using multiplicative model in STL decomposition on electricity data.
elect.decomp = ts.decomp(lnelec.ts, Mult = TRUE, is.df = FALSE, span = 0.25)
#compute and plot the ACF and PACF for the remainder series.
options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf(elect.decomp[, 3], is.df = FALSE)

## Compute ARIMA model of electric residual
elect.arima = ts.model(elect.decomp[, 3], col = 'ARIMA model for electricity', order = c(2,1,2))
elect.arima = ts.model(elect.decomp[, 3], col = 'ARIMA model for electricity', order = c(0,1,1))

#Use forecast to find the optimal seasonal ARIMA Model
require(forecast)
fit.elect = auto.arima(aus.ts[, 'lnelec.ts'], max.p=3, max.q=3,
                       max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                       start.p=0, start.q=0, start.P=0, start.Q=0)
summary(fit.elect)
accuracy(fit.elect)

## Make the forecast for the next year
elect.forecast = forecast(fit.elect, h=12)
summary(elect.forecast)
plot(elect.forecast)

attributes(lnelec.ts)
seasonplot(lnelec.ts)
