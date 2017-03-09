Fact <- function(n) if (n == 1) 1 else n * Fact(n - 1)
Fact(5)

data("AirPassengers")
AP <- AirPassengers
AP
class(AP)
start(AP)
end(AP)
frequency(AP)
plot(AP, ylab = "Passengers (1000's)")

layout(1:2)
plot(aggregate(AP))
boxplot(AP ~ cycle(AP))

file <- "LAUS_County_LMA_Town_NSA.csv"
Maine.month <- read.csv(file = file, header = TRUE)
class(Maine.month)
attach(Maine.month)

Maine.month.ts <- ts(Geography, start = c(1976, 1),frequency = 12)
Maine.annual.ts <- aggregate(Maine.month.ts)/12

layout(1:2)
plot(Maine.month.ts, ylab = "Geography")
plot(Maine.annual.ts, ylab = Geography)

Maine.Feb <- window(Maine.month.ts, start = c(2016,2),frequency=TRUE)
Maine.Aug <- window(Maine.month.ts, start = c(2016,8),frequency = TRUE)
Maine.1976 <- window(Maine.annual.ts, start = c(1976,1),frequency=TRUE)
Maine.2016 <- window(Maine.annual.ts, start = c(2016,1),frequency = TRUE)

Feb.ratio <- mean(Maine.Feb)/mean(Maine.month.ts)
Aug.ratio <- mean(Maine.Aug)/mean(Maine.month.ts)
M1976.ratio <- mean(Maine.1976)/mean(Maine.annual.ts)
M2016.ratio <- mean(Maine.2016)/mean(Maine.annual.ts)

AP.maine <- ts.intersect(AP,Maine.month.ts)
cor(AP, Maine.month.ts)



vec = sin((1:365)/30)  # A vector of values
vec1 = cos((1:365)/30)  # A vector of values
vec2 = tan(1:365/30)
class(vec) # Vector is an atomic class in R

## Create a ts class object from the vector
## by adding time series attributes
vec.ts = ts(vec, start = 1990/01/01, freq = 365)
vec1.ts = ts(vec1, start = 1990/01/01, freq = 365)
vec2.ts = ts(vec2, start = 1990/01/01, freq = 365)
class(vec.ts)
attributes(vec.ts) # Note the time series attributes
attributes(vec1.ts) # Note the time series attributes
attributes(vec2.ts) # Note the time series attributes
require(repr)
options(repr.pmales.extlot.width=8, repr.plot.height=4)
layout(1:3)
plot(vec.ts, ylab = "sine")
plot(vec1.ts, ylab = "cosine")
plot(vec2.ts, ylab = "tangent")

options(repr.pmales.extlot.width=8, repr.plot.height=4)
ts.white = function(n, mean = 0.0, sd = 1.0, start = 1990, freq = 12){
  ts(rnorm(n, mean = mean, sd = sd), start = start, freq = 12)
}
white = ts.white(180)
attributes(white)
plot(white)

dist.ts = function(df, col = 'residual', bins = 40){
  par(mfrow = c(1,2))
  temp = as.vector(df)
  breaks = seq(min(temp), max(temp), length.out = (bins + 1))
  hist(temp, breaks = breaks, main = paste('Distribution of ', col), xlab = col)
  qqnorm(temp, main = paste('Normal Q-Q plot of ', col))
  par(mfrow = c(1,1))
}
dist.ts(white, col = 'white noise')

options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf <- function(df, col = 'remainder', is.df =TRUE){
  if(is.df) temp <- df[, col]
  else temp <- df
  par(mfrow = c(2,1))
  acf(temp, main = paste('ACF of', col))
  pacf(temp, main = paste('PACF of', col))
  par(mfrow = c(1,1))
}
plot.acf(white, col = 'white noise', is.df = F)

ts.white.sin = function(n, mean = 0.0, sd = 1.0, start = 1990, freq = 12){
  sin.vals = 1 * sin(1:n/freq)
  ts((rnorm(n, mean = mean, sd = sd) + sin.vals), start = start, freq = 12)
}
white.sin = ts.white.sin(180)
attributes(white.sin)
plot(white.sin)

## Investigate the time series properties of random walk
options(repr.pmales.extlot.width=8, repr.plot.height=4)
ran.walk = function(n, freq = 12, start = 1990, sd = 1.0, mean = 0.0){
  norms = rnorm(n, mean = mean, sd = sd)
  ts(cumsum(norms), start = start, freq = 12)
}
ranWalk = ran.walk(180)
plot(ranWalk, main = 'Random walk time series')

options(repr.pmales.extlot.width=8, repr.plot.height=4)
dist.ts(ranWalk, col = 'random walk')
options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf(ranWalk, col = 'random walk', is.df = F)

## ---- Investigate time series properties of 
## trend + white noise
options(repr.pmales.extlot.width=8, repr.plot.height=4)
ts.trend = function(n, slope = 0.01, mean = 0.0, sd = 1.0, start = 1990, freq = 12){
  temp = seq(0, slope * n, length.out = n) + 
    rnorm(n, mean = mean, sd = sd)
  ts(temp, start = start, freq = 12)
}
trend = ts.trend(180, slope = 0.05)
plot(trend, main = 'Trend + white noise time series')

dist.ts(trend, col = 'trend + white noise')


options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf(trend, col = 'trend + white noise', is.df = F)

## --- Investigate time series properties of 
## trend + white noise + seasonal
ts.season = function(n, slope = 0.01, mean = 0.0, sd = 1.0, start = 1990, freq = 12){
  temp = seq(0, slope * n, length.out = n) + 
    rnorm(n, mean = mean, sd = sd) +
    2 * sin(0:(n -1) * pi / freq) +
    cos(0:(n -1) * pi / freq)
  ts(temp, start = start, freq = 12)
}
season = ts.season(180, slope = 0.00)
options(repr.pmales.extlot.width=8, repr.plot.height=4)
plot(season, main = 'White noise + seasonal time series')

options(repr.pmales.extlot.width=8, repr.plot.height=4)
dist.ts(season, col = 'seasonal + white noise')
options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf(season, col = 'seasonal + white noise', is.df = F)

## Decomposition of the time series into components
ts.decomp <- function(df, col = 'elec.ts', span = 0.5, Mult = TRUE, is.df = TRUE){
  # if(Mult) temp = log(df[, col])  else temp = ts(df[, col]
  if(is.df) temp = log(df[, col])  
  else temp = df
  spans = span * length(temp)  
  fit <- stl(temp, s.window = "periodic", t.window = spans)
  plot(fit, main = paste('Decompositon of',col,'with lowess span = ', as.character(span)))
  fit$time.series
}
season.trend = ts.season(180, slope = 0.05)      
temp = ts.decomp(season.trend, is.df = FALSE, Mult = FALSE)

## Use a first order difference series to 
## remove the trend
ts.diff <- function(ts, lag = 1){
  diff(ts, lag = lag)
}
diff.walk = ts.diff(ranWalk)
options(repr.pmales.extlot.width=8, repr.plot.height=4)
plot(diff.walk, main = 'Difference of random walk time series')

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

options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf(arMod, col = 'AR(1) model', is.df = F)

## Function for ARIMA model estimation
ts.model = function(ts, col = 'remainder', order = c(0,0,1)){
  mod = arima(ts, order = order, include.mean = FALSE)
  print(mod)
  mod
}
mod.est = ts.model(arMod, col = 'AR(1) process', order = c(1,0,0))
plot.acf(mod.est$resid[-1], col = 'AR(1) estimate', is.df = F)
