# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

#check the distribution
dist.ts = function(df, col = 'residual', bins = 40){
  par(mfrow = c(1,2))
  temp = as.vector(df)
  breaks = seq(min(temp), max(temp), length.out = (bins + 1))
  hist(temp, breaks = breaks, main = paste('Distribution of ', col), xlab = col)
  qqnorm(temp, main = paste('Normal Q-Q plot of ', col))
  par(mfrow = c(1,1))
}

#plot the autocorrelation function (acf) and partial autocorrelation function (pacf) of the white noise series
plot.acf <- function(df, col = 'remainder', is.df =TRUE, decomp = TRUE){
  if(is.df) temp <- df[, col]
  else temp <- df
  par(mfrow = c(2,1))
  if(decomp)
  {
    acf(temp, main = paste('Post decomposition ACF of', col))
    pacf(temp, main = paste('Post decomposition PACF of', col))
  }
  else
  { 
    acf(temp, main = paste('Pre decomposition ACF of', col))
    pacf(temp, main = paste('Pre decomposition PACF of', col))
  } 
  par(mfrow = c(1,1))
}

## Decomposition of the time series into components
ts.decomp <- function(df, col = 'elec.ts', span = 0.5, Mult = TRUE, is.df = TRUE){
  #if(Mult) temp = log(df[, col])  else temp = ts(df[, col]
  if(is.df) temp = log(df[, col])  
  else temp = df
  spans = span * length(temp)  
  fit <- stl(temp, s.window = "periodic", t.window = spans)
  plot(fit, main = paste('Decompositon of',col,'with loess span = ', as.character(span)))
  fit$time.series
}

#Apply ARIMA Model
## Function for ARIMA model estimation
ts.model = function(ts, col = 'remainder', order = c(0,0,1)){
  mod = arima(ts, order = order, include.mean = FALSE)
  print(mod)
  mod
}


# CA Dairy Production data from Assignment.
getwd()
CADairyProd <- read.csv('CADairyProduction.csv', header = TRUE,stringsAsFactors = FALSE)
head(CADairyProd)

Milk.ts = ts(CADairyProd[,5], start = 1995, freq = 12)
lnMilk.ts = log(ts(CADairyProd[,5], start = 1995, freq = 12))
Ice.ts = ts(CADairyProd[,4], start = 1995, freq = 12)
lnIce.ts = ts(log(CADairyProd[,4]), start = 1995, freq = 12)
Cheese.ts = ts(CADairyProd[,3], start = 1995, freq = 12)
lnCheese.ts = ts(log(CADairyProd[,3]), start = 1995, freq = 12)
CADairy.ts = cbind(Milk.ts, lnMilk.ts, Ice.ts, lnIce.ts, Cheese.ts, lnCheese.ts)
## First look at the series
options(repr.pmales.extlot.width=8, repr.plot.height=8)
plot(CADairy.ts)

#Check the distribution
dist.ts(Milk.ts,'Milk')
dist.ts(lnMilk.ts,'Log Milk')
#Check the ACf & PACF
# Milk has a trend component hence it is not stationary.
plot.acf(Milk.ts,'Milk',is.df =FALSE,decomp=FALSE)
plot.acf(lnMilk.ts,'Log Milk',is.df =FALSE,decomp=FALSE)
#Check the distribution
dist.ts(Ice.ts,'Ice Cream')
dist.ts(lnIce.ts,'Log Ice Cream')
#Check the ACf & PACF
# Ice cream has a strong seasonal component, it however does not decay over time, it seems to be maintaining its variance across the time period.
plot.acf(Ice.ts,'Ice Cream',is.df =FALSE,decomp=FALSE)
plot.acf(lnIce.ts,'Log Ice Cream',is.df =FALSE,decomp=FALSE)

#Using multiplicative model in STL decomposition of milk data as it has a strong trend and seasonal aspect.
MilkMult.decomp = ts.decomp(lnMilk.ts, "Log Milk",Mult = TRUE, is.df = FALSE, span = 0.25)
#compute and plot the ACF and PACF for the remainder series.
options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf(MilkMult.decomp[, 3], 'Multiplicative Milk Residual',is.df = FALSE,decomp=TRUE)

#Using additive model in STL decomposition of Ice cream data as it has no trend.
IceAdd.decomp = ts.decomp(Ice.ts, "Ice",Mult = TRUE, is.df = FALSE, span = 0.25)
#compute and plot the ACF and PACF for the remainder series.
options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf(IceAdd.decomp[, 3], 'Additive Ice Residual',is.df = FALSE,decomp=TRUE)

## Compute ARIMA model of Milk remainder.
Milk212.arima = ts.model(MilkMult.decomp[, 3], col = 'ARIMA model for Milk', order = c(2,1,2))
Milk011.arima = ts.model(MilkMult.decomp[, 3], col = 'ARIMA model for Milk', order = c(0,1,1))

#Use forecast to find the optimal seasonal ARIMA Model for Milk
require(forecast)
Milkfit.elect = auto.arima(CADairy.ts[, 'Milk.ts'], max.p=3, max.q=3,
                       max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                       start.p=0, start.q=0, start.P=0, start.Q=0)
summary(Milkfit.elect)

## Make Milk forecast for 2014
Milkelect.forecast = forecast(Milkfit.elect, h=12)
summary(Milkelect.forecast)
plot(Milkelect.forecast)

## Compute ARIMA model of Ice Cream remainder.
Ice101.arima = ts.model(IceAdd.decomp[, 3], col = 'ARIMA model for Ice Cream', order = c(1,0,1))
Ice301.arima = ts.model(IceAdd.decomp[, 3], col = 'ARIMA model for Ice Cream', order = c(3,0,1))

#Use forecast to find the optimal seasonal ARIMA Model for Ice cream
Icecreamfit.elect = auto.arima(CADairy.ts[, 'Ice.ts'], max.p=3, max.q=3,
                           max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                           start.p=0, start.q=0, start.P=0, start.Q=0)
summary(Icecreamfit.elect)

## Make Icecream forecast for 2014
Icecreamelect.forecast = forecast(Icecreamfit.elect, h=12)
summary(Icecreamelect.forecast)
plot(Icecreamelect.forecast)

plot.acf(Icecreamelect.forecast$mean,'Ice cream Production Forecast',is.df = FALSE,decomp=TRUE)
plot.acf(Milkelect.forecast$mean, 'Milk Production Forecast',is.df = FALSE,decomp=TRUE)

