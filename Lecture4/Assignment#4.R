rm(list=ls())
# Clear Console:
cat("\014")
read.auto = function(file = 'Automobile price data _Raw_.csv'){
  ## Read the csv file
  auto.price <- read.csv(file, header = TRUE, 
                         stringsAsFactors = FALSE)
  
  ## Coerce some character columns to numeric
  numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
  auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
  
  ## Remove cases or rows with missing values. In this case we keep the 
  ## rows which do not have nas. 
  auto.price[complete.cases(auto.price), ]
}

# Load and cleanse the csv file.
auto.price = read.auto()
# Get price data in a vector
price <- auto.price$price
#Log of price data
logprice <- log(price)

#z-Score Normalization of price data
mean_price <- mean(price)
sd_price <- sd(price)
normalized_price <- (price - mean_price) / sd_price
normalized_price

#z-Score Normalization of log price data
mean_logprice <- mean(logprice)
sd_logprice <- sd(logprice)
normalized_logprice <- (logprice - mean_logprice) / sd_logprice
normalized_logprice

#Add log price and normalized log price to the data frame.
require(dplyr)
auto.price = mutate(auto.price, logprice = logprice, normalized_price = normalized_price, normalized_logprice = normalized_logprice)

# QQ plot for price to visually check normality
par(mfrow = c(1, 2))
qqnorm(normalized_price, main = 'Q-Q plot(Normalized price)')

# QQ plot for logprice to visually check normality
par(mfrow = c(1, 2))
qqnorm(normalized_logprice, main = 'Q-Q plot(Normalized log price)')


## Plot the cdfs of standard data.
plot(ecdf(price), col='blue', main ='CDFs of Price', 
     xlab = 'Value', ylab = 'Cumulative density')
lines(ecdf(logprice), col='red')

## Plot the cdfs on normalized data
plot(ecdf(normalized_price), col='blue', main ='CDFs of Normalized Price', 
     xlab = 'Value', ylab = 'Cumulative density')
lines(ecdf(normalized_logprice), col='red')
#K-S Test
ks.test(normalized_price,normalized_logprice)
ks.test(price,logprice)


#Stratifying the data.
n = 5
#Group By Fuel Type
stratified_pricefuel = auto.price %>% group_by(fuel.type) %>% sample_n(n, replace = FALSE)
logpricegroupedbyfueltypegas = stratified_pricefuel %>% filter(fuel.type == 'gas')
logpricegroupedbyfueltypediesel = stratified_pricefuel %>% filter(fuel.type == 'diesel')
#Group by Aspiration
stratified_priceasp = auto.price %>% group_by(aspiration) %>% sample_n(n, replace = FALSE)
logpricegroupedbyaspirationstd = stratified_priceasp %>% filter(aspiration == 'std')
logpricegroupedbyaspirationsturbo = stratified_priceasp %>% filter(aspiration == 'turbo')
#Group by drive wheel
stratified_pricedrive = auto.price %>% group_by(drive.wheels) %>% sample_n(n, replace = FALSE)
logpricegroupedbydrivewheelfwd = stratified_pricedrive %>% filter(drive.wheels == 'fwd')
logpricegroupedbydrivewheelrwd = stratified_pricedrive %>% filter(drive.wheels == 'rwd')
logpricegroupedbydrivewheel4wd = stratified_pricedrive %>% filter(drive.wheels == '4wd')


## Student T-Test to gauge the impact on the fule price.
#Gauge impact of fuel type on price
t.test(logpricegroupedbyfueltypegas$normalized_logprice, auto.price$normalized_logprice, alternative = "two.sided")
t.test(logpricegroupedbyfueltypediesel$normalized_logprice, auto.price$normalized_logprice, alternative = "two.sided")
#Gauge impact of drive wheel on price
t.test(logpricegroupedbydrivewheel4wd$normalized_logprice, auto.price$normalized_logprice, alternative = "two.sided")
t.test(logpricegroupedbydrivewheelfwd$normalized_logprice, auto.price$normalized_logprice, alternative = "two.sided")
t.test(logpricegroupedbydrivewheelrwd$normalized_logprice, auto.price$normalized_logprice, alternative = "two.sided")
#Gauge impact of aspiration on price
t.test(logpricegroupedbyaspirationstd$normalized_logprice, auto.price$normalized_logprice, alternative = "two.sided")
t.test(logpricegroupedbyaspirationsturbo$normalized_logprice, auto.price$normalized_logprice, alternative = "two.sided")

#Anova Test
#Stratify data as per number of doors and body style.

#Group By number of doors
stratified_pricedoors = auto.price %>% group_by(num.of.doors) %>% filter(num.of.doors != '?') %>% sample_n(n, replace = FALSE)
#Group by body style
stratified_pricebody = auto.price %>% group_by(body.style) %>% sample_n(n, replace = FALSE)

df_aov_door = aov(auto.price$normalized_logprice ~ auto.price$num.of.doors, data = stratified_pricedoors)
summary(df_aov_door)

df_aov_door = aov(auto.price$normalized_logprice ~ auto.price$drive.wheels, data = stratified_pricedoors)
summary(df_aov_door)
