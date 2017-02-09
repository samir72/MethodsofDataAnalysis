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
auto.price = read.auto()

require(dplyr)
df = filter(auto.price, auto.price$price > 1000)
df

df.slice = slice(auto.price, 20:30)
df.slice?

df.rand = sample_frac(df.slice, 0.2)
df.rand

df2 = select(df,wheel.base,curb.weight,horsepower,price)
df2 = select(df, drive.wheels, wheel.base, curb.weight, horsepower, price)
df2

df2 = arrange(df,body.style,make)
df2

df2 = auto.price %>% 
  filter(make == 'audi') %>% 
  select(drive.wheels, wheel.base, curb.weight, engine.size, price)
df2

temp = auto.price %>% filter(make == 'dodge') %>%
  mutate(weight.kg = curb.weight / 2.2) %>%
  mutate(weight.kg.hp = weight.kg / horsepower)
temp

temp = auto.price %>% 
  group_by(make)%>% 
  summarise(count = n(), mean.price = mean(price, na.rm = TRUE), sd.price = sd(price, na.rm = TRUE), 
            max.price = max(price), min.price = min(price))
