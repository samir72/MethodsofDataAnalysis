rm(list=ls())
# Clear Console:
cat("\014")
library(HistData)
library(resample)
library(simpleboot)
library(dplyr)


male = GaltonFamilies[GaltonFamilies$gender == 'male',]
female = GaltonFamilies[GaltonFamilies$gender == 'female',]

femaledp = GaltonFamilies %>% filter(gender == 'female')
maledp = GaltonFamilies %>% filter(gender == 'male')

plot.dists <- function(a, b, cols = c('pop_A', 'pop_B'), nbins = 20){
  dat = c(a,b)
  maxs = max(dat, na.rm = TRUE)
  mins = min(dat, na.rm = TRUE)
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  par(mfrow = c(2, 1))
  hist(a, breaks = breaks, main = paste('Histogram of', cols[1]), xlab = cols[1])
  abline(v = mean(a), lwd = 4, col = 'red')
  hist(b, breaks = breaks, main = paste('Histogram of', cols[2]), xlab = cols[2])
  abline(v = mean(b), lwd = 4, col = 'red')
  par(mfrow = c(1, 1))
}

plot.dists(male$childHeight, female$childHeight, cols = c('sons', 'daughters'), nbins = 30)

#BootStrap Mean
plot.hist <- function(a, maxs, mins, cols = 'difference of means', nbins = 80, p = 0.05) {
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  hist(a, breaks = breaks, main = paste('Histogram of', cols), xlab = cols)
  abline(v = mean(a), lwd = 4, col = 'red')
  abline(v = 0, lwd = 4, col = 'blue')
  abline(v = quantile(a, probs = p/2), lty = 3, col = 'red', lwd = 3)  
  abline(v = quantile(a, probs = (1 - p/2)), lty = 3, col = 'red', lwd = 3)
}

plot.t <- function(a, b, cols = c('pop_A', 'pop_B'), nbins = 80, p = 0.05){
  maxs = max(c(max(a), max(b)))
  mins = min(c(min(a), min(b)))
  par(mfrow = c(2, 1))
  plot.hist(a, maxs, mins, cols = cols[1])
  plot.hist(b, maxs, mins, cols = cols[2])
  par(mfrow = c(1, 1))
}

## Bootstrap the mean of the sons and of daughters
mean.boot.male = one.boot(male$childHeight, mean, R = 100000)
mean.boot.female = one.boot(female$childHeight, mean, R = 100000)
plot.t(mean.boot.male$t, mean.boot.female$t, nbins = 80)

# Heights of father and sons
father_mean <- mean(male$father)
son_mean <- mean(male$childHeight)

## Bootstrap the mean of the father and son
mean.boot.father_mean = one.boot(male$father, mean, R = 100000)
mean.boot.son_mean = one.boot(male$childHeight, mean, R = 100000)
plot.t(mean.boot.father_mean$t, mean.boot.son_mean$t, nbins = 80)

## Bootstrap the difference in means of sons and daughters
plot.diff <- function(a, cols = 'difference of means', nbins = 80, p = 0.05){
  maxs = max(a)
  mins = min(a)
  plot.hist(a, maxs, mins, cols = cols[1])
}

require(repr)
options(repr.plot.width=6, repr.plot.height=4)
two.boot.father= two.boot(male$childHeight, male$father, mean, R = 100000)
plot.diff(two.boot.father$t)
qqnorm(two.boot.father$t, main = 'Quantiles of standard Normal vs. bookstrapped mean')

## Bootstrap the difference in medians of sons and fathers
options(repr.plot.width=6, repr.plot.height=4)
two.boot.median = two.boot(male$childHeight, male$father, median, R = 100000)
plot.diff(two.boot.median$t)

## Jackknife the mean of the sons and of daughters
mean.jack.male = jackknife(male$childHeight, mean)
mean.jack.male$stats
mean.jack.female = jackknife(female$childHeight, mean)
mean.jack.female$stats
