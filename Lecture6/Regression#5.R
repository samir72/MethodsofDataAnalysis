##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##---- Regression with R ----
##
##--------------------------------------------
# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")
sim.reg.data <- function(x1, y1, x2, y2, n, sd){
  w <- rnorm(n, mean = 0, sd = sd)
  data.frame(
    x = seq(from = x1, to = x2, length.out = n),
    y = (seq(from = y1, to = y2, length.out = n) + w)
  )
}


reg.data = sim.reg.data(0, 0, 10, 10, 50, 1)
head(reg.data)

require(repr)
options(repr.plot.width=5, repr.plot.height=5)
plot.dat <- function(df){
  require(ggplot2)
  ggplot(df, aes(x, y)) + 
    geom_point(size = 2) +
    ggtitle('X vs. Y')
}
plot.dat(reg.data)

mod = lm(y ~ x, data = reg.data)
reg.data$score <- predict(mod, data = reg.data)
reg.data$resids <- reg.data$y - reg.data$score
head(reg.data)

options(repr.plot.width=8, repr.plot.height=4)
plot.regression <- function(df, mod, x = 'x', y = 'y', k = 2, two.plot = TRUE){
  require(ggplot2)
  require(gridExtra)
  
  if(two.plot) {
    p1 <- ggplot(df, aes_string(x, y)) + 
      geom_point(size = 2) +
      geom_line(aes_string(x, 'score'), color = 'Red') + 
      ggtitle('X vs. Y with regression line')
  }
  
  p2 <- ggplot(df, aes(resids)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(color = 'red', fill = 'red', alpha = 0.2) +
    ggtitle('Histogram of residuals')
  
  if(two.plot) {grid.arrange(p1, p2, ncol = 2)}
  else{print(p2)}
  if(k > 2){plot(mod)}
  
  Ybar = mean(df$y)
  SSE <- sum((df$y - Ybar)^2)
  SSR <- sum(df$resids * df$resids)
  cat(paste('SSE =', as.character(SSE), '\n'))
  cat(paste('SSR =', as.character(SSR), '\n'))
  cat(paste('SST =', as.character(SSE + SSR), '\n'))
  cat(paste('RMSE =', as.character(SSE/(nrow(df) - 2)), '\n'))
  if(k == 1 | k == 2){
    n = nrow(df)
    #    adjR2  <- 1.0 - (SSE/SSR) * ((n - 1)/(n - k))
    
    adjR2  <- 1.0 - (SSR/(SSE + SSR)) * ((n - 1)/(n - k))
    cat(paste('Adjusted R^2 =', as.character(adjR2)), '\n')
    cat(paste('Intercept =', as.character(mod$coefficients[1]), '\n'))
    cat(paste('Slope =', as.character(mod$coefficients[2]), '\n'))
  } else {
    cat('\n')
    cat('\n')
    cat('Summary on R Model Object')
    summary(mod)
  }
}
plot.regression(reg.data, mod)

#Scaling is key for regression.

reg.data$x.scale = scale(reg.data$x)
str(reg.data)

mod.scale = lm(y ~ x.scale, data = reg.data)
plot.regression(reg.data, x = 'x.scale', mod.scale)

#Linear model using a straight line for polynomial curve data.
sim.data.ploy <- function(x1, y1, x2, y2, c1 = 1.0, c2 = 0.5, n, sd){
  require(dplyr)
  error <- rnorm(n, mean = 0, sd = sd)
  df = data.frame(
    x = seq(from = x1, to = x2, length.out = n),
    y = (seq(from = y1, to = y2, length.out = n))
  )
  df = df %>% mutate(y = c1 * y + c2 * y^2 + error)
  df$x = scale(df$x)
  df
}
reg.data.poly = sim.data.ploy(0, 0, 10, 10, n = 50, sd = 3)
head(reg.data.poly)

mod.poly = lm(y ~ x + I(x^2), reg.data.poly)
reg.data.poly$score <- predict(mod.poly, data = reg.data.poly)
reg.data.poly$resids <- reg.data.poly$y - reg.data.poly$score
plot.regression(reg.data.poly, mod.poly, k = 3)

#Homoscedastic and Heteroscedastic errora
sim.data.het <- function(x1, y1, x2, y2, n, sd, factor = 5){
  require(dplyr)
  error <- rnorm(n, mean = 0, sd = sd)
  error = error * seq(1, factor, length.out = n)
  df = data.frame(
    x = seq(from = x1, to = x2, length.out = n),
    y = (seq(from = y1, to = y2, length.out = n))
  )
  df = df %>% mutate(y = y + error)
}
reg.data.het = sim.data.het(0, 0, 10, 10, n = 50, sd = 3)

mod.het = lm(y ~ x, data = reg.data.het)
reg.data.het = reg.data.het
reg.data.het$score <- predict(mod.het, data = reg.data.het)
reg.data.het$resids <- reg.data.het$y - reg.data.het$score
plot.regression(reg.data.het, mod.het, k = 2)
summary(mod.het)
plot(mod.het)

#Cooks distance
error.data = rbind(reg.data[, c('x', 'y')], c(0.0, 20.0))
error.data$x = scale(error.data$x)
mod.error = lm(y ~ x, data = error.data)
error.data$score = predict(mod.error, error.data)
error.data$resids = error.data$score - error.data$y
plot.regression(error.data, mod.error, k = 2)
summary(mod.error)
plot(mod.error)

#Cooks distance
error.data = rbind(reg.data[, c('x', 'y')], c(5, 20.0))
error.data$x = scale(error.data$x)
mod.error = lm(y ~ x, data = error.data)
error.data$score = predict(mod.error, error.data)
error.data$resids = error.data$score - error.data$y
plot.regression(error.data, mod.error, k = 2)
summary(mod.error)
plot(mod.error)

#Bootstrap linear model.
## Bootstrap and plot the linear model
require(simpleboot)
reg.data.2 = reg.data
# Scale, but loose the scaling attributes as they break lm.boot
reg.data.2$x = scale(reg.data$x)[1:nrow(reg.data)]  
mod.3 = lm(y ~ x, data = reg.data.2)
mod.boot = lm.boot(mod.3, R = 10000)
plot(mod.boot)

## Plot the histogram of the bootstrapped coeficents
plot.dist <- function(a, name = 'Intercept', nbins = 80, p = 0.05){
  maxs = max(a)
  mins = min(a)
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  hist(a, breaks = breaks, 
       main = paste('Histogram of distribution of the', name), 
       xlab = paste('Values of', name))
  abline(v = mean(a), lwd = 4, col = 'red')
  abline(v = quantile(a, probs = p/2), lty = 3, col = 'red', lwd = 3)  
  abline(v = quantile(a, probs = (1 - p/2)), lty = 3, col = 'red', lwd = 3)
}

## View distribution of model coeficients
intercept = sapply(1:length(mod.boot$boot.list),
                   function(x) mod.boot$boot.list[[x]]$coef[1])
slope = sapply(1:length(mod.boot$boot.list),
               function(x) mod.boot$boot.list[[x]]$coef[2])
par(mfrow = c(1,2))
plot.dist(intercept, name = 'intercept')
plot.dist(slope, name = 'slope')
par(mfrow = c(1,1))
