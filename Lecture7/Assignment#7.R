rm(list=ls())
# Clear Console:
cat("\014")
library(dplyr)
library(repr)
read.auto = function(file = 'Automobile price data _Raw_.csv'){
  ## Read the csv file
  auto.price <- read.csv(file, header = TRUE, 
                         stringsAsFactors = FALSE)
  
  ## Coerce some character columns to numeric
  numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
  auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
  
  ## Remove columns or rows with missing values. In this case we keep the 
  ## rows which do not have nas. 
  auto.price[complete.cases(auto.price), ]
}

# Load and cleanse the csv file.
auto.price = read.auto()
#Remove first two columns of auto.price$symboling & auto.price$normalized.losses
auto.price[1:2] <- list(NULL)

#z-Score Normalization of price data
mean_price <- mean(auto.price$price)
sd_price <- sd(auto.price$price)
normalized_zprice <- (auto.price$price - mean_price) / sd_price

#Add normalized price to the data frame.
auto.price = mutate(auto.price,normalized_zprice = normalized_zprice)
dim(auto.price)
str(auto.price)

#Compute a model with all features.
lm.auto.price = lm(normalized_zprice ~ . -price, data = auto.price)
# Removing price from the formula as I have included normalized price in the data frame.
summary(lm.auto.price)
plot(lm.auto.price)

#Apply Stepwise Regression
library(MASS)
lm.step = stepAIC(lm.auto.price, direction = 'both')
lm.step$anova # ANOVA of the result 
summary(lm.step) # Summary of the best model
plot(lm.step)

# Apply Singular Value Decomposition using Pseudo Inverse.
# This data frame consists of number of catgegorical variables,in order to work with a model matrix we first need to convert these categorical 
#variables into binary indicator variables.
mod.mat = model.matrix(normalized_zprice ~ . -price, data = auto.price)
mod.mat[1:10, ]
# Remove intercept
mod_withoutintercept.mat <- mod.mat[1:195,-1]
M = as.matrix(mod_withoutintercept.mat)
head(M)
MTM = t(M) %*% M
MTM
dim(MTM)
#compute the SVD of model matrix M (195 * 60)
# Examine the singular values and in the process check for "Rank Deficiency".
mSVD <- svd(MTM)
#Validate whether singular vectors are orthogonal.
uOrth <- t(mSVD$u) %*% mSVD$u
vOrth <- mSVD$v %*% t(mSVD$v)
uOrthSingLeftVector <- mSVD$u
vOrthSingRightVector <- mSVD$v
diagSDV <- mSVD$d
diagSDV
#By looking at singlular values we can now deduce that this matrix is rank deficient with 15 values (45 - 59) below zero.
# Let us now compute the pseudo inverse of MTM matrix.
cat('Compute and print the inverse singular value matrix')
d.trim = rep(0, 59)
d.trim[1:44] =1/ mSVD$d[1:44]
mD = diag(d.trim)
mD
cat('Compute and print the pseudo inverse')
mInv = mSVD$v %*% mD %*% t(mSVD$u)
mInv
cat('Compute and print the dimensions of the matrix MInvM')
MInvM = mInv %*% t(M)
dim(MInvM)

# Compute the vector of model coefficients by multiplying MInvM with normalized auto price.
b <- MInvM %*% auto.price$normalized_zprice
dim(b)

# Now we can evaluate the model using this vector of model coefficient.
auto.price$score = M %*% b + mean(auto.price$normalized_zprice)
auto.price$resids = auto.price$score - auto.price$normalized_zprice

require(repr)
options(repr.pmales.extlot.width=8, repr.plot.height=4)

plot.svd.reg <- function(df, k = 4){
  require(ggplot2)
  require(gridExtra)
  
  p1 <- ggplot(df) + 
    geom_point(aes(score, resids), size = 2) + 
    stat_smooth(aes(score, resids)) +
    ggtitle('Residuals vs. fitted values')
  
  p2 <- ggplot(df, aes(resids)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(color = 'red', fill = 'red', alpha = 0.2) +
    ggtitle('Histogram of residuals')
  
  qqnorm(df$resids)
  
  grid.arrange(p1, p2, ncol = 2)
  
  df$std.resids = sqrt((df$resids - mean(df$resids))^2)  
  
   p3 = ggplot(df) + 
    geom_point(aes(score, std.resids), size = 2) + 
    stat_smooth(aes(score, std.resids)) +
    ggtitle('Standardized residuals vs. fitted values')
  print(p3) 
  
  n = nrow(df)
  Ybar = mean(df$normalized_zprice)
  SST <- sum((df$normalized_zprice - Ybar)^2)
  SSR <- sum(df$resids * df$resids)
  SSE = SST - SSR
  cat(paste('SSE =', as.character(SSE), '\n'))
  cat(paste('SSR =', as.character(SSR), '\n'))
  cat(paste('SST =', as.character(SSE + SSR), '\n'))
  cat(paste('RMSE =', as.character(SSE/(n - 2)), '\n'))
  
  adjR2  <- 1.0 - (SSR/SST) * ((n - 1)/(n - k - 1))
  cat(paste('Adjusted R^2 =', as.character(adjR2)), '\n')
}

plot.svd.reg(auto.price)

#Elastic Net Regression.
require(glmnet)
b = as.matrix(auto.price$normalized_zprice)
mod.ridge = glmnet(M, b, family = 'gaussian', nlambda = 20, alpha = .5)
plot(mod.ridge, xvar = 'lambda', label = TRUE)
plot(mod.ridge, xvar = 'dev', label = TRUE)

#Let us now evaluate the model created by elastice net regression by calculating the score using predict function.

auto.price$score = predict(mod.ridge, newx = M)[, 20]
auto.price$resids = auto.price$score - auto.price$normalized_zprice

plot.svd.reg(auto.price)
