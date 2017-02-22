# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

library(LearnBayes)
library(repr)
# Compute the beta prior for the driver texting at a specific intersection on the national average.
beta.par <- beta.select(list(p=0.5, x=0.1), list(p=0.75, x=0.3))
#Paremeters of beta prior distribution.
beta.par
options(repr.plot.width=6, repr.plot.height=5)
#Plot the prior with no data.
triplot(beta.par, c(0, 0))
#Plot the prior, likelihood and posterior three times as you update your belief based on collecting more data
#Compute posterior with the below observations.
# 2 out of 20 drivers texting.
beta.par218 <- beta.par + c(2, 18)
triplot(beta.par, c(2, 18))
# 4 out of 20 drivers texting.
beta.par634 <- beta.par + c(6, 34)
triplot(beta.par, c(6, 34))
# 1 out of 20 drivers texting.
beta.par753 <- beta.par + c(7, 53)
triplot(beta.par, c(7, 53))

# Simulate the final posterior distribution
options(repr.plot.width=8, repr.plot.height=5)
beta.post.par <- beta.par + c(7, 53)
post.sample <- rbeta(10000, beta.post.par[1], beta.post.par[2])
par(mfrow = c(1,2))
quants = quantile(post.sample, c(0.05, 0.95))
breaks = seq(min(post.sample), max(post.sample), length.out = 50)
#Plot the posterior with the 90% HDI shown
hist(post.sample, breaks = breaks, 
     main = 'Distribution of samples \n with 90% HDI',
     xlab = 'Sample value',
     ylab = 'Density')
abline(v = quants[1], lty = 3, col = 'red', lwd = 3)
abline(v = quants[2], lty = 3, col = 'red', lwd = 3)
qqnorm(post.sample)
par(mfrow = c(1,1))
#Report the upper and lower limits of the 90% HDI
quants


# Simulate the national numbers
options(repr.plot.width=8, repr.plot.height=5)
beta.nat <- beta.par
nat.sample <- rbeta(10000, beta.nat[1], beta.nat[2])
par(mfrow = c(1,2))
quantsnat = quantile(nat.sample, c(0.05, 0.95))
breaks = seq(min(nat.sample), max(nat.sample), length.out = 41)
#Plot the natinal numbers with the 90% HDI shown
hist(nat.sample, breaks = breaks, 
     main = 'Distribution of national numbers \n with 90% HDI',
     xlab = 'Sample value',
     ylab = 'Density')
abline(v = quantsnat[1], lty = 3, col = 'red', lwd = 3)
abline(v = quantsnat[2], lty = 3, col = 'red', lwd = 3)
qqnorm(nat.sample)
par(mfrow = c(1,1))
#Report the upper and lower limits of the 90% HDI
quantsnat

## Check the model based on local observations.
# sample size = 60
# Observed number of successes yobs 7(2+4+1).
predplot(beta.par, 60, 7)
## Check the model based on national observations.
# sample size = 40
# Observed number of successes yobs .(10+15)
predplot(beta.par, 40,25)


# Using this model forecast the number of drivers texting locally in a group of 100.
n <- 100
s <- 0:n
pred.probs <- pbetap(beta.post.par, n, s)#Using posterior as the observations are available.
plot(s, pred.probs, type="h", 
     main = paste('Probability distribution of successes in', as.character(n), 'local trials'),
     xlab = 'Successes')
discint(cbind(s, pred.probs), 0.90)

# Using this model forecast the number of drivers texting nationally in a group of 100.
n <- 100
s <- 0:n
pred.probsnat <- pbetap(beta.nat, n, s)#Using prior as this is what we have available at this point.
plot(s, pred.probsnat, type="h", 
     main = paste('Probability distribution of successes in', as.character(n), 'national trials'),
     xlab = 'Successes')
discint(cbind(s, pred.probsnat), 0.90)

