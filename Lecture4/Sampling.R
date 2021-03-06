rm(list=ls())
# Clear Console:
cat("\014")
# Create and data frame and look at the group size in the sample
data = data.frame(index = 1:200,
                  var1 = rnorm(200),
                  group = sample(1:4, 200, replace=TRUE, prob=c(0.1,0.3,0.4,0.2)))
require(dplyr)
data %>% group_by(group) %>% summarise(n.group = n(), mean.index = mean(var1))

##----Bernoulli Sampling-----
p = 0.1
bernoulli_sample = data[runif(200)<p,] # Yay for R vectorization
  bernoulli_sample %>% group_by(group) %>% summarise(n.group = n(), mean.index = mean(var1))

##-----Stratified Sampling for equal numbers -----
n = 5
stratified_sample = data %>% group_by(group) %>% sample_n(n, replace = FALSE)
stratified_sample %>% group_by(group) %>% summarise(n.group = n(), mean.index = mean(var1))

##----Cluster Sampling-----
num_clusters = 10
data$cluster_labels = sample(rep(1:num_clusters,each=2*num_clusters))
clusters_sampled = sample(1:10, 3) # Pick three clusters to sample
cluster_sample = data[data$cluster_labels %in% clusters_sampled,]
cluster_sample %>% group_by(cluster_labels) %>% summarise(n.group = n(), mean.index = mean(var1))

##-----Law of Large Numbers----
##-----Use rolls of dice-------
## set a probability
p_six = 1/6
p_six
xs = c(10, 100, 1000, 10000, 100000)
sizes = c(60, 600, 6000, 60000, 600000)
# roll the dice and find p(x)
Map(function(x,s) dbinom(x = x, size = s, prob=p_six), xs, sizes)


# Probability of within 5%?
# 1) p(7<x<13|60 trails)
pbinom(12, size=60, prob=p_six) - pbinom(7, size=60, prob=p_six)
sum(sapply(8:12, function(x) dbinom(x, size=60, prob=p_six)))

# alternatively# 2) p(70<x<130|600 trails)
pbinom(129, size=600, prob=p_six) - pbinom(70, size=600, prob=p_six)
# alternatively
sum(sapply(71:129, function(x) dbinom(x, size=600, prob=p_six)))

# View Distributions:
x_60 = 1:60
y_60 = dbinom(x_60, size=60, prob=p_six)

x_600 = 1:150
y_600 = dbinom(x_600, size=600, prob=p_six)

plot(x_60, y_60, type='l', main='Roll a Dice 60 or 600 Times', xlab="# of Successes",
     ylab="Probability", lwd=2, col="green", xlim=c(1,150))
lines(x_600, y_600, lwd=2, col="blue")
legend("topright", c("Roll 60 Times", "Roll 600 Times"), col=c("green", "blue"),
       lty=c(1,1), lwd=c(2,2))

#Convergence of a binomial distribution.

##----Coin Flips-----
# Calculate a running average of N-trials of flipping a fair coin
n = 10000
outcomes = round(runif(n))
running_average = sapply(1:n, function(x) mean(outcomes[1:x]))
plot(running_average, type='l')
grid()

outcomes_sd = sd(outcomes)
outcomes_sd
outcomes_sd_theo = sqrt( 0.5 * (1 - 0.5) )
outcomes_sd_theo

#Convergence of normal distribution
##----St. Dev. vs. St. Error-----
n = seq(10,10000,len=1000)

sample_means = sapply(n, function(x) mean(rnorm(x)))
sample_sds = sapply(n, function(x) sd(rnorm(x)))

plot(n, sample_means) # Plot means
lines(n, 1/sqrt(n))   # Plot means +- st. error
lines(n, -1/sqrt(n))

##----St. Dev. vs. St. Error-----
n = seq(10,10000,len=1000)

sample_means = sapply(n, function(x) mean(rnorm(x)))
sample_sds = sapply(n, function(x) sd(rnorm(x)))

plot(n, sample_sds) # Plot means
lines(n, 1/sqrt(n))   # Plot means +- st. error
lines(n, -1/sqrt(n))
