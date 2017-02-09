rm(list=ls())
# Clear Console:
cat("\014")
prob_normal = function(a, b, mean=0, sd=1){
  stopifnot(a<=b) # Test input condition
  return(pnorm(b,mean,sd) - pnorm(a,mean,sd))
}

#One tailed
prob_normal(20.1262055, Inf, 15, 4) # 10% of the area lies to the left of 20.1262055 on N(15,4)
#prob_normal(20.1262055, 21, 15, 4) # 10% of the area lies to the left of 20.1262055 on N(15,4)

pnorm(1) - pnorm(-1)

norm1 = rnorm(100,mean=0,sd=1)
norm2 = rnorm(100,mean=0,sd=1)

a <- mean(norm1)
b <- sd(norm1)
normalized <- (norm1 - a) / b
normalized

## Plot the cdfs
plot(ecdf(norm1), col='blue', main ='CDFs of samples', 
     xlab = 'Value', ylab = 'Cumulative density')
lines(ecdf(norm2), col='red')


# Have to standardize the x-values
x_seq = seq(-3,3,len=100)
y_cdf1 = sapply(x_seq, function(x){
  sum(norm1<x)/length(norm1)
})
y_cdf2 = sapply(x_seq, function(x){
  sum(norm2<x)/length(norm1)
})

plot(x_seq,y_cdf1, col='blue', pch=16, main ='CDFs of standardized samples', 
     xlab = 'Value', ylab = 'Cumulative density')
points(x_seq,y_cdf2,col='red', pch=16)

## Find the max deviation
k_s_stat = max(abs(y_cdf1 - y_cdf2))
k_s_stat
# where does it occur?
k_index = which.max(abs(y_cdf1-y_cdf2))
k_s_x = x_seq[k_index]
plot(x_seq,y_cdf1, col='blue', pch=16, main ='CDFs of standardized samples', 
     xlab = 'Value', ylab = 'Cumulative density')
points(x_seq,y_cdf2,col='red', pch=16) 
lines(c(k_s_x,k_s_x), c(y_cdf1[k_index],y_cdf2[k_index]),
      col='black', lwd=8)


# Create k-s statistic function
ks_stat = function(x_min,x_max, dist_a, dist_b){
  x_seq = seq(x_min,x_max,len=1000)
  y_cdf1 = sapply(x_seq, function(x){
    sum(dist_a<x)/length(dist_a)
  })
  y_cdf2 = sapply(x_seq, function(x){
    sum(dist_b<x)/length(dist_b)
  })
  k_s_stat = max(abs(y_cdf1-y_cdf2))
  return(k_s_stat)
}


##----Repeat N Times-----
N = 1000
k_s_rep = sapply(1:N, function(i){
  dist_a = rnorm(100,mean=0,sd=1)
  dist_b = rnorm(100,mean=0,sd=1)
  return(ks_stat(-3, 3, dist_a, dist_b))
})

hist(k_s_rep, breaks=30, freq=FALSE, xlab = 'K-S statistic',
     main = 'Histogram of k-s statistic')
lines(density(k_s_rep))

#ANOVA
df = data.frame('group'=c(rep(1,50),
                          rep(2,50),
                          rep(3,60),
                          rep(4,40)),
                'val' = c(rnorm(50, mean=0, sd=1),
                          rnorm(50, mean=0, sd=1),
                          rnorm(60, mean=0.5, sd=1),
                          rnorm(40, mean=0, sd=1)))
df$group = factor(df$group) # Make sure your groups are a factor (for further analysis below)
boxplot(df$val ~ df$group)

#Applying R function to four groups
df_aov = aov(val ~ group, data = df)
summary(df_aov)

tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova

plot(tukey_anova)