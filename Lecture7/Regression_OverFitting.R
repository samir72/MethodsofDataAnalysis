# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")
# Create a data set
require(HistData)
require(dplyr)
males = GaltonFamilies[GaltonFamilies$gender == 'male',]
males.ext = males[, c('mother', 'father', 'childHeight')]
males.ext = mutate(males.ext, mother.sqr = mother^2, father.sqr = father^2)
males.ext[, c('mother', 'father', 'mother.sqr', 'father.sqr')] = 
  lapply(males.ext[, c('mother', 'father', 'mother.sqr', 'father.sqr')], 
         scale)
str(males.ext)
# Compute a model to gauge Child's height based on all the features in males.ext
lm.males = lm(childHeight ~ ., data = males.ext)
summary(lm.males)
plot(lm.males)

# Model is overparameterized, apply stepwise regression.
library(MASS)
lm.step = stepAIC(lm.males, direction = 'both')
lm.step$anova # ANOVA of the result 
summary(lm.step) # Summary of the best model
plot(lm.step)

#Moore Penrose inverse.
cat('Create a matrix of random values')
C = matrix(rnorm(9), nrow = 3, ncol = 3)
C
cat('Compute the SVD and look at the sigular values')
CSVD = svd(C)
CSVD$d
cat('The inverse matrix of sigular values')
D = diag(1/CSVD$d)
D
cat('The pseudo inverse of the matrix')
cInv = CSVD$v %*% D %*% t(CSVD$u)
cInv
cat('The pseudo inverse times the matrix')
cInv %*% C

# 4*4 matrix
cat('Create a matrix of random values')
C = matrix(rnorm(16), nrow = 4, ncol = 4)
C[, 4] = 0.4 * C[, 1] + 0.2 * C[, 2] + 0.4 * C[, 3]
C
cat('Compute the SVD and look at the sigular values')
CSVD = svd(C)
CSVD$d
cat('The inverse matrix of sigular values')
D = diag(1/CSVD$d)
D
cat('The pseudo inverse of the matrix')
cInv = CSVD$v %*% D %*% t(CSVD$u)
cInv
cat('The pseudo inverse times the matrix')
cInv %*% C
