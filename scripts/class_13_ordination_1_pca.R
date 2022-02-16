require(here)
require(dplyr)

## Eigenvector formula
#Ax = lamba(x)

A = matrix(c(3, 1, 1, 0.5), nrow = 2)
A

E <- eigen(A); E
Evalues <- E$values
Evectors <- E$vectors

Evectors
Evalues

## The important aspect of Eigenvalues is if you add them up, 
## you'll get the sum of the matrix diagonals.
## Total sum of diagonals adds up to the total sum of the variance.

## Always as many Eigenvalues as there are rows or columns.
## They're a declining function -> first is always the biggest.

## These are the two multiplications that will satisfy the equation:
#Ax = lamba(x)
A %*% Evectors[,1]
Evalues[1]*Evectors[,1]

darl <- read.csv(here('data/Darlingtonia_GE_Table12.1.csv'), stringsAsFactors = TRUE)
str(darl)

darl.data <- darl[ , ! colnames(darl) %in% c('site', 'plant')]
str(darl.data)

hist(darl$height, breaks = 15)
hist(darl$wingsprea, breaks = 15)
hist(darl$tube.diam, breaks = 15)
hist(darl$keel.diam, breaks = 15)

scaled.data <- scale(darl.data)
s <- cov(scaled.data);s
s %>% round(3)

p <- cor(darl.data) %>% 
  round(2)

s.eigen <- eigen(p)
s.eigen

s.eigen$values
sum(s.eigen$values)

## Gives us the proportion of the variance explained by each principle component (eigenvalue)
s.eigen.prop <- s.eigen$values / sum(s.eigen$values)
s.eigen.prop

## eigenvalues: columns are eigenvectors, rows are variables.
## 'loadings'
## Can use to explain the variance in the eigenvalues. 
s.eigen$vectors

PC1 <- scaled.data[1,] %*% s.eigen$vectors[,1]
PC1

PC.scores <- scaled.data %*% s.eigen$vectors
PC.scores

## If we retain all axes, then there should be zero correlation between all the axes,
## without changing the fundamental relationships in the data. 
## Even though I'm getting some minor correlation here lolol, Jon isn't.
cor(PC.scores) %>% round(3)

loadings <- round(s.eigen$vectors[,1:3], digits = 3)
rownames(loadings) <- colnames(darl.data)
colnames(loadings) <- c('PC1', 'PC2', 'PC3')
loadings

## Number of functions to do PCA
# princomp
darl.PCA <- princomp(darl.data, cor = TRUE)
darl.PCA
summary(darl.PCA)
str(darl.PCA)
darl.PCA$loadings

darl.PCA2 <- prcomp(darl.data)
darl.PCA2

biplot(darl.PCA)

## there's a ggbiplot package

## the more highly correlated data are, the better pca will function