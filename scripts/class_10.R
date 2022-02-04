## Class ten
## Cluster Analysis
## Don Radcliffe
## Feb 4 2022

eg <- read.table('data/cluster.example.txt')
eg.dist <- as.dist(eg)
eg.hclust <- hclust(eg.dist, method = 'single')
plot(eg.hclust)

cophenetic(eg.hclust)
cor(eg.dist, cophenetic(eg.hclust))
plot(eg.dist, cophenetic(eg.hclust))
abline(a = 0, b = 1)

## Jon designed this function to make a scree plot,
## to look for sharp bends in the plot,
## script in scripts folder.
source('scripts/scree.R')
scree

source('scripts/load.oak.data.R')
Oak1.hclust <- hclust(d = Oak1.dist, method = 'ward.D2')

scree(hclust.obj = Oak1.hclust)

## Cuttree function to organize your data into groups
g6 <- cutree(Oak1.hclust, k = 6)
g6

plot(Oak1.hclust, horiz = TRUE)

require(dendextend)
require(tidyverse)

Oak1.hclust %>%
  set('branches_k_color', k = 6) %>%
  set('labels_col', k = 6) %>%
  plot

Oak1.hclust %>%
  rect.dendrogram(Oak1.hclust, k = 6, border = 8, lty = 5, lwd = 1)

## Heat map
tabasco(x = vegtab(Oak1, minval = nrow(Oak1)/2), use = Oak1.hclust)

## kmeans function to specify how many groups we want
k6 <- kmeans(x = Oak1, centers = 6, nstart = 100)
k6
k6$cluster

## cascade KM tests a range of groups
## here is lower value 2, upper value 8, for groups
plot(cascadeKM(Oak1, inf.gr = 2, sup.gr = 8))
## Calinski criterion is supposed to tell us which level of grouping has the strongest support

## pam function can use a distance matrix