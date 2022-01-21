require(vegdist)

test <- read.table("data/Legendre.Legendre.2012.p311.txt",
                   header = TRUE)
#To see the Euclidean distances among plots in test:
  (ED.test <- dist(test))
#Note that the result is displayed as a lower triangular matrix. What class is this object? It can
#easily be converted to a full matrix:
  as.matrix(ED.test)
  
dist(test)
dist(test, method = 'manhattan')

vegan::vegdist()