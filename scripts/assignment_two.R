## Don Radcliffe
## Script to support assignment two, SEFS 502
## Jon Bakker
## January 19th, 2022

vector <- c(1,7)
V <- matrix(c(1,7)) #capital 'V'
M <- matrix(c(5,10,2,1,7,8), nrow=2)

##### Question 1 #####

t(V) %*% M

V%*%M

vector%*%M

M%*%vector

M%*%M

M%*%t(M)

t(M)%*%M

V%*%V

vector%*%vector

V%*%vector

vector%*%V

###### Question Two ######

dist(M, method = 'euc')

## Why are decostand results different than calculating manually?  I went with the manual calculation. 
M_relativized_by_max <- M/max(M)
M_relativized_by_max
decostand(M, method = 'max')

dist(M_relativized_by_max, method = 'euc')
dist(M, method = 'euc')

##### Question Three ######
vegdist(M, method = 'bray')
vegdist(M_relativized_by_max, method = 'bray')
