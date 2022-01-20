vector <- c(1,7)
V <- matrix(c(1,7)) #capital 'V'
M <- matrix(c(5,10,2,1,7,8), nrow=2)

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

dist(M)
