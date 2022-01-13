## class three
## Just copying Jon's code, taking notes from what he says

x <- 5

## Both semi-colon and double parentheses are ways to create and print an object.
v <- c(1,3,5,7); v
(v <- c(1,3,5,7))

v[3]

## R assumes any column is a column, not a row
## That's not how they're dealt with in matrix algebra.

# matrices
A <- matrix(data = c(3,-1,0,4,5,3))
another.matrix <- matrix(data = c(3,-1,0,4,5,3), nrow = 3); another.matrix

## Give it a not easily divided number of rows, and it recycles data to fill it in. 
A1 <- matrix(data = c(3,-1,0,4,5,3), nrow = 4); A1

## Can do by row
B <- matrix(data = c(3,-1,0,4,5,3), nrow = 3, byrow = TRUE); B

## Multiply first row of B by ten
B[1,] <- B[1,] * 10

## Matrix addition and subtraction
A <- matrix(data = c(3,-1,0,4,5,2), nrow = 3)
B <- matrix(data = c(4,1,-2,8,-3,6), nrow = 3)
C <- A + B; C

## 'Order doesn't really matter for matrix addition' ##
## 'Just like regular math, order matters for subtraction' ##
## Both happen one element at a time

## 'Scalar multiplication is pretty trivial ##
k <- 3
A <- matrix(data = c(1,2,3,0,2,0,-5,3), nrow = 2)
k * A
A * k
c <- 5
c * (k * A)
(c * k) * A
B <- matrix(data = c(1,2,3,0,2,0,-5,3), nrow = 2)
k * (A + B)
k * A + k * B

## Matrix multiplication
A <- matrix(data = c(1,2,3,0,3,-2), nrow = 2)
# A is a 2x3 matrix
B <- matrix(data = c(1,3,-1,-2,2,3,2,2,3), nrow = 3)
# B is a 3x3 matrix

## 'If the inner dimensions match, the outer dimensions will give the dimensionality of the new object. 

A %*% B

## If a matrix is symmetric, A - t(A) gives you a matrix of zeros

## We can pull the matrix diagonal out using the diag() function

A <- matrix(data = c(1,2,2,2,4,3,2,3,-4), nrow = 3)
# What are the dimensions of this matrix?

A
t(A)
A - t(A)

I <- diag(x = 4); I

A
solve(a = A)
A %*% solve(a = A)

##### class data
chlorella <- read.csv("data/chlorella.csv", header = TRUE,
                      row.names = 1)
chlorella

X <- matrix(data = c(rep(1,11), chlorella$x), nrow = 11)
X
dim(X)

solve(t(X) %*% X)

b <- solve(t(X) %*% X) %*% (t(X) %*% chlorella$y)

library(ggplot2)
gg <- ggplot(data = chlorella, aes(x = x, y = y)) +
  geom_point() +
  geom_abline(intercept = b[1], slope = b[2]) +
  theme_bw()

predict(gg, 1)
