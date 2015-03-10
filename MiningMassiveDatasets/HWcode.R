# Homework 4A
# Question 1
# Normalization
# subtracting the average for each row and then subtracting the average for each column
A = matrix(c(1,2,5,2,3,5,3,2,5,4,5,3,5,3,2),nrow=3)
B = A
for (i in 1:nrow(A)) {
  rmean = mean(A[i,])
  B[i,] = A[i,] - rmean
}
C = B
for (i in 1:ncol(B)) {
  cmean = mean(B[,i])
  C[,i] = B[,i] - cmean
}

# Question 2
# cosine distance function: dot product / sqrt(length1 * length2)
AB <- function (a) {
  (2 + 12 * a^2) / sqrt((3 + 4 * a^2) * (3 + 36 * a^2))
}
BC <- function (a) {
  (1 + 12 * a^2) / (sqrt(3 + 36 * a^2) * sqrt(2 + 4 * a^2))
}
AC <- function (a) {
  4 * a^2 / (sqrt(3 + 4 * a^2) * sqrt(2 + 4 * a^2))
}
AB(0)
BC(0)
AC(0)
AB(1)
BC(1)
AC(1)
AB(2)
BC(2)
AC(2)
AB(0.5)
AC(0.5)
BC(0.5)

# homework 4B
# Question 1
U = matrix(c(2/7, 3/7, 6/7, -.548, .401, .273), ncol=2)
U = matrix(c(2/7, 3/7, 6/7, .429, .857, .286), ncol=2)
U = matrix(c(2/7, 3/7, 6/7, -.937, .312, .156), ncol=2)
U = matrix(c(2/7, 3/7, 6/7, .954, .728, -.682), ncol=2)

t(U) %*% U

# Question 3
d = matrix(c(1,1,2,2,3,4),nrow = 3,byrow=T)
t(d) %*% d

N = 100000
M = 100000000
2 * N^2
12 * M