#Number 1
A <- matrix(c(3, 1, 2, 0), 2, 2, byrow = TRUE)
B <- matrix(c(6, 7, -3, -2, -1, 4), 2, 3, byrow = TRUE)
#Number 2
C <- matrix(c(1, 0, 3, 2, 5, 0, -1, -6, 4, 0, 2, -2), 3, 4, byrow = TRUE)
y <- qr(C)