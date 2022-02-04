#Problem 1 part b
n <- 150
beta_true <- c(3,3,3)
X_2 <- runif(n, 2, 10)
X_3 <- runif(n, -10, -2)
X <- cbind(rep(1, n), X_2, X_3)
eps <- rnorm(n, sd = sqrt(X_2))
Y <- X %*% beta_true + eps
mydata <- data.frame("Y"=Y, "X_2"=X[,2], "X_3"=X[,3])
reg_results <- lm(Y ~ X - 1)
summary(reg_results)

#part c
myDataGenerator <- function(n, beta){
  ##
  X_2 <- runif(n, 2, 10)
  X_3 <- runif(n, -10, -2)
  X <- cbind(rep(1, n), X_2, X_3)
  eps <- rnorm(n, sd = sqrt(X_2))
  Y <- X %*% beta_true + eps
  mydata <- data.frame("Y"=Y, "X_2"=X[,2], "X_3"=X[,3])
  ##
  return(mydata)
}
n <- 100
beta_true <- c(3,3,3)
rep <- 500 # MC replications
beta_hat_2 <- rep(NA, times=rep)
##
for(r in 1:rep){
  MC_data <- myDataGenerator(n= n, beta = beta_true)
  lm_obj <- lm(Y ~ X_2 + X_3, data = MC_data)
  beta_hat_2[r] <- coef(lm_obj)[2]
}
sd(beta_hat_2)
##
#problem 2
library("wooldridge") # data mroz
data("mroz")
