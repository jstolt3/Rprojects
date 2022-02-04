library("sandwich")
library("parallel")

set.seed(109) # Sets the "seed" of the random number generators:
n <- 100 # Number of observations
## Generate two explanatory variables plus an intercept-variable:
X_1 <- rep(1, n) # Intercept
X_2 <- rnorm(n, mean=10, sd=1.5) # Draw realizations form a normal distr.
X_3 <- runif(n, min = 0.2, max = 8) # Draw realizations form a t-distr.
X <- cbind(X_1, X_2, X_3) # Save as a Nx3-dimensional data matrix.
beta <- c(1, -5, 5)
## Generate realizations from the heteroscadastic error term
eps <- rnorm(n, mean=0, sd=abs(X_3))
## Dependent variable:
Y <- X %*% beta + eps

Var_theo <- solve(t(X) %*% X) %*% t(X) %*% diag(X_3^2) %*%
  X %*% solve(t(X) %*% X)
rownames(Var_theo) <- c("", "", "") # remove row-names
colnames(Var_theo) <- c("", "", "") # remove col-names

library("sandwich") # HC robust variance estimation
MC_reps <- 10000 # Number of Monte Carlo replications
VarHC3_estims <- matrix(NA, 3, MC_reps) # Container to collect the results

for(r in 1:MC_reps){
  ## Generate new realizations from the heteroscedastic error term
  eps <- rnorm(n, mean=0, sd=abs(X_3))
  ## Generate new realizations from the dependent variable:
  Y <- X %*% beta + eps
  ## Now OLS estimation
  lm_fit <- lm(Y ~ X)
  ## Now robust estimation of the variance of \hat\beta:
  VarHC3_estims[,r] <- diag(sandwich::vcovHC(lm_fit, type="HC3"))
}

##Function for parallel computation
MC_fx <- function(trials){
  ## Generate new realizations from the heteroscedastic error term
  eps <- rnorm(n, mean=0, sd=abs(X_3))
  ## Generate new realizations from the dependent variable:
  Y <- X %*% beta + eps
  ## Now OLS estimation
  lm_fit <- lm(Y ~ X)
  ## Now robust estimation of the variance of \hat\beta:
  VarHC3_estims[,r] <- diag(sandwich::vcovHC(lm_fit, type="HC3"))
}



VarHC3_estims_means <- rowMeans(VarHC3_estims)
## Compare the theoretical variances Var(\hat\beta_2) and Var(\hat\beta_3)
## with the means of the 10000 variance estimations
## \hat{Var}(\hat\beta_2) and \hat{Var}(\hat\beta_3)
cbind(diag(Var_theo)[c(2,3)], VarHC3_estims_means[c(2,3)])
## [,1] [,2]
## 0.06923203 0.07232364
## 0.05734246 0.05935843
plot(x=c(1,2), y=c(0,0), ylim=range(VarHC3_estims[c(2,3),]), type="n", axes = FALSE,
     xlab = "", ylab = "")
box()
axis(1, c(1,2), labels=c(2,3))
axis(2)
points(x=rep(1,MC_reps), y=VarHC3_estims[2,], pch=21, col=gray(.5,.25), bg=gray(.5,.25))
points(x=1, y=VarHC3_estims_means[2], pch=21, col="black", bg="black")
points(x=1, y=diag(Var_theo)[2], pch=1)
points(x=rep(2,MC_reps), y=VarHC3_estims[3,], pch=21, col=gray(.5,.25), bg=gray(.5,.25))
points(x=2, y=VarHC3_estims_means[3], pch=21, col="black", bg="black")
points(x=2, y=diag(Var_theo)[3], pch=1)
legend("top",
       legend = c("10000 Variance estimates", "Mean of variance estimates",
                  "True Variance"), bty = "n", pt.bg = c(gray(.5,.75),"black","black"),
       pch = c(21,21,1), col=c(gray(.5,.75),"black","black"))