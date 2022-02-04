# install.packages("AER")
library("AER")
## attach the data-set Journals to the current R-session
data("Journals", package = "AER")
## ?Journals # Check the help file
##
## Select variables "subs" and "price"
journals <- Journals[, c("subs", "price")]
## Define variable 'journal-price per citation'
journals$citeprice <- Journals$price/Journals$citations
## Define variable 'journal-age'
journals$age <- 2020 - Journals$foundingyear
## Check variable names in 'journals'
names(journals)

lm_result_single <- lm(log(journals$subs) ~ log(journals$citeprice))
resid_1 <- resid(lm_result_single)
plot_resid <- plot(y = resid_1, x = fitted(lm_result_single),
     ylab="Residuals",
     xlab="Subs",
     main="Residuals for Journal Subs")
abline(0, 0)

library("sandwich") # HC robust variance estimation
## Robust estimation of the variance of \hat\beta:
Var_hat_beta_HC3 <- sandwich::vcovHC(lm_result_single, type="HC3")
## Robust standard error of \hat\beta_2
sqrt(diag(Var_hat_beta_HC3)[2])
## log(citeprice)
## 0.03447364
## Comparison with the classic standard error estimation
sqrt(diag(vcov(lm_result_single))[2])
