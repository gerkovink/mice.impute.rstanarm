symm <- function(x){
  (x + t(x))/2
}
.stan.draw <- function(y, ry, x, ridge = 1e-05, ...){
  noprint <- capture.output(fit <- stan_glm(y ~ x[,-1], algorithm = "optimizing"))
  coef <- as.matrix(coef(fit))
  residuals <- residuals(fit)
  df <- fit$df.residual
  sigma.star <- sqrt(sum((residuals)^2)/rchisq(1, df))
  cp <- crossprod(model.matrix(fit))
  pen <- ridge * diag(cp)
  beta.star <- coef + (t(chol(symm(solve(cp)))) %*% rnorm(ncol(cp))) * sigma.star
  parm <- list(coef, beta.star, sigma.star)
  names(parm) <- c("coef", "beta", "sigma")
  return(parm)
}

mice.impute.rstanarm <- function(y, ry, x, ...){
  x <- cbind(1, as.matrix(x))
  parm <- .stan.draw(y, ry, x)
  return(x[!ry, ] %*% parm$beta + rnorm(sum(!ry)) * parm$sigma)
}

#test
require(mice)
require(rstanarm)
imp1 <- mice(boys[,1:5], method = "rstanarm", m=5)
imp2 <- mice(boys[,1:5], method = "norm", m=5)
summary(pool(with(imp1, lm(bmi ~ age + wgt + hgt))))
summary(pool(with(imp2, lm(bmi ~ age + wgt + hgt))))

#test 
boys2 <- na.omit(boys[1:5])
boys2[sample(1:nrow(boys), 150), 2] <- NA
boys2[sample(1:nrow(boys), 150), 3] <- NA
boys2[sample(1:nrow(boys), 150), 4] <- NA
boys2[sample(1:nrow(boys), 150), 5] <- NA
imp3 <- mice(boys2, method = "rstanarm", m=50)
imp4 <- mice(boys2, method = "norm", m=50)
summary(lm(bmi ~ age + wgt + hgt, data = na.omit(boys[1:5])))
summary(pool(with(imp3, lm(bmi ~ age + wgt + hgt))))
summary(pool(with(imp4, lm(bmi ~ age + wgt + hgt))))
