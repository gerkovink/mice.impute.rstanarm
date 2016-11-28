.stan.draw <- function(y, ry, x, ridge = 1e-05, ...){
  noprint <- capture.output(fit <- stan_glm(y ~ x[,-1], algorithm = "optimizing"))
  sigma.star <- sqrt(sum((residuals(fit))^2)/rchisq(1, fit$df.residual))
  parm <- list(coef(fit), sigma.star)
  names(parm) <- c("beta", "sigma")
  return(parm)
}

mice.impute.rstanarm <- function(y, ry, x, ...){
  x <- cbind(1, as.matrix(x))
  parm <- .stan.draw(y, ry, x)
  return(x[!ry, ] %*% parm$beta + rnorm(sum(!ry)) * parm$sigma)
}