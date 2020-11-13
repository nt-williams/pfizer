a <- 0.700102
b <- 1

theta <- function(ve) {
  (1 - ve) / (2 - ve)
}

ve <- function(theta) {
  (1 - 2*theta) / (1 - theta)
}

prior <- function(x, a = 0, b = 0) {
  dbeta(x, 0.700102 + a, 1 + b)
}

mu_beta <- function(x, a, b) {
  x * prior(x, a, b)
}

ci <- function(x, a, b) {
  qbeta(x, 0.700102 + a, 1 + b)
}

prior_theta <- integrate(mu_beta, 0, 1, a = 0, b = 0)$value

criterion <- function(a, b) {
  pbeta(prior_theta, 0.700102 + a, 1 + b, lower.tail = FALSE)
}

calc_statistics <- function(a = 0, b = 0) {
  theta <- integrate(mu_beta, 0, 1, a = a, b = b)$value
  ci <- rev(ve(ci(c(0.025, 0.975), a, b)))

  cat(sprintf("E[theta]: %.4f\n", theta))
  cat(sprintf("VE: %.4f\n", ve(theta)))
  cat(sprintf("P(VE < 30%%): %g\n", criterion(a, b)))
  cat(sprintf("VE 95%% Credible Interval: %.4f, %.4f", ci[1], ci[2]))

  post <- function(x) {
    dbeta(x, 0.700102 + a, 1 + b)
  }
  curve(post, n = 1000)
}
