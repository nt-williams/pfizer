My take on the Pfizer results
================

Inspired by others and realizing this a good chance to learn a bit about
Bayesian statistics, here’s my take on the Pfizer calculations.

``` r
a <- 0.700102
b <- 1

theta <- function(ve) {
  (1 - ve) / (2 - ve)
}

ve <- function(theta) {
  (1 - 2*theta) / (1 - theta)
}

mu_beta <- function(x, a, b) {
  x * prior(x, a, b)
}

prior <- function(x, a = 0, b = 0) {
  dbeta(x, 0.700102 + a, 1 + b)
}

ci <- function(x, a, b) {
  qbeta(x, 0.700102 + a, 1 + b)
}

criterion <- function(a, b) {
  pbeta(theta(0.3), 0.700102 + a, 1 + b, lower.tail = FALSE)
}

calc_statistics <- function(a = 0, b = 0) {
  theta <- integrate(mu_beta, 0, 1, a = a, b = b)$value
  ci <- rev(ve(ci(c(0.025, 0.975), a, b)))

  cat(sprintf("E[theta]: %.4f\n", theta))
  cat(sprintf("VE: %.4f\n", ve(theta)))
  cat(sprintf("P(VE < 30%%): %g\n", criterion(a, b)))
  cat(sprintf("VE 95%% Credible Interval: %.4f, %.4f", ci[1], ci[2]))

  theta_dist <- function(x) {
    dbeta(x, 0.700102 + a, 1 + b)
  }
  
  curve(theta_dist, n = 1000, xlab = expression(theta), ylab = "f(x)")
}
```

The prior:

``` r
calc_statistics()
```

    ## E[theta]: 0.4118
    ## VE: 0.2999
    ## P(VE < 30%): 0.462702
    ## VE 95% Credible Interval: -26.1556, 0.9948

![](pfizer-ve_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Assuming an 8 and 86 case split between the arms:

``` r
calc_statistics(8, 86)
```

    ## E[theta]: 0.0909
    ## VE: 0.9000
    ## P(VE < 30%): 6.42927e-13
    ## VE 95% Credible Interval: 0.8156, 0.9559

![](pfizer-ve_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
