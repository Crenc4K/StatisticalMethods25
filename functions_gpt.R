############################################################
# COMPLETE SET OF DISTRIBUTION + STATISTICS FUNCTIONS
# With explanations for each function.
############################################################


#############################
# 0. Helper: Explain all functions
#############################

explain_functions <- function() {
  cat("--------------------------------------------------\n")
  cat("📘 OVERVIEW OF FUNCTIONS\n")
  cat("--------------------------------------------------\n\n")
  
  cat("Uniform Discrete Distribution U(a,b):\n")
  cat("  discrete_uniform_pmf(k,a,b)   -> Probability X = k\n")
  cat("  discrete_uniform_cdf(q,a,b)   -> Probability X <= q\n")
  cat("  discrete_uniform_rnd(n,a,b)   -> Random values\n")
  cat("  discrete_uniform_moments(a,b) -> Mean & variance\n\n")
  
  cat("Bernoulli Distribution:\n")
  cat("  bernoulli_pmf(k,p)\n")
  cat("  bernoulli_cdf(q,p)\n")
  cat("  bernoulli_rnd(n,p)\n")
  cat("  bernoulli_moments(p)\n\n")
  
  cat("Binomial Distribution:\n")
  cat("  binom_pmf(k,n,p)\n")
  cat("  binom_cdf(q,n,p)\n")
  cat("  binom_rnd(n_samples,n,p)\n")
  cat("  binom_moments(n,p)\n\n")
  
  cat("Geometric Distribution:\n")
  cat("  geom_pmf(k,p)\n")
  cat("  geom_cdf(q,p)\n")
  cat("  geom_rnd(n,p)\n")
  cat("  geom_moments(p)\n\n")
  
  cat("Poisson Distribution:\n")
  cat("  pois_pmf(k,lambda)\n")
  cat("  pois_cdf(q,lambda)\n")
  cat("  pois_rnd(n,lambda)\n")
  cat("  pois_moments(lambda)\n\n")
  
  cat("Continuous Uniform Distribution:\n")
  cat("  unif_cont_pdf(x,a,b)\n")
  cat("  unif_cont_cdf(q,a,b)\n")
  cat("  unif_cont_rnd(n,a,b)\n")
  cat("  unif_cont_moments(a,b)\n\n")
  
  cat("Exponential Distribution:\n")
  cat("  exp_pdf(x,lambda)\n")
  cat("  exp_cdf(q,lambda)\n")
  cat("  exp_rnd(n,lambda)\n")
  cat("  exp_moments(lambda)\n\n")
  
  cat("Normal Distribution:\n")
  cat("  norm_pdf(x,mu,sigma)\n")
  cat("  norm_cdf(q,mu,sigma)\n")
  cat("  norm_rnd(n,mu,sigma)\n")
  cat("  norm_moments(mu,sigma)\n\n")
  
  cat("Chi-square Independence Test:\n")
  cat("  chi_square_independence(obs_table, alpha = 0.05)\n")
  cat("    -> Tests independence between two categorical variables\n")
  cat("       alpha can be any value in (0,1), e.g. 0.01, 0.05, 0.10\n\n")
  
  cat("--------------------------------------------------\n")
  cat("End of list\n")
  cat("--------------------------------------------------\n")
}



############################################################
# 1. DISCRETE UNIFORM U(a,b)
############################################################

discrete_uniform_pmf <- function(k, a, b) {
  k <- as.numeric(k)
  if (b < a) stop("'b' must be >= 'a'")
  ok <- (k %% 1 == 0) & (k >= a) & (k <= b)
  out <- numeric(length(k))
  out[ok] <- 1 / (b - a + 1)
  out
}

discrete_uniform_cdf <- function(q, a, b) {
  q <- as.numeric(q)
  if (b < a) stop("'b' must be >= 'a'")
  Fq <- numeric(length(q))
  Fq[q < a] <- 0
  Fq[q >= b] <- 1
  inside <- (q >= a) & (q < b)
  Fq[inside] <- (floor(q[inside]) - a + 1) / (b - a + 1)
  Fq
}

discrete_uniform_rnd <- function(n, a, b) {
  if (b < a) stop("'b' must be >= 'a'")
  as.integer(sample(a:b, size = n, replace = TRUE))
}

discrete_uniform_moments <- function(a, b) {
  list(
    mean = (a + b) / 2,
    variance = ((b - a + 1)^2 - 1) / 12
  )
}



############################################################
# 2. BERNOULLI
############################################################

bernoulli_pmf <- function(k, p) ifelse(k == 1, p, ifelse(k == 0, 1 - p, 0))
bernoulli_cdf <- function(q, p) ifelse(q < 0, 0, ifelse(q < 1, 1 - p, 1))
bernoulli_rnd <- function(n, p) rbinom(n, 1, p)
bernoulli_moments <- function(p) list(mean = p, variance = p*(1-p))



############################################################
# 3. BINOMIAL
############################################################

binom_pmf <- function(k, n, p) dbinom(k, n, p)
binom_cdf <- function(q, n, p) pbinom(q, n, p)
binom_rnd <- function(n_samples, n, p) rbinom(n_samples, n, p)
binom_moments <- function(n, p) list(mean = n*p, variance = n*p*(1-p))



############################################################
# 4. GEOMETRIC
############################################################

geom_pmf <- function(k, p) dgeom(k, p)
geom_cdf <- function(q, p) pgeom(q, p)
geom_rnd <- function(n, p) rgeom(n, p)
geom_moments <- function(p) list(mean = (1-p)/p, variance = (1-p)/p^2)



############################################################
# 5. POISSON
############################################################

pois_pmf <- function(k, lambda) dpois(k, lambda)
pois_cdf <- function(q, lambda) ppois(q, lambda)
pois_rnd <- function(n, lambda) rpois(n, lambda)
pois_moments <- function(lambda) list(mean = lambda, variance = lambda)



############################################################
# 6. CONTINUOUS UNIFORM
############################################################

unif_cont_pdf <- function(x, a, b) dunif(x, a, b)
unif_cont_cdf <- function(q, a, b) punif(q, a, b)
unif_cont_rnd <- function(n, a, b) runif(n, a, b)
unif_cont_moments <- function(a, b) list(
  mean = (a + b)/2,
  variance = (b - a)^2 / 12
)



############################################################
# 7. EXPONENTIAL
############################################################

exp_pdf <- function(x, lambda) dexp(x, lambda)
exp_cdf <- function(q, lambda) pexp(q, lambda)
exp_rnd <- function(n, lambda) rexp(n, lambda)
exp_moments <- function(lambda) list(mean = 1/lambda, variance = 1/lambda^2)



############################################################
# 8. NORMAL
############################################################

norm_pdf <- function(x, mu, sigma) dnorm(x, mu, sigma)
norm_cdf <- function(q, mu, sigma) pnorm(q, mu, sigma)
norm_rnd <- function(n, mu, sigma) rnorm(n, mu, sigma)
norm_moments <- function(mu, sigma) list(mean = mu, variance = sigma^2)



############################################################
# 9. CHI-SQUARE INDEPENDENCE TEST (UNIVERSAL, ADJUSTABLE α)
############################################################

chi_square_independence <- function(obs_table, alpha = 0.05) {
  
  # Validate alpha
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a number between 0 and 1.")
  }
  
  # Run test without Yates correction (recommended for >2x2)
  test <- suppressWarnings(chisq.test(obs_table, correct = FALSE))
  
  chi2 <- test$statistic
  df   <- test$parameter
  pval <- test$p.value
  
  # Interpretation
  if (pval < alpha) {
    conclusion <- paste0(
      "❌ Reject H₀ at α = ", alpha,
      ": Evidence of dependence (interaction exists)."
    )
  } else {
    conclusion <- paste0(
      "✔ Do NOT reject H₀ at α = ", alpha,
      ": No evidence of interaction (variables appear independent)."
    )
  }
  
  list(
    observed   = obs_table,
    expected   = test$expected,
    chi_square = chi2,
    df         = df,
    p_value    = pval,
    alpha      = alpha,
    conclusion = conclusion
  )
}
