#PROBABILITY

#use choose(n, k) in R to get combinations
#n items, how many ways to select a group of k items

#Conditional probability (conditional probability of A given B)
#P(probability A knowing B)
#P(A | B) = P(A ∩ B) / P(B)

#A and B are independent if
#P(A | B) = P(A) OR P(A ∩ B) = P(A) * P(B)

#Theorem of Total Probability
#If we have P(A'), P(A), P(B|A), P(B|A')
#P(B) = P(A)P(B|A) + P(A')P(B|A')  (if there are more Ai + as much Ai variants we have)

#Bayes theorem
#P(Ai|B) = P(Ai ∩ B)/P(B)
# = P(Ai)P(B|Ai)/sum from i=1 to n of (P(Ai)P(B|Ai))

#P(A|B) = P(A)P(B|A) / P(A)P(B|A) + P(A')P(B|A')

#P(A'|B) = P(A')P(B|A') / P(A)P(B|A) + P(A')P(B|A')

#APPROXIMATION
#The normal distribution with mean np and standard deviation √npq is
#used as an approximation to the binomial distribution with parameters n
#and p when n is large and np ≥ 5 and nq ≥ 5.
#The Poisson distribution, P(λ) can be aproximated to a Normal
#distribution when λ > 10, that is, N(µ = λ, σ =√λ).
#To correctly approximate a discrete random variable X with a binomial
#distribution using a continuous random variable Y with a normal
#distribution, a continuity correction is necessary, such that:
#P(X = a) = P(a − 0.5 ≤ Y ≤ a + 0.5),
#P(a < X < b) = P(a + 0.5 ≤ Y ≤ b − 0.5),
#P(a ≤ X ≤ b) = P(a − 0.5 ≤ Y ≤ b + 0.5),
#P(a < X ≤ b) = P(a + 0.5 ≤ Y ≤ b + 0.5),
#P(a ≤ X < b) = P(a − 0.5 ≤ Y ≤ b − 0.5).

explain_functions <- function() {
  cat("--------------------------------------------------\n")
  cat("📘 OVERVIEW OF FUNCTIONS\n")
  cat("--------------------------------------------------\n")
  cat("  k = specific number (discrete), q = cutoff (continuous)\n")
  cat("  p = probability, n = sample size\n\n")
  
  cat("1. Discrete Uniform (Rolling a die):\n")
  cat("   uniform_probEqual(k, min, max)           P(X = k)\n")
  cat("   uniform_probLessThan(q, min, max)        P(X <= q)\n")
  cat("   uniform_probMoreThanEqual(k, min, max)   P(X >= k)\n")
  cat("   uniform_probBetween(low, high, ...)\n\n")
  
  cat("2. Binomial (Coin flips):\n")
  cat("   binom_probEqual(k, n, p)                 P(X = k)\n")
  cat("   binom_probLessThan(q, n, p)              P(X <= q)\n")
  cat("   binom_probMoreThanEqual(k, n, p)         P(X >= k)\n")
  cat("   binom_inverse(p, n, p)\n\n")
  
  cat("3. Geometric (Tries until success):\n")
  cat("   geom_probEqual(k, p)                     P(X = k)\n")
  cat("   geom_probLessThan(q, p)                  P(X <= q)\n")
  cat("   geom_probMoreThanEqual(k, p)             P(X >= k)\n\n")
  
  cat("4. Poisson (Events over time):\n")
  cat("   pois_probEqual(k, lambda)                P(X = k)\n")
  cat("   pois_probLessThan(q, lambda)             P(X <= q)\n")
  cat("   pois_probMoreThanEqual(k, lambda)        P(X >= k)\n\n")
  
  cat("5. Normal Distribution:\n")
  cat("   norm_density(x, mu, sigma)\n")
  cat("   norm_probLessThan(q, mu, sigma)          P(X <= q)\n")
  cat("   norm_probMoreThanEqual(q, mu, sigma)     P(X >= q)\n")
  cat("   norm_inverse(p, mu, sigma)\n\n")
  
  cat("6. Student's t Distribution:\n")
  cat("   t_probLessThan(q, df)                    P(X <= q)\n")
  cat("   t_probMoreThanEqual(q, df)               P(X >= q)\n")
  cat("   t_inverse(p, df)\n\n")
  
  cat("7. Chi-Square Distribution:\n")
  cat("   chisq_probLessThan(q, df)                P(X <= q)\n")
  cat("   chisq_probMoreThanEqual(q, df)           P(X >= q)\n")
  cat("   chisq_inverse(p, df)\n\n")
  
  cat("8. Hypothesis Testing:\n")
  cat("   chi_square_independence(table)\n")
}


############################################################
# 1. DISCRETE UNIFORM U(a,b)
############################################################

uniform_probEqual <- function(k, min, max) {
  ifelse(k >= min & k <= max & k %% 1 == 0, 1 / (max - min + 1), 0) # P(X = k)
}

uniform_probLessThan <- function(q, min, max) {
  ifelse(q < min, 0, ifelse(q >= max, 1, (floor(q) - min + 1) / (max - min + 1))) # P(X <= q)
}

uniform_probMoreThanEqual <- function(k, min, max) {
  # Logic: Total (1) minus probability of being less than k-1
  1 - uniform_probLessThan(k - 1, min, max) # P(X >= k)
}

uniform_probBetween <- function(low, high, min, max) {
  uniform_probLessThan(high, min, max) - uniform_probLessThan(low - 1, min, max) # P(low <= X <= high)
}

uniform_rnd <- function(n, min, max) {
  as.integer(sample(min:max, size = n, replace = TRUE))
}


############################################################
# 2. BERNOULLI
############################################################

bernoulli_probEqual <- function(k, p) {
  ifelse(k == 1, p, ifelse(k == 0, 1 - p, 0)) # P(X = k)
}

bernoulli_probLessThan <- function(q, p) {
  ifelse(q < 0, 0, ifelse(q < 1, 1 - p, 1)) # P(X <= q)
}

bernoulli_probMoreThanEqual <- function(k, p) {
  1 - bernoulli_probLessThan(k - 1, p) # P(X >= k)
}

bernoulli_rnd <- function(n, p) {
  rbinom(n, 1, p)
}


############################################################
# 3. BINOMIAL
############################################################

binom_probEqual <- function(k, n, p) {
  dbinom(k, n, p) # P(X = k)
}

binom_probLessThan <- function(q, n, p) {
  pbinom(q, n, p) # P(X <= q)
}

binom_probMoreThanEqual <- function(k, n, p) {
  # Note: pbinom with lower.tail=FALSE gives > (strictly greater). 
  # To get >= k, we calculate > (k-1).
  pbinom(k - 1, n, p, lower.tail = FALSE) # P(X >= k)
}

binom_probBetween <- function(low, high, n, p) {
  pbinom(high, n, p) - pbinom(low - 1, n, p) # P(low <= X <= high)
}

binom_inverse <- function(prob, n, p) {
  qbinom(prob, n, p) # P(X <= x) = prob
}

binom_rnd <- function(n_samples, n, p) {
  rbinom(n_samples, n, p)
}


############################################################
# 4. GEOMETRIC
############################################################

geom_probEqual <- function(k, p) {
  dgeom(k, p) # P(X = k)
}

geom_probLessThan <- function(q, p) {
  pgeom(q, p) # P(X <= q)
}

geom_probMoreThanEqual <- function(k, p) {
  pgeom(k - 1, p, lower.tail = FALSE) # P(X >= k)
}

geom_probBetween <- function(low, high, p) {
  pgeom(high, p) - pgeom(low - 1, p) # P(low <= X <= high)
}

geom_inverse <- function(prob, p) {
  qgeom(prob, p) # P(X <= x) = prob
}

geom_rnd <- function(n, p) {
  rgeom(n, p)
}


############################################################
# 5. POISSON
############################################################

pois_probEqual <- function(k, lambda) {
  dpois(k, lambda) # P(X = k)
}

pois_probLessThan <- function(q, lambda) {
  ppois(q, lambda) # P(X <= q)
}

pois_probMoreThanEqual <- function(k, lambda) {
  ppois(k - 1, lambda, lower.tail = FALSE) # P(X >= k)
}

pois_probBetween <- function(low, high, lambda) {
  ppois(high, lambda) - ppois(low - 1, lambda) # P(low <= X <= high)
}

pois_inverse <- function(prob, lambda) {
  qpois(prob, lambda) # P(X <= x) = prob
}

pois_rnd <- function(n, lambda) {
  rpois(n, lambda)
}


############################################################
# 6. NORMAL DISTRIBUTION
############################################################

norm_density <- function(x, mu, sigma) {
  dnorm(x, mu, sigma) # f(x) (Height)
}

norm_probLessThan <- function(q, mu, sigma) {
  pnorm(q, mu, sigma) # P(X <= q)
}

norm_probMoreThanEqual <- function(q, mu, sigma) {
  # For continuous, P(X >= q) is the same as P(X > q)
  pnorm(q, mu, sigma, lower.tail = FALSE) # P(X >= q)
}

norm_probBetween <- function(low, high, mu, sigma) {
  pnorm(high, mu, sigma) - pnorm(low, mu, sigma) # P(low <= X <= high)
}

norm_inverse <- function(prob, mu, sigma) {
  qnorm(prob, mu, sigma) # P(X <= x) = prob
}

norm_rnd <- function(n, mu, sigma) {
  rnorm(n, mu, sigma)
}


############################################################
# 7. STUDENT'S T DISTRIBUTION
############################################################

t_density <- function(x, df) {
  dt(x, df) # f(x)
}

t_probLessThan <- function(q, df) {
  pt(q, df) # P(X <= q)
}

t_probMoreThanEqual <- function(q, df) {
  pt(q, df, lower.tail = FALSE) # P(X >= q)
}

t_probBetween <- function(low, high, df) {
  pt(high, df) - pt(low, df) # P(low <= X <= high)
}

t_inverse <- function(prob, df) {
  qt(prob, df) # P(X <= x) = prob
}

t_rnd <- function(n, df) {
  rt(n, df)
}


############################################################
# 8. CHI-SQUARE DISTRIBUTION
############################################################

chisq_density <- function(x, df) {
  dchisq(x, df) # f(x)
}

chisq_probLessThan <- function(q, df) {
  pchisq(q, df) # P(X <= q)
}

chisq_probMoreThanEqual <- function(q, df) {
  pchisq(q, df, lower.tail = FALSE) # P(X >= q)
}

chisq_probBetween <- function(low, high, df) {
  pchisq(high, df) - pchisq(low, df) # P(low <= X <= high)
}

chisq_inverse <- function(prob, df) {
  qchisq(prob, df) # P(X <= x) = prob
}

chisq_rnd <- function(n, df) {
  rchisq(n, df)
}
############################################################
# DISTRIBUTION FITTING (Goodness of Fit) intervals
#breaks <- seq(0, 100, by=10)
fit_normal_distribution <- function(midscores, observed, breaks, confidence = 0.95) {
  
  # 1. Basic Setup
  n <- sum(observed)
  k <- length(observed)
  
  # 2. Estimate Parameters (Weighted Mean and SD)
  mu_hat <- sum(observed * midscores) / n
  sigma_hat <- sqrt(sum(observed * (midscores - mu_hat)^2) / n)
  
  # 3. Calculate Theoretical Probabilities (Area under the curve)
  p <- numeric(k)
  for(i in 1:k){
    # Area between break[i] and break[i+1]
    p[i] <- pnorm(breaks[i+1], mean = mu_hat, sd = sigma_hat) - 
      pnorm(breaks[i],   mean = mu_hat, sd = sigma_hat)
  }
  
  # 4. Normalize Probabilities (Ensure they sum to exactly 1)
  #    This handles the tiny tails of the distribution outside the bins
  p <- p * (1 / sum(p))
  
  # 5. Calculate Chi-Square Statistic
  #    Formula: Sum( (Observed - Expected)^2 / Expected )
  expected <- n * p
  chi_stat <- sum((observed - expected)^2 / expected)
  
  # 6. Degrees of Freedom
  #    k (bins) - 1 (sum constraint) - 2 (estimated parameters: mu, sigma)
  df_adj <- k - 3
  
  # 7. Critical Value and P-Value
  chi_crit <- qchisq(confidence, df = df_adj)
  p_value  <- pchisq(chi_stat, df = df_adj, lower.tail = FALSE)
  
  # 8. Conclusion
  if (chi_stat < chi_crit) {
    conclusion <- "ACCEPT Null (H0): Data follows a Normal Distribution"
  } else {
    conclusion <- "REJECT Null (H0): Data does NOT follow a Normal Distribution"
  }
  
  # 9. Return Results List
  list(
    estimated_mean = mu_hat,
    estimated_sd   = sigma_hat,
    observed       = observed,
    expected       = round(expected, 2),
    chi_statistic  = chi_stat,
    degrees_of_freedom = df_adj,
    critical_value = chi_crit,
    p_value        = p_value,
    conclusion     = conclusion
  )
}
############################################################
# DISTRIBUTION FITTING (Exact/Discrete Scores)

fit_normal_exact_scores <- function(scores, observed, confidence = 0.95) {
  
  # 1. Basic Setup
  n <- sum(observed)
  k <- length(observed)
  
  # 2. Estimate Parameters (Weighted Mean and SD)
  mu_hat <- sum(observed * scores) / n
  sigma_hat <- sqrt(sum(observed * (scores - mu_hat)^2) / n)
  
  # 3. Define "Virtual" Breaks (Continuity Correction)
  #    We treat a score of X as the interval [X - 0.5, X + 0.5]
  lower_bounds <- scores - 0.5
  upper_bounds <- scores + 0.5
  
  # 4. Calculate Probabilities
  p <- numeric(k)
  for(i in 1:k){
    p[i] <- pnorm(upper_bounds[i], mean = mu_hat, sd = sigma_hat) - 
      pnorm(lower_bounds[i], mean = mu_hat, sd = sigma_hat)
  }
  
  # 5. Normalize Probabilities
  #    Ensures they sum to 1 (accounts for the tails of the curve)
  p <- p * (1 / sum(p))
  
  # 6. Chi-Square Statistic
  expected <- n * p
  chi_stat <- sum((observed - expected)^2 / expected)
  
  # 7. Degrees of Freedom
  #    k (categories) - 1 (sum constraint) - 2 (estimated parameters)
  df_adj <- k - 3
  
  # Safety check for low DF
  if (df_adj <= 0) {
    warning("Degrees of freedom is <= 0. Not enough data points to fit Normal distribution parameters.")
    return(NULL)
  }
  
  # 8. Critical Value and Conclusion
  chi_crit <- qchisq(confidence, df = df_adj)
  p_value  <- pchisq(chi_stat, df = df_adj, lower.tail = FALSE)
  
  if (chi_stat < chi_crit) {
    conclusion <- "ACCEPT Null (H0): Data fits a Normal Distribution"
  } else {
    conclusion <- "REJECT Null (H0): Data does NOT fit a Normal Distribution"
  }
  
  # 9. Return Results
  list(
    estimated_mean = mu_hat,
    estimated_sd   = sigma_hat,
    observed       = observed,
    expected       = round(expected, 2),
    chi_statistic  = chi_stat,
    degrees_of_freedom = df_adj,
    critical_value = chi_crit,
    p_value        = p_value,
    conclusion     = conclusion
  )
}

fit_poisson_distribution <- function(counts, observed, confidence = 0.95) {
  
  # 1. Setup
  n <- sum(observed)
  k <- length(observed)
  
  # 2. Estimate Parameter (Lambda is just the weighted mean)
  lambda_hat <- sum(observed * counts) / n
  
  # 3. Calculate Probabilities (Using dpois for exact integers)
  p <- dpois(counts, lambda = lambda_hat)
  
  # FIX: The last category usually implies "X >= k", not just "X = k"
  # Theoretically, probabilities must sum to 1.
  # We force the sum to 1 to account for the infinite tail of Poisson.
  p <- p / sum(p)
  
  # 4. Expected Counts
  expected <- n * p
  
  # 5. Chi-Square Test
  chi_stat <- sum((observed - expected)^2 / expected)
  
  # 6. Degrees of Freedom (k - 1 - 1 parameter)
  df_adj <- k - 2
  
  # 7. Conclusion
  chi_crit <- qchisq(confidence, df = df_adj)
  
  if (chi_stat < chi_crit) {
    conclusion <- "ACCEPT Null: Data fits Poisson"
  } else {
    conclusion <- "REJECT Null: Data does NOT fit Poisson"
  }
  
  list(
    lambda = lambda_hat,
    chi_stat = chi_stat,
    df = df_adj,
    conclusion = conclusion
  )
}


############################################################
# 9. CHI-SQUARE INDEPENDENCE TEST
############################################################

chi_square_independence <- function(obs_table, alpha = 0.05) {
  
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) stop("alpha must be between 0 and 1")
  
  test <- suppressWarnings(chisq.test(obs_table, correct = FALSE))
  pval <- test$p.value
  
  if (pval < alpha) {
    conclusion <- "REJECT Null: Variables are Dependent (Related)"
  } else {
    conclusion <- "ACCEPT Null: Variables are Independent (Not Related)"
  }
  
  list(
    observed   = obs_table,
    expected   = test$expected,
    chi_square = test$statistic,
    p_value    = pval,
    conclusion = conclusion
  )
} 
#when doing by hand if in the TABLES.pdf the equation for hypothesis satisfies, then we reject h0

# t.test: alternative can be “two.sided” H1 !=, “less” H1 <, “greater” H1 >
#result <- t.test(a, b, alternative = "less", var.equal = TRUE, conf.level = 0.95)
#t.test(x,mu=2,alternative="two.sided",conf.level=0.95)
#might need library(epitools)


