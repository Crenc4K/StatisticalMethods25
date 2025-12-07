# --- Topic: Discrete Distributions (Binomial & Poisson) ---

# Example 1: Binomial Distribution
n <- 10        # Number of trials
p <- 0.5       # Probability of success
x <- 4         # Target number of successes

prob_binom <- dbinom(x, size = n, prob = p)
cat("Probability of exactly 4 heads in 10 tosses:", prob_binom, "\n")


# Example 2: Poisson Distribution
lambda_per_min <- 3      # Average per minute
duration <- 5            # Duration in minutes
lambda_total <- lambda_per_min * duration  # Total lambda for 5 mins (15)

# Calculate P(X >= 10). We use 1 - P(X <= 9) because ppois is cumulative
prob_poisson <- 1 - ppois(9, lambda = lambda_total)
cat("Probability of at least 10 calls in 5 mins:", prob_poisson, "\n")