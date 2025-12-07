# --- Topic: Normal Distribution Calculations ---

mu <- 60      # Mean
sigma <- 10   # Standard Deviation

# Question 1: P(X > 70) -> 1 - P(X <= 70)
prob_normal <- 1 - pnorm(70, mean = mu, sd = sigma)
cat("Probability of scoring higher than 70:", prob_normal, "\n")

# Question 2: Top 5% cutoff (95th percentile)
critical_grade <- qnorm(0.95, mean = mu, sd = sigma)
cat("Minimum grade for the top 5%:", critical_grade, "\n")

# Extra: Plotting the distribution
curve(dnorm(x, mean=mu, sd=sigma), from=30, to=90, 
      main="Grade Distribution (Normal)", col="blue", lwd=2)