# Define the problem values
n <- 200                    # Number of tosses
X <- 110                    # Number of heads obtained
p_hat <- X / n              # Observed proportion of heads

# Confidence level and critical Z value for a 99% confidence interval (alpha = 0.01)
alpha <- 0.01
Z_alpha_2 <- qnorm(1 - alpha / 2)

# Calculate the standard error of the proportion
std_error <- sqrt((p_hat * (1 - p_hat)) / n)

# Calculate the confidence interval bounds
lower_bound <- p_hat - Z_alpha_2 * std_error
upper_bound <- p_hat + Z_alpha_2 * std_error

# Display the confidence interval
cat("99% Confidence Interval: [", lower_bound, ",", upper_bound, "]\n")

