# Define the data vector
x <- c(2.2, 2.66, 2.74, 3.41, 2.46, 2.96, 3.34,
       2.16, 2.46, 2.71, 2.04, 3.74, 3.24, 3.92, 2.38,
       2.82, 2.2, 2.42, 2.82, 2.84, 4.22, 3.64, 1.77,
       3.44, 1.53)

# --- OPTION 1: Using the standard R function (Recommended) ---

# Perform a one-sample t-test against the null hypothesis mu = 2
test_result <- t.test(x, mu = 2)

# Display the full results
print(test_result)


# --- OPTION 2: Manual Calculation (Step-by-Step) ---

# 1. Calculate sample statistics
n <- length(x)                 # Sample size
sample_mean <- mean(x)         # Calculate sample mean
sample_sd <- sd(x)             # Calculate sample standard deviation
hypothesized_mean <- 2         # The value from H0

# 2. Calculate the t-statistic
# Formula: (mean - mu) / (sd / sqrt(n))
t_stat <- (sample_mean - hypothesized_mean) / (sample_sd / sqrt(n))

# 3. Find the critical t-value 
# Using 0.975 because alpha is 0.05 and it is a two-tailed test
critical_value <- qt(0.975, df = n - 1)

# 4. Print values for comparison
cat("T-Statistic:", t_stat, "\n")
cat("Critical Value:", critical_value, "\n")

# 5. Logical check for rejection
if (abs(t_stat) > critical_value) {
  print("Reject the Null Hypothesis (Mean is not 2)")
} else {
  print("Fail to reject the Null Hypothesis")
}