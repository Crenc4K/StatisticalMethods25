# Data 
x <- c(2.2, 2.66, 2.74, 3.41, 2.46, 2.96, 3.34,
       2.16, 2.46, 2.71, 2.04, 3.74, 3.24, 3.92, 2.38,
       2.82, 2.2, 2.42, 2.82, 2.84, 4.22, 3.64, 1.77,
       3.44, 1.53)

# Calculation
n <- 25                      # Sample size
# Critical t-value for 95% confidence (degrees of freedom n-1)
qt(.975, n - 1)              

sqrt(var(x))                 # Sample standard deviation
mean(x)                      # Sample mean

# Lower Bound Calculation
mean(x) - qt(.975, n - 1) * sqrt(var(x)) / sqrt(n)
# [1] 2.523844

# Upper Bound Calculation
mean(x) + qt(.975, n - 1) * sqrt(var(x)) / sqrt(n)
# [1] 3.085756