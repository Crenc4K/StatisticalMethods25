# For Neighborhood A:
pA <- 43 / 100          # Sample proportion (p hat)
nA <- 500               # Sample size (n)
# Margin of error calculation (using qnorm(.975) for 95% confidence level)
e <- qnorm(.975) * sqrt(pA * (1 - pA) / nA) 
c(pA - e, pA + e)       # Print the confidence interval (lower and upper bound)
# [1] 0.3866055 0.4733945

# For Neighborhood B:
pB <- 42 / 100          # Sample proportion
nB <- 300               # Sample size
e <- qnorm(.975) * sqrt(pB * (1 - pB) / nB)
c(pB - e, pB + e)       # Print the confidence interval
# [1] 0.3641496 0.4758504
