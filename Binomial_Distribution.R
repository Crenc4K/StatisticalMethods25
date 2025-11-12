calcBinomialDistribution <- function(n, x, p) {
  return (choose(n, x)*p^x * (1-p)^(n-x))
}

# example: 10 (n = 10) coin tosses (p = 1/2), exactly 4x heads (x = 4)
fourheads <- calcBinomialDistribution(10, 4, 0.5)
