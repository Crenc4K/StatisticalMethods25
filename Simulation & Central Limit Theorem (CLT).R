# --- Topic: Central Limit Theorem Simulation ---

set.seed(123)       # For reproducibility
num_samples <- 1000
n <- 30             # Size of each sample
sample_means <- numeric(num_samples) # Empty vector to store means

# Simulation Loop
for (i in 1:num_samples) {
  # Draw 30 random numbers from Uniform Dist (0 to 100)
  sample_data <- runif(n, min = 0, max = 100) 
  sample_means[i] <- mean(sample_data)
}

# Plot Histogram
hist(sample_means, 
     breaks = 30, 
     col = "lightblue", 
     main = "Central Limit Theorem: Distribution of Sample Means",
     xlab = "Sample Means")

# Overlay Normal Curve (for visual check)
curve(dnorm(x, mean=mean(sample_means), sd=sd(sample_means)) * length(sample_means) * diff(hist(sample_means, plot=FALSE)$breaks)[1], 
      add=TRUE, col="red", lwd=2)
