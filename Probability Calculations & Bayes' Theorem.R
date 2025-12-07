# --- Topic: Probability & Bayes' Theorem ---

# Given Probabilities
p_sick <- 0.01                  # Prevalence (Prior)
p_healthy <- 1 - p_sick         # Probability of being healthy
p_pos_given_sick <- 0.99        # Sensitivity
p_pos_given_healthy <- 0.05     # False Positive Rate

# Total Probability of testing Positive
p_positive <- (p_sick * p_pos_given_sick) + (p_healthy * p_pos_given_healthy)

# Bayes' Theorem: P(Sick | Positive)
p_sick_given_pos <- (p_sick * p_pos_given_sick) / p_positive

cat("Probability of being sick given a positive test:", p_sick_given_pos, "\n")