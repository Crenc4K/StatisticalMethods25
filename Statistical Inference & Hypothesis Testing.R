# --- Topic: Hypothesis Testing ---

# Example 1: One-Sample T-Test
# Sample data (simulated)
screw_diameters <- c(49.8, 50.1, 49.9, 50.2, 49.7, 50.0, 49.9, 50.3, 49.8, 50.1,
                     50.2, 49.9, 50.0, 49.8, 50.1, 50.0, 49.9, 50.2, 49.8, 50.1)

# H0: mu = 50 vs H1: mu != 50
t_test_result <- t.test(screw_diameters, mu = 50)
print(t_test_result)


# Example 2: Chi-Square Test of Independence
# Create the table: Rows (Smoking: Yes, No), Cols (Disease: Yes, No)
data_table <- matrix(c(30, 10,   # Smokers: 30 Sick, 10 Healthy
                       15, 45),  # Non-Smokers: 15 Sick, 45 Healthy
                     nrow = 2, byrow = TRUE)

colnames(data_table) <- c("Sick", "Healthy")
rownames(data_table) <- c("Smoker", "Non-Smoker")

print(data_table)

# Perform the test
chi_sq_result <- chisq.test(data_table)
print(chi_sq_result)