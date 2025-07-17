data = read.csv("C:\\Users\\admin\\Desktop\\Gargi study\\TYBsc project\\final project\\For final project dataset.csv", header = TRUE)

# Load necessary libraries
library(dplyr)
library(tidyr)

# Define variable types
numeric_vars <- c("ScrabbleScore", "CV_Ratio", 
                  "UniqueLetters", "Positional_Score")
categorical_vars <- c("Bigram", "Trigram")

# 1️⃣ Kruskal-Wallis Tests: numeric ~ categorical
cat("=== Kruskal-Wallis Tests (Numeric ~ Categorical) ===\n")
for (num_var in numeric_vars) {
  for (cat_var in categorical_vars) {
    test_result <- kruskal.test(as.formula(paste(num_var, "~ factor(", cat_var, ")")), data = data)
    cat(sprintf("Kruskal-Wallis: %s ~ %s\n  chi-sq = %.4f, df = %d, p = %.5f\n\n",
                num_var, cat_var,
                test_result$statistic,
                test_result$parameter,
                test_result$p.value))
  }
}

# 2️⃣ Spearman Correlation: numeric vs numeric
cat("=== Spearman Correlation (Numeric ~ Numeric) ===\n")
for (i in 1:(length(numeric_vars) - 1)) {
  for (j in (i+1):length(numeric_vars)) {
    var1 <- numeric_vars[i]
    var2 <- numeric_vars[j]
    test_result <- cor.test(data[[var1]], data[[var2]], method = "spearman", exact = FALSE)
    cat(sprintf("Spearman: %s ~ %s\n  rho = %.4f, p = %.5f\n\n",
                var1, var2, test_result$estimate, test_result$p.value))
  }
}

# 3️⃣ Chi-squared Test: categorical ~ categorical
cat("=== Chi-Squared Tests (Categorical ~ Categorical) ===\n")
for (i in 1:(length(categorical_vars) - 1)) {
  for (j in (i+1):length(categorical_vars)) {
    var1 <- categorical_vars[i]
    var2 <- categorical_vars[j]
    tbl <- table(data[[var1]], data[[var2]])
    test_result <- chisq.test(tbl)
    cat(sprintf("Chi-sq Test: %s ~ %s\n  chi-sq = %.4f, df = %d, p = %.5f\n\n",
                var1, var2,
                test_result$statistic,
                test_result$parameter,
                test_result$p.value))
  }
}

# 📦 Install required packages if not already installed
install.packages("BSDA")
if (!require("tseries")) install.packages("tseries")

library(BSDA)
library(tseries)

cat("=== 1️⃣ SIGN TEST ===\n")

# One-sample sign test: Is Average.guess median ≠ 4?
sign_test1 <- SIGN.test(data$Average.guess, md = 4)
cat("Sign Test: Average.guess vs median = 4\n")
print(sign_test1)
cat("\n")

# Paired sign test: ScrabbleScore vs Positional_Score
sign_test2 <- SIGN.test(data$ScrabbleScore, y = data$Positional_Score, paired = TRUE)
cat("Sign Test: Paired ScrabbleScore vs Positional_Score\n")
print(sign_test2)
cat("\n")


cat("=== 2️⃣ RUNS TEST ===\n")

# Runs test for binary randomness in Has_HighFreq_Bigram
cat("Runs Test: Randomness in Has_HighFreq_Bigram (0/1)\n")
print(runs.test(factor(data$Has_HighFreq_Bigram)))
cat("\n")

# Runs test for binary randomness in Has_HighFreq_Trigram
cat("Runs Test: Randomness in Has_HighFreq_Trigram (0/1)\n")
print(runs.test(factor(data$Has_HighFreq_Trigram)))
cat("\n")

# Runs test on directional change in Average.guess
# Convert to signs (1 = increase, 0 = decrease/same)
guess_diff <- diff(data$Average.guess)
guess_signs <- ifelse(guess_diff > 0, 1, 0)
cat("Runs Test: Direction of change in Average.guess (increasing/decreasing)\n")
print(runs.test(as.factor(guess_signs)))
