library("readxl")
df <- read_xlsx("C:/Users/hp/Desktop/MFE 2/main data mfe.xlsx")
df$Ave_india <- as.numeric(gsub("[^0-9.]+", "", df$Ave_india))
df$Ave_india

india_runs_per_over <- df$Ave_india
india_runs_per_over <- india_runs_per_over[!is.na(india_runs_per_over)]

n <- length(india_runs_per_over)
xbar <- mean(india_runs_per_over)
s <- sd(india_runs_per_over)

# Calculate the overall mean of the Ave_india column
hypothesized_mean <- mean(india_runs_per_over)

t.test(india_runs_per_over, mu = hypothesized_mean, alternative = "two.sided")

t.test_result <- t.test(india_runs_per_over, mu = hypothesized_mean, alternative = "two.sided")

# Extract the p-value from the output of the t-test
p_value <- t.test_result$p.value

# Set your significance level (alpha)
alpha <- 0.05

# Make a conclusion based on the p-value and the significance level
if (p_value < alpha) {
  cat("Reject the null hypothesis. There is evidence that the true mean is different from", hypothesized_mean, "\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to suggest that the true mean is different from", hypothesized_mean, "\n")
}