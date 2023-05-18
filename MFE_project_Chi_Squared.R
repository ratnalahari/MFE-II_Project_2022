library("readxl")
df=read_xlsx("C:/Users/hp/Desktop/main data mfe.xlsx")
India_score <- df$Runs_India
India_score
df$Score_Range <- cut(df$Runs_India, breaks = seq(0, 450, by = 50), include.lowest = TRUE, right = FALSE)
table <- table(df$Score_Range, df$Runs_India)
exp_freq <- apply(table, 1, function(x) sum(x) * sum(x, na.rm = TRUE) / sum(table))
d_f <- (nrow(table) - 1) * (ncol(table) - 1)
crit_value <- qchisq(1 - alpha, d_f)
crit_value
chi_sq
if (chi_sq > crit_value) {
  cat("Reject the null hypothesis: there is a significant difference in the frequency of score ranges between the Indian cricket team and other teams.\n")
} else {
  cat("Fail to reject the null hypothesis: there is no significant difference in the frequency of score ranges between the Indian cricket team and other teams.\n")
}

India_score <- subset(df)
table(India_score$Score_Range)
library(ggplot2)
ggplot(df, aes(x = Runs_India)) +
  geom_histogram(binwidth = 25, fill = "darkblue", color = "black") +
  labs(title = "Distribution of Scores", x = "Score", y = "Frequency")