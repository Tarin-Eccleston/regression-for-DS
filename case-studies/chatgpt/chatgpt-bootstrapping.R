# Load necessary libraries
library(tidyverse)

# Read in the data on tree presence at different altitudes
setwd("/Users/tarineccleston/Documents/Masters/STATS 762/regression-for-DS/case-studies/chatgpt")
data <- read.csv("data/kauri.csv")

# Define the number of bootstrap samples to generate
B <- 1000

# Define the desired confidence level for the intervals
alpha <- 0.05

# Fit a logistic regression model to the data
model <- glm(presence ~ altitude, data = data, family = binomial)

# Generate the bootstrap samples
samples <- matrix(0, nrow = B, ncol = nrow(data))
for (i in 1:B) {
  samples[i, ] <- sample(1:nrow(data), replace = TRUE)
}

# Estimate the probability of tree presence for each altitude and bootstrap sample
probs <- matrix(0, nrow = B, ncol = 3)
for (i in 1:B) {
  data_boot <- data[samples[i, ], ]
  model_boot <- glm(presence ~ altitude, data = data_boot, family = binomial)
  for (j in 1:3) {
    probs[i, j] <- predict(model_boot, newdata = data.frame(altitude = c(100, 300, 600)[j]), type = "response")
  }
}

# Calculate the mean and standard deviation of the probability estimates for each altitude
means <- apply(probs, 2, mean)
sds <- apply(probs, 2, sd)

# Compute the confidence intervals for the probability estimates for each altitude
ci <- matrix(0, nrow = 3, ncol = 2)
for (i in 1:3) {
  ci[i, ] <- c(quantile(probs[, i], alpha / 2), quantile(probs[, i], 1 - alpha / 2))
}

# Print the results
cat("Altitude 100 m:", "Mean:", round(means[1], 3), "CI:", round(ci[1, 1], 3), "-", round(ci[1, 2], 3), "\n")
cat("Altitude 300 m:", "Mean:", round(means[2], 3), "CI:", round(ci[2, 1], 3), "-", round(ci[2, 2], 3), "\n")
cat("Altitude 600 m:", "Mean:", round(means[3], 3), "CI:", round(ci[3, 1], 3), "-", round(ci[3, 2], 3), "\n")
